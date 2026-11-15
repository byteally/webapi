{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | Regression cases (zenbridge #2713): each test a DL property plays,
-- written out the way "Test.WebApi.Case" keeps it.
--
-- quickcheck-dynamic draws a test as a 'DynLogicTest': the quantifiers'
-- picks and the steps, in order. 'forAllDLRecorded' is 'forAllDL' with
-- that test handed to the property beside its actions, so a recorder can
-- keep the case it is playing; quickcheck-dynamic's own runner hands the
-- property the actions alone. It needs the formula a 'DL' program denotes
-- ('runDL', exported by byteally's quickcheck-dynamic 4.0.1.1).
module Test.WebApi.Replay
  ( forAllDLRecorded
  , caseItemsOf
    -- * Replay
  , ReplayHooks (..)
  , replayTest
  , webApiReplayHooks
  ) where

import qualified Data.Map.Strict as M
import           Data.Maybe (fromMaybe)
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Typeable (Typeable)
import           Test.QuickCheck (Property, Testable)
import qualified Test.QuickCheck as QC
import           Test.QuickCheck.DynamicLogic (DL, DynLogicModel, runDL)
import           Test.QuickCheck.DynamicLogic.Internal
  ( DynFormula (..), DynLogic (..), DynLogicTest (DLScript), TestContinuation (..), TestSequence (..), Witnesses (..)
  , generateDLTest, getScript, initialStateFor, shrinkDLTest, withDLScript )
import           Test.QuickCheck.DynamicLogic.SmartShrinking (shrinkSmart)
import qualified Test.QuickCheck.DynamicLogic.Utils as QCU
import           Test.QuickCheck.DynamicLogic.Quantify (Quantification (..), QuantifyConstraints)
import           Test.QuickCheck.StateModel
  ( Action, ActionWithPolarity (..), Actions, Annotated (..), Any (..), Polarity (..), Step (..), StateModel
  , Var, computeNextState, computePrecondition, initialAnnotatedState, mkVar )
import           Test.WebApi.Case
import           Data.Proxy (Proxy (..))
import           Data.Reflection (Reifies, reflect)
import           Test.WebApi.StateModel
  ( ApiState, WebApiGlobalStateModel (..), drawnOf, moveIdentity, repointAction, requestLinks, stampDrawn, unWebApiAction )
import           Test.WebApi.Trace (StepInfo (..), kindText)

-- | 'Test.QuickCheck.DynamicLogic.forAllDL', the property handed each test
-- it plays as @keep size test@ beside the test's actions. Generation and
-- narrowing are quickcheck-dynamic's own ('forAllMappedScripts' with the
-- test itself as its representation).
forAllDLRecorded
  :: (DynLogicModel s, Testable a)
  => (Int -> DynLogicTest s -> b)
  -> DL s ()
  -> (b -> Actions s -> a)
  -> Property
forAllDLRecorded keep d k =
  QCU.withSize $ \n ->
    let dl = unDynFormula (runDL initialAnnotatedState d) n
     in QC.forAllShrinkBlind
          (QC.Smart 0 <$> QC.sized (generateDLTest dl))
          (shrinkSmart (shrinkDLTest dl))
          $ \(QC.Smart _ test) -> withDLScript dl (k (keep n test)) test

-- | A pick, whatever its type.
data AnyPick where
  PickOf :: QuantifyConstraints a => a -> AnyPick

-- | A test as a regression case keeps it: each pick by its value, each
-- step by what tells it apart (see "Test.WebApi.Case").
caseItemsOf :: forall s c xstate apps. StateModel xstate => DynLogicTest (ApiState s c xstate apps) -> [CaseItem]
caseItemsOf test = numbered 1 flat
  where
    flat = flatten (getScript test)
    -- the move that bound each variable (its step's number)
    moveOf :: M.Map Int Int
    moveOf = M.fromList [ (stepIndex st, k) | (k, st) <- zip [1 ..] [ st | Right st <- flat ] ]
    numbered :: Int -> [Either AnyPick (Step (ApiState s c xstate apps))] -> [CaseItem]
    numbered _ [] = []
    numbered k (Left (PickOf a) : rest) = ItemPick (encodePick (\i -> fromMaybe 0 (M.lookup i moveOf)) a) : numbered k rest
    numbered k (Right st : rest) = ItemMove (moveRecord (\i -> fromMaybe 0 (M.lookup i moveOf)) k st) : numbered (k + 1) rest

flatten :: TestSequence s -> [Either AnyPick (Step s)]
flatten (TestSeq w) = go w
  where
    go :: Witnesses (TestContinuation s) -> [Either AnyPick (Step s)]
    go = \case
      Do ContStop -> []
      Do (ContStep st rest) -> Right st : flatten rest
      Witness a k -> Left (PickOf a) : go k

-- | A step's variable's index.
stepIndex :: Step s -> Int
stepIndex (v := _) = varIndex v

moveRecord :: forall s c xstate apps. StateModel xstate => (Int -> Int) -> Int -> Step (ApiState s c xstate apps) -> MoveRecord
moveRecord moveOfVar k (_ := ActionWithPolarity {polarAction = act, polarity = pol}) =
  let wact = unWebApiAction act
      (kd, info, op) = moveIdentity wact
  in MoveRecord
      { n = k
      , kind = kindText kd
      , trail = maybe [] (\StepInfo {trail = t} -> t) info
      , fqn = fromMaybe op (info >>= \StepInfo {name = nm} -> nm)
      , negative = pol == NegPolarity
      , generated = (\(operation, draws) -> GeneratedRecord
          { operation, draws
          , links = [ LinkRecord {at = p, move = moveOfVar v} | (p, v) <- requestLinks wact ] }) <$> drawnOf wact
      }

-- ---------------------------------------------------------------------------
-- Replay

-- | What the walker asks of the model: whether the step the program plays
-- here is the case's move ('Nothing'), or why not; and a drawn move made
-- again, its links resolved against the replay's own steps (move number →
-- variable index).
data ReplayHooks s = ReplayHooks
  { sameMove :: forall a. Annotated s -> Action s a -> MoveRecord -> Maybe Text
  , rebuildMove :: Annotated s -> M.Map Int Int -> MoveRecord -> Either Text (Any (ActionWithPolarity s))
  }

-- | Why a branch failed, and how far into the case it got (the deepest is
-- the one reported when every branch fails).
data Blocked = Blocked {consumed :: Int, reason :: Text}

deeper :: Blocked -> Blocked -> Blocked
deeper e1@Blocked {consumed = c1} e2@Blocked {consumed = c2} = if c2 > c1 then e2 else e1

-- | The test a case plays against the program as it is now: walk the
-- program's formula, each quantifier taking the case's next pick and each
-- step the case's next move. 'Left' says why the program no longer plays
-- the case.
replayTest :: forall s. DynLogicModel s => ReplayHooks s -> DynLogic s -> [CaseItem] -> Either Text (DynLogicTest s)
replayTest ReplayHooks {sameMove, rebuildMove} d0 items0 =
  either (\Blocked {reason = r} -> Left r) (Right . DLScript) (go (initialStateFor d0) 0 M.empty 0 d0 items0)
  where
    go :: Annotated s -> Int -> M.Map Int Int -> Int -> DynLogic s -> [CaseItem] -> Either Blocked (TestSequence s)
    -- a case may end before the program does: narrowing cuts a test to a
    -- prefix of the program's steps, and quickcheck-dynamic plays the
    -- prefix as it stands, so the replay stops where the case does
    go _ _ _ _ _ [] = Right (TestSeq (Do ContStop))
    go st nextVar moves k d items = case d of
      EmptySpec -> stuck "the program allows nothing here"
      Stop -> case items of
        ItemMove MoveRecord {n} : _ -> stuck ("the program stops before move " <> tshow n)
        ItemPick _ : _ -> stuck "the program stops before a pick"
      Stopping d' -> go st nextVar moves k d' items
      Weight _ d' -> go st nextVar moves k d' items
      Monitor _ d' -> go st nextVar moves k d' items
      Alt _ a b -> case go st nextVar moves k a items of
        Right t -> Right t
        Left e1 -> case go st nextVar moves k b items of
          Right t -> Right t
          Left e2 -> Left (deeper e1 e2)
      Error msg _ -> stuck ("the program fails here: " <> T.pack msg)
      ForAll q next -> case items of
        ItemPick p : rest -> case decodePick (\m -> M.findWithDefault (-1) m moves) p of
          Left why -> stuck why
          Right a
            | isaQ q a -> prepend a <$> go st nextVar moves (k + 1) (next a) rest
            | otherwise -> stuck "a pick the program no longer allows"
        ItemMove MoveRecord {n} : _ -> stuck ("the program draws a value before move " <> tshow n <> ", which the case does not")
      After aw next -> case items of
        ItemMove m@MoveRecord {n} : rest -> case sameMove st (polarAction aw) m of
          Just why -> stuck why
          Nothing -> step aw (\v st' -> next v st') m n rest
        ItemPick _ : _ -> stuck "the program plays a move where the case has a pick"
      AfterAny next -> case items of
        ItemMove m@MoveRecord {n} : rest -> case rebuildMove st moves m of
          Left why -> stuck ("move " <> tshow n <> ": " <> why)
          Right (Some aw@ActionWithPolarity {}) -> step aw (\_ st' -> next st') m n rest
        ItemPick _ : _ -> stuck "the program draws a move where the case has a pick"
      where
        stuck why = Left Blocked {consumed = k, reason = why}
        prepend :: QuantifyConstraints a => a -> TestSequence s -> TestSequence s
        prepend a (TestSeq w) = TestSeq (Witness a w)
        step :: forall a. (Typeable a, Eq (Action s a), Show (Action s a))
          => ActionWithPolarity s a -> (Var a -> Annotated s -> DynLogic s) -> MoveRecord -> Int -> [CaseItem] -> Either Blocked (TestSequence s)
        step aw next _ n rest
          | not (computePrecondition st aw) = stuck ("move " <> tshow n <> " is not allowed where the program now plays it")
          | otherwise =
              let v = mkVar nextVar
                  st' = computeNextState st aw v
              in (\t -> TestSeq (Do (ContStep (v := aw) t))) <$> go st' (nextVar + 1) (M.insert n nextVar moves) (k + 1) (next v st') rest

tshow :: Show a => a -> Text
tshow = T.pack . show

-- | The hooks for webapi-test's own state: a step the program plays is the
-- case's move when its kind, trail and operation are the ones kept. A
-- drawn move is made again by the model ('rebuildMove' here refuses until
-- the model can).
webApiReplayHooks :: forall s c xstate apps. (StateModel xstate, Reifies s (WebApiGlobalStateModel c xstate apps)) => ReplayHooks (ApiState s c xstate apps)
webApiReplayHooks = ReplayHooks
  { sameMove = \_ act MoveRecord {n, kind = k0, trail = t0, fqn = f0} ->
      let (kd, info, op) = moveIdentity (unWebApiAction act)
          t1 = maybe [] (\StepInfo {trail = t} -> t) info
          f1 = fromMaybe op (info >>= \StepInfo {name = nm} -> nm)
          k1 = kindText kd
          shown ts = if null ts then "(drawn)" else T.intercalate "." ts
      in if k1 == k0 && t1 == t0 && f1 == f0
           then Nothing
           else if t1 == t0 && k1 == k0
             then Just ("move " <> tshow n <> " (" <> shown t0 <> ") changed: it was " <> f0 <> ", it is now " <> f1)
             else Just ("move " <> tshow n <> " (" <> shown t0 <> ", " <> f0 <> ") is gone: the program now plays " <> shown t1 <> " (" <> f1 <> ") there")
  , rebuildMove = \st moves MoveRecord {generated, negative, fqn} -> case generated of
      Nothing -> Left ("the program draws a move here, and the case's (" <> fqn <> ") was not drawn")
      Just GeneratedRecord {operation, draws, links} ->
        case reflect (Proxy @s) of
          WebApiGlobalStateModel {appRedraw} -> do
            Some act <- appRedraw (vars st) (underlyingState st) operation draws
            -- each link to the step of the replay that plays its move
            repointed <- traverse (\LinkRecord {at, move} -> maybe (Left ("it refers to move " <> tshow move <> ", which the replay has not played")) (\v -> Right (at, v)) (M.lookup move moves)) links
            let act' = stampDrawn operation draws (repointAction (M.fromList repointed) act)
            pure (Some (ActionWithPolarity act' (if negative then NegPolarity else PosPolarity)))
  }

