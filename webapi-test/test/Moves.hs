{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
-- | A scenario's moves against a toy platform (zenbridge #2714): an
-- order's 850 is acknowledged by an 855, and the buggy platform does not
-- acknowledge a replacement (BEG01 05), as zenbridge #2698 did not.
--
-- Two moves: @send850@ makes an order with a fresh PO, @replace850@ picks
-- an acknowledged order and sends its replacement. The host observes the
-- platform's documents; the track's @stageOf@ reads an order's stage off
-- them, and a check at @855 sent@ holds one 855 per 850 the moves sent.
module Main (main) where

import           Control.Monad (unless)
import           Control.Monad.IO.Class (liftIO)
import           Data.IORef
import           Data.List (isInfixOf)
import qualified Data.Map.Strict as M
import qualified Data.Set as Set
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Typeable (Proxy (..), gcast)
import           System.Exit (exitFailure)
import qualified Test.QuickCheck as QC
import           Test.QuickCheck.DynamicLogic (DL, action)
import           Test.QuickCheck.Random (mkQCGen)
import           Test.QuickCheck.StateModel (Any (..), HasVariables (..), LookUp, initialState)
import           Test.WebApi (emptyApp, runWebApis)
import           Test.WebApi.Case (CaseRecord)
import           Test.WebApi.DynamicLogic
import           Test.WebApi.Moves
import           Test.WebApi.Trace

-- ---------------------------------------------------------------------------
-- The platform under test

data Doc = Doc {kind :: Text, po :: Text, purpose :: Text}
  deriving (Show, Eq)

-- | The platform keeps its documents across cases, as a real one does: a
-- check reads only its own orders' (by PO).
data Platform = Platform
  { docs :: IORef [Doc]
  , orderCount :: IORef Int
  , buggy :: Bool
  , settles :: Bool
  }

-- | An 850 arrives; the platform acknowledges it, except a replacement
-- when it is buggy.
receive :: Platform -> Text -> Text -> IO ()
receive Platform {docs, buggy} p purp = do
  modifyIORef' docs (++ [Doc {kind = "850", po = p, purpose = purp}])
  unless (buggy && purp == "05") $
    modifyIORef' docs (++ [Doc {kind = "855", po = p, purpose = purp}])

-- | An original 850: the PO is the sender's fresh value, made unique here
-- (two cases may draw their tokens in the same millisecond).
receiveNew :: Platform -> Text -> IO Text
receiveNew pf@Platform {orderCount} token = do
  k <- atomicModifyIORef' orderCount (\c -> (c + 1, c + 1))
  let p = token <> "/" <> T.pack (show k)
  receive pf p "00"
  pure p

host :: Platform -> MoveHost '[]
host Platform {docs, settles} = MoveHost
  { settle = \_ -> pure (if settles then Right () else Left "the platform is still busy")
  , observe = liftIO (Right . SomeObservation <$> readIORef docs)
  , hostChecks = const []
  }

-- ---------------------------------------------------------------------------
-- The scenario

type St s = ApiState s () NoXState '[]

orders :: Track
orders = Track
  { name = "orders"
  , stages = ["850 arrives", "855 sent"]
  , refusals = Set.empty
  , answers = [Answer {from = "850 arrives", to = "855 sent", when = Nothing, label = Nothing}]
  }

poOf :: LookUp -> Thing -> Either Text Text
poOf lkp Thing {value = SomeVal v} = case gcast v of
  Just (pv :: Val Text) -> resolveVal lkp pv
  Nothing -> Left "an order's value is its PO"

count :: Text -> Text -> [Doc] -> Int
count k p ds = length [ () | Doc {kind, po} <- ds, kind == k, po == p ]

judge :: Judge
judge = Judge
  { stageOf = M.singleton "orders" $ StageOf $ \lkp ds t -> do
      p <- poOf lkp t
      let sent = count "850" p ds
      pure (if sent > 0 && count "855" p ds == sent then "855 sent" else "850 arrives")
  , atStage =
      [ ( "one-855-per-850", ("orders", "855 sent"), ThingCheck $ \lkp _ t@Thing {movesOn} ds -> do
            p <- poOf lkp t
            let acks = count "855" p ds
                sends = length (filter (`elem` ["send850", "replace850"]) movesOn)
            if acks == sends then Right () else Left (T.pack (show acks <> " 855s for " <> show sends <> " 850s sent")) ) ]
  , afterMove = []
  , always =
      [ ( "an 850 per move", PredictionCheck $ \lkp p ds ->
            let ours = Set.fromList [ x | t <- predictedThings p, Right x <- [poOf lkp t] ]
                moved = sum [ length movesOn | Thing {movesOn} <- predictedThings p ]
                seen = length [ () | Doc {kind = "850", po} <- ds, Set.member po ours ]
            in if moved == seen then Right () else Left (T.pack (show seen <> " 850s seen, " <> show moved <> " sent")) ) ]
  }

info :: Text -> StepInfo
info nm = StepInfo {trail = ["orders", "partner", nm], summary = nm, name = Just nm}

-- | A glue step: the 850 sent; its value is the order's PO.
sendStep :: Platform -> Text -> (Text -> IO Text) -> Val Text -> DL (St s) (Val Text)
sendStep _ nm send pv = fmap (Var id) $ action $ mkWebApiAction $
  EffectCall (T.unpack nm) (Just (info nm)) (getAllVariables pv) $ \lkp -> case resolveVal lkp pv of
    Right p -> Right <$> send p
    Left why -> pure (Left why)

send850 :: Platform -> DL (St s) ()
send850 pf = move Move
  { name = "send850", info = info "send850", picked = [], makes = [("orders", "850 arrives")]
  , body = do
      token <- freshM "po"
      rush <- genValM (QC.arbitrary :: QC.Gen Bool) QC.shrink
      pv <- sendStep pf "send850" (receiveNew pf) token
      pure ([SomeVal pv], if rush then Set.singleton "rush" else Set.empty)
  , tracks = M.singleton "orders" orders, judge, budget = 1 }

replace850 :: Platform -> DL (St s) ()
replace850 pf = do
  o <- pickAt "orders" "855 sent" (\_ _ -> True)
  move Move
    { name = "replace850", info = info "replace850", picked = [(o, Just "850 arrives")], makes = []
    , body = do
        pv <- either (fail . T.unpack) pure (thingPo o)
        _ <- sendStep pf "replace850" (\p -> p <$ receive pf p "05") pv
        pure ([], Set.empty)
    , tracks = M.singleton "orders" orders, judge, budget = 1 }
  where
    thingPo Thing {value = SomeVal v} = maybe (Left "an order's value is its PO") Right (gcast v)

scenario :: Platform -> Double -> DL (St s) ()
scenario pf replaceWeight = anyMoves 6 [(3, send850 pf), (replaceWeight, replace850 pf)]

model :: Platform -> WebApiGlobalStateModel () NoXState '[]
model pf = WebApiGlobalStateModel
  { appAribitaryAction = \_ _ -> pure (Some (mkWebApiAction (ModelOnly "nothing" id)))
  , appInitState = ApiState
      { apiState = mempty, namedEntityTyped = mempty, defaultContext = Nothing, currentContext = Nothing
      , xActionState = initialState, generateFrom = Nothing, classTypes = mempty, prediction = emptyPrediction }
  , appStageOf = noStage
  , appRedraw = noRedraw
  , appMoveHost = host pf
  }

-- ---------------------------------------------------------------------------
-- Running it

newPlatform :: Bool -> Bool -> IO Platform
newPlatform b s = do
  r <- newIORef []
  c <- newIORef 0
  pure Platform {docs = r, orderCount = c, buggy = b, settles = s}

-- | The scenario run, traced: QuickCheck's result and the run's trace.
run :: Platform -> Double -> Int -> IO (QC.Result, RunTrace)
run pf w n = do
  rec <- newRecorderFor "toy" "orders" ["rush"]
  res <- reify (model pf) $ \(ps :: Proxy s) ->
    QC.quickCheckWithResult QC.stdArgs {QC.maxSuccess = n, QC.chatty = False, QC.replay = Just (mkQCGen 7, 0), QC.maxDiscardRatio = 3}
      (propDLTraced rec ps (runWebApis emptyApp) (scenario @s pf w))
  (,) res <$> readRunTrace rec

-- | The cases replayed against a platform.
replay :: Platform -> [CaseRecord] -> IO [ReplayRecord]
replay pf cases = do
  rec <- newRecorderFor "toy" "orders" []
  reify (model pf) $ \(ps :: Proxy s) -> replayCasesDL rec ps (runWebApis emptyApp) (scenario @s pf 1) cases
  RunTrace {replays} <- readRunTrace rec
  pure replays

check :: String -> Bool -> IO Bool
check what ok = do
  putStrLn ((if ok then "  ok   " else "  FAIL ") ++ what)
  pure ok

main :: IO ()
main = do
  results <- sequence
    [ soundPlatform
    , buggyPlatform
    , unsettledPlatform
    ]
  unless (and (concat results)) exitFailure

soundPlatform :: IO [Bool]
soundPlatform = do
  putStrLn "a sound platform:"
  pf <- newPlatform False True
  (res, RunTrace {cases, moves, labels, firstFailure}) <- run pf 1 30
  unless (QC.isSuccess res) $ do
    putStrLn (QC.output res)
    mapM_ (\CaseTrace {steps} -> mapM_ print steps) firstFailure
  let names = Set.fromList (concat [ ms | CaseSummary {moves = ms} <- cases ])
      played m = sum [ t | MoveCount {move = m', timesPlayed = t} <- moves, m' == m ]
  sequence
    [ check "the property passes" (QC.isSuccess res)
    , check "every case passed" (all (\CaseSummary {outcome} -> outcome == Passed) cases)
    , check ("a case lists its moves, not their steps: " <> show (Set.toList names)) (names `Set.isSubsetOf` Set.fromList ["orders.partner.send850", "orders.partner.replace850"] && not (Set.null names))
    , check ("the heavier move is drawn more: " <> show (played "send850", played "replace850")) (played "send850" > played "replace850" && played "replace850" > 0)
    , check "a draw's label is counted" (any (\LabelCount {label, cases = k} -> label == "rush" && k > 0) labels)
    ]

buggyPlatform :: IO [Bool]
buggyPlatform = do
  putStrLn "a platform that does not acknowledge a replacement:"
  pf <- newPlatform True True
  (res, RunTrace {narrowed}) <- run pf 1 30
  case narrowed of
    Nothing -> sequence [check "the property fails, with a narrowed case" False]
    Just CaseTrace {steps, replayable} -> do
      let ms = [ operation | StepRecord {kind = KindSettle, operation} <- steps ]
          failing = [ s | s@StepRecord {ok = False} <- steps ]
          failedChecks = [ (c, subj) | StepRecord {checks} <- failing, CheckRecord {check = c, held = False, subject = subj} <- checks ]
          outside = [ operation | StepRecord {inMove = Nothing, operation} <- steps ]
      sound <- newPlatform False True
      again <- replay pf (maybe [] pure replayable)
      fixed <- replay sound (maybe [] pure replayable)
      sequence
        [ check "the property fails" (not (QC.isSuccess res))
        , check ("the narrowed case is two whole moves: " <> show ms) (ms == ["send850", "replace850"])
        , check ("every step is in a move: " <> show outside) (null outside)
        , check ("it fails at replace850's settle step: " <> show [ operation | StepRecord {operation} <- failing ]) (map (\StepRecord {kind, operation} -> (kind, operation)) failing == [(KindSettle, "replace850")])
        , check ("the failure names the check and the order: " <> show failedChecks) (("stage", Just "orders #1") `elem` failedChecks)
        , check "the narrowed case replays to the same failure" (map (\ReplayRecord {replayOutcome, replayAt} -> (replayOutcome, replayAt)) again == [(ReplayFailed, Just (length steps))])
        , check "and passes once the platform is fixed" (map (\ReplayRecord {replayOutcome} -> replayOutcome) fixed == [ReplayPassed])
        ]

unsettledPlatform :: IO [Bool]
unsettledPlatform = do
  putStrLn "a platform that never settles:"
  pf <- newPlatform False False
  (res, RunTrace {cases}) <- run pf 1 5
  let whys = [ w | CaseSummary {outcome = Inconclusive, why = Just w} <- cases ]
  sequence
    [ check "the property does not fail" (case res of QC.Failure {} -> False; _ -> True)
    , check "no case failed" (all (\CaseSummary {outcome} -> outcome /= Failed) cases)
    , check ("the cases that played a move are inconclusive, saying why: " <> show (take 1 whys)) (not (null whys) && all ("still busy" `isInfixOf`) (map T.unpack whys))
    ]
