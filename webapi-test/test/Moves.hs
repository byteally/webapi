{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
-- | A scenario's moves against a toy platform (zenbridge #2714): an
-- order's 850 is acknowledged by an 855, and the buggy platform does not
-- acknowledge a replacement (BEG01 05), as zenbridge #2698 did not.
--
-- Two moves: @send850@ makes an order with a fresh PO, @replace850@ picks
-- an acknowledged order and sends its replacement. The host observes the
-- platform's documents; the track's @stageOf@ reads an order's stage off
-- them, and a check at @855 sent@ holds one 855 per 850 the moves sent.
--
-- One section has an app of its own, one route that echoes its path
-- (zenbridge #2715): a move's own steps, as its host is handed them, with
-- a call among them. The last three are the host's word on a move once
-- its checks have run, the evidence it attaches to a move, and its being
-- told that a case begins.
module Main (main) where

import           Control.Concurrent (threadDelay)
import           Control.Exception (AllocationLimitExceeded (..), evaluate, try)
import           Control.Monad (unless, void, when)
import           System.Mem (getAllocationCounter)
import           Test.WebApi.Budget
import           Control.Monad.IO.Class (liftIO)
import           Data.Aeson (object, toJSON, (.=))
import           Data.IORef
import           Data.List (isInfixOf)
import qualified Data.Map.Strict as M
import qualified Data.Set as Set
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Typeable (Proxy (..), gcast)
import           System.Exit (exitFailure)
import qualified Test.QuickCheck as QC
import           System.Timeout (timeout)
import           Test.QuickCheck.DynamicLogic (DL, action)
import           Test.QuickCheck.Random (mkQCGen)
import           Test.QuickCheck.StateModel (Any (..), HasVariables (..), LookUp, initialState)
import           Test.WebApi (ClientRequest (..), WebApiSessionsConfig, addApp, emptyApp, runWebApis)
import           Test.WebApi.Case (CaseRecord)
import           Test.WebApi.DynamicLogic
import           Test.WebApi.Moves
import           Test.WebApi.Trace
import           WebApi.Contract
import           WebApi.Server

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
  , stallAt :: Maybe Int
    -- ^ the order whose 850 the platform takes a long while over
  , heavy :: Bool
    -- ^ every 850 costs its sender a lot of allocation
  , atSettle :: IO ()
    -- ^ run as a move's settle step begins
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
receiveNew pf@Platform {orderCount, stallAt, heavy} token = do
  k <- atomicModifyIORef' orderCount (\c -> (c + 1, c + 1))
  when heavy (heavyWork k)
  when (stallAt == Just k) (threadDelay 30000000)
  let p = token <> "/" <> T.pack (show k)
  receive pf p "00"
  pure p

host :: Platform -> MoveHost '[]
host Platform {docs, settles, atSettle} = defMoveHost
  { settle = \_ _ -> liftIO atSettle >> pure (if settles then Right () else Left "the platform is still busy")
  , observe = \_ -> liftIO (Right . SomeObservation <$> readIORef docs)
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
    Right p -> Right <$> liftIO (send p)
    Left why -> pure (Left why)

send850 :: Platform -> DL (St s) ()
send850 pf = move Move
  { name = "send850", info = info "send850", picked = [], makes = [("orders", "850 arrives")]
  , body = do
      token <- freshM "po"
      rush <- genValM (QC.arbitrary :: QC.Gen Bool) QC.shrink
      pv <- sendStep pf "send850" (receiveNew pf) token
      pure MoveBody {values = [SomeVal pv], labels = if rush then Set.singleton "rush" else Set.empty, uses = Set.empty}
  , tracks = M.singleton "orders" orders, judge, budget = 1 }

replace850 :: Platform -> DL (St s) ()
replace850 pf = do
  o <- pickAt "orders" "855 sent" (\_ _ -> True)
  move Move
    { name = "replace850", info = info "replace850", picked = [(o, Just "850 arrives")], makes = []
    , body = do
        pv <- either (fail . T.unpack) pure (thingPo o)
        _ <- sendStep pf "replace850" (\p -> p <$ receive pf p "05") pv
        pure MoveBody {values = [], labels = Set.empty, uses = Set.empty}
    , tracks = M.singleton "orders" orders, judge, budget = 1 }
  where
    thingPo Thing {value = SomeVal v} = maybe (Left "an order's value is its PO") Right (gcast v)

scenario :: Platform -> Double -> DL (St s) ()
scenario pf replaceWeight = anyMoves 6 [(3, send850 pf), (replaceWeight, replace850 pf)]

model :: Platform -> WebApiGlobalStateModel () NoXState '[]
model pf = modelOf (host pf)

-- | The toy model, its moves settled, observed and judged by the host given.
modelOf :: MoveHost '[] -> WebApiGlobalStateModel () NoXState '[]
modelOf h = WebApiGlobalStateModel
  { appAribitaryAction = \_ _ -> pure (Some (mkWebApiAction (ModelOnly "nothing" id)))
  , appInitState = ApiState
      { apiState = mempty, namedEntityTyped = mempty, defaultContext = Nothing, currentContext = Nothing
      , xActionState = initialState, generateFrom = Nothing, classTypes = mempty, prediction = emptyPrediction }
  , appStageOf = noStage
  , appRedraw = noRedraw
  , appMoveHost = h
  }

-- ---------------------------------------------------------------------------
-- Running it

newPlatform :: Bool -> Bool -> IO Platform
newPlatform b s = do
  r <- newIORef []
  c <- newIORef 0
  pure Platform {docs = r, orderCount = c, buggy = b, settles = s, stallAt = Nothing, heavy = False, atSettle = pure ()}

-- | The scenario run, traced: QuickCheck's result and the run's trace.
run :: Platform -> Double -> Int -> IO (QC.Result, RunTrace)
run = runTo Nothing

-- | 'run', what is recorded handed to a sink as it happens.
runTo :: Maybe (TraceEvent -> IO ()) -> Platform -> Double -> Int -> IO (QC.Result, RunTrace)
runTo out pf = runHost (host pf) out pf

-- | 'runTo', against a host of the test's own.
runHost :: MoveHost '[] -> Maybe (TraceEvent -> IO ()) -> Platform -> Double -> Int -> IO (QC.Result, RunTrace)
runHost h out pf w n = do
  rec <- maybe newRecorderFor newRecorderTo out "toy" "orders" ["rush"]
  res <- reify (modelOf h) $ \(ps :: Proxy s) ->
    QC.quickCheckWithResult QC.stdArgs {QC.maxSuccess = n, QC.chatty = False, QC.replay = Just (mkQCGen 7, 0), QC.maxDiscardRatio = 3}
      (propDLTraced rec ps (runWebApis emptyApp) (scenario @s pf w))
  (,) res <$> readRunTrace rec

-- | The cases replayed against a platform.
replay :: Platform -> [CaseRecord] -> IO [ReplayRecord]
replay pf = replayHost (host pf) pf

-- | 'replay', against a host of the test's own.
replayHost :: MoveHost '[] -> Platform -> [CaseRecord] -> IO [ReplayRecord]
replayHost h pf cases = do
  rec <- newRecorderFor "toy" "orders" []
  reify (modelOf h) $ \(ps :: Proxy s) -> replayCasesDL rec ps (runWebApis emptyApp) (scenario @s pf 1) cases
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
    , cutRun
    , budgetRuns
    , sinkRuns
    , sinkCut
    , handedMoves
    , hostWord
    , evidenceRuns
    , caseBeginsRuns
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

-- | A run's time limit cuts it while a move waits on the platform: the
-- cancel reaches the case (nothing on the way swallows it), and the case is
-- recorded as cut, at its last step, with the move it was in.
cutRun :: IO [Bool]
cutRun = do
  putStrLn "a run cut while a move waits on the platform:"
  pf0 <- newPlatform False True
  let pf = pf0 {stallAt = Just 2}
  rec <- newRecorderFor "toy" "orders" []
  r <- timeout 1000000 $ reify (model pf) $ \(ps :: Proxy s) ->
    QC.quickCheckWithResult QC.stdArgs {QC.maxSuccess = 30, QC.chatty = False, QC.replay = Just (mkQCGen 7, 0), QC.maxDiscardRatio = 3}
      (propDLTraced rec ps (runWebApis emptyApp) (scenario @s pf 1))
  closeCut rec "the run passed its time limit of 1 s"
  RunTrace {cases, cut} <- readRunTrace rec
  let lastCase = reverse cases
      cutSteps = maybe [] (\CaseTrace {steps} -> steps) cut
  sequence
    [ check "the time limit cut the run (the cancel was not swallowed)" (isNothing' r)
    , check "the last case is cut, saying why" (case lastCase of CaseSummary {outcome = Cut, why = Just w} : _ -> "time limit" `isInfixOf` T.unpack w; _ -> False)
    , check ("it is cut at its last step: " <> show (map (\CaseSummary {at} -> at) (take 1 lastCase), length cutSteps)) (case lastCase of CaseSummary {at = Just n} : _ -> n == length cutSteps && n > 0; _ -> False)
    , check ("its moves end with the one it was in: " <> show (map (\CaseSummary {moves = ms} -> ms) (take 1 lastCase))) (case lastCase of CaseSummary {moves = ms@(_ : _)} : _ -> last ms == "orders.partner.send850"; _ -> False)
    , check "the trace keeps the cut case's steps" (not (null cutSteps))
    ]
  where
    isNothing' = \case Nothing -> True; Just _ -> False

-- | Work that allocates: what a heavy 850 costs its sender.
heavyWork :: Int -> IO ()
heavyWork k = void (evaluate (length (concatMap show [k .. k + 200000])))

-- | A move over its own budget is cut; a case over its budget is cut at a
-- later move although each of its moves is within the move's budget; and
-- light moves pass under the same budgets.
budgetRuns :: IO [Bool]
budgetRuns = do
  putStrLn "allocation budgets, a case's and a move's:"
  c0 <- getAllocationCounter
  heavyWork 0
  c1 <- getAllocationCounter
  let x = c0 - c1
  heavyPf <- newPlatform False True
  let hpf = heavyPf {heavy = True}
  (rMove, cutMove) <- budgetRun hpf AllocBudget {perCase = Nothing, perMove = Just (x `div` 2)}
  (rCase, cutCase) <- budgetRun hpf AllocBudget {perCase = Just (x * 3 `div` 2), perMove = Just (x * 3 `div` 2)}
  light <- newPlatform False True
  (rLight, _) <- budgetRun light AllocBudget {perCase = Just (x * 3 `div` 2), perMove = Just (x * 3 `div` 2)}
  let movesOf = maybe [] (\CaseTrace {steps} -> [ operation | StepRecord {kind = KindBegin, operation} <- steps ])
  sequence
    [ check ("a move over its budget is cut (" <> show (x `div` 1024) <> " KiB a heavy send, " <> show (x `div` 2048) <> " KiB a move)") (exceeded rMove && not (null (movesOf cutMove)))
    , check ("a case over its budget is cut at a later move: the moves begun " <> show (movesOf cutCase)) (exceeded rCase && length (movesOf cutCase) >= 2)
    , check "light moves pass under the same budgets" (either (const False) QC.isSuccess rLight)
    ]
  where
    exceeded = either (const True) (const False)

budgetRun :: Platform -> AllocBudget -> IO (Either AllocationLimitExceeded QC.Result, Maybe CaseTrace)
budgetRun pf b = do
  rec <- newRecorderFor "toy" "orders" []
  r <- try $ reify (model pf) $ \(ps :: Proxy s) ->
    QC.quickCheckWithResult QC.stdArgs {QC.maxSuccess = 20, QC.chatty = False, QC.replay = Just (mkQCGen 7, 0), QC.maxDiscardRatio = 3}
      (propDLTraced rec ps (\sess -> withCaseBudget b (runWebApis emptyApp sess)) (scenario @s pf 1))
  either (const (closeCut rec "the allocation budget")) (const (pure ())) r
  RunTrace {cut} <- readRunTrace rec
  pure (r, cut)

-- ---------------------------------------------------------------------------
-- The sink (zenbridge #2741)

-- | A sink that keeps what it is handed, in order.
collecting :: IO (TraceEvent -> IO (), IO [TraceEvent])
collecting = do
  ref <- newIORef []
  pure (\e -> modifyIORef' ref (e :), reverse <$> readIORef ref)

-- | The events as plays: each play's phase, its steps in the order handed
-- over, and its close; 'Left' when a step is out of place.
plays :: [TraceEvent] -> Either String [(Phase, [StepRecord], CaseClose)]
plays = go Nothing []
  where
    go open acc = \case
      [] -> case open of
        Nothing -> Right (reverse acc)
        Just (ph, _) -> Left ("a play is left open: " <> show ph)
      StepEnded ph st@StepRecord {n} : rest -> case open of
        Nothing | n == 1 -> go (Just (ph, [st])) acc rest
        Just (ph', sts) | ph' == ph && n == length sts + 1 -> go (Just (ph, sts ++ [st])) acc rest
        _ -> Left ("step " <> show n <> " of " <> show ph <> " is out of place")
      CaseEnded ph c : rest -> case open of
        Nothing -> go Nothing ((ph, [], c) : acc) rest
        Just (ph', sts) | ph' == ph -> go Nothing ((ph, sts, c) : acc) rest
        Just (ph', _) -> Left (show ph <> " closes while " <> show ph' <> " is open")

-- | What a recorder hands its sink: every step of every play once, in
-- order, as its checks left it; each play's close after its last step;
-- the narrowing tries and a replay by their phase; and each step as soon as
-- its checks have run, before the next step begins.
sinkRuns :: IO [Bool]
sinkRuns = do
  putStrLn "a recorder's sink, as the run plays:"
  -- a sound platform: every case passes
  (out, handedOver) <- collecting
  sound0 <- newPlatform False True
  -- what the sink had been handed last, each time a move's settle step began
  atSettles <- newIORef []
  let sound = sound0 {atSettle = handedOver >>= \es -> modifyIORef' atSettles (take 1 (reverse es) :)}
  (_, RunTrace {cases, discarded}) <- runTo (Just out) sound 1 30
  evs <- handedOver
  (_, RunTrace {cases = casesBare}) <- run sound0 1 30
  seen <- readIORef atSettles
  let ps = plays evs
      closes = [ c | Right xs <- [ps], (Generated _, _, c) <- xs ]
      passed = length [ () | CaseClose {outcome = Passed} <- closes ]
      dropped = length [ () | CaseClose {outcome = Discarded} <- closes ]
      settles' = [ st | StepEnded _ st@StepRecord {kind = KindSettle} <- evs ]
      amended StepRecord {checks, model = m} = not (null checks) && m /= Nothing
      line CaseSummary {outcome, moves = ms} = (outcome, ms)
      sendBeforeSettle = \case
        [StepEnded _ StepRecord {kind = KindGlue}] -> True
        _ -> False
  -- a buggy platform: a case fails, and is narrowed
  (outB, handedB) <- collecting
  buggy <- newPlatform True True
  (_, RunTrace {firstFailure, narrowed, narrowing}) <- runTo (Just outB) buggy 1 30
  evsB <- handedB
  let psB = either (const []) id (plays evsB)
      firstFailed = take 1 [ sts | (Generated _, sts, CaseClose {outcome = Failed}) <- psB ]
      tries = [ (k, sts, c) | (Narrowing k, sts, c) <- psB ]
      lastTaken = take 1 (reverse [ sts | (_, sts, CaseClose {outcome = Failed}) <- tries ])
      stepsOf = maybe [] (\CaseTrace {steps} -> steps)
  -- the narrowed case replayed
  (outR, handedR) <- collecting
  recR <- newRecorderTo outR "toy" "orders" []
  reify (model buggy) $ \(ps' :: Proxy s) ->
    replayCasesDL recR ps' (runWebApis emptyApp) (scenario @s buggy 1) (maybe [] pure (narrowed >>= \CaseTrace {replayable} -> replayable))
  evsR <- handedR
  sequence
    [ check ("every step comes once, in order, each play closed after its last step" <> either (": " <>) (const "") ps) (either (const False) (not . null) ps)
    , check ("a close per generated case: " <> show (passed, length cases)) (passed == length cases && passed > 0)
    , check ("a discarded case closes too: " <> show (dropped, discarded)) (dropped == discarded)
    , check "a step is handed over as its checks left it" (not (null settles') && all amended settles')
    , check ("a move's own step is handed over before its settle step begins: " <> show (length seen)) (not (null seen) && all sendBeforeSettle seen)
    , check "the trace is the same with a sink and without" (map line cases == map line casesBare)
    , check "the first failing case's steps are the trace's" (firstFailed == [stepsOf firstFailure] && not (null (stepsOf firstFailure)))
    , check ("the narrowing tries come as tries, one close each: " <> show (length tries, length narrowing)) (length tries == length narrowing && map (\(k, _, _) -> k) tries == [1 .. length tries] && not (null tries))
    , check "the last try that still failed is the narrowed case" (lastTaken == [stepsOf narrowed])
    , check "a replayed case comes as a replay, closed as it fared" (case plays evsR of
        Right [(Replayed 1, sts, CaseClose {outcome = Failed, at})] -> map shape sts == map shape (stepsOf narrowed) && at == Just (length sts) && not (null sts)
        _ -> False)
    ]
  where
    -- a replay's tokens are new: its steps are the narrowed case's by kind and operation
    shape StepRecord {kind, operation, ok} = (kind, operation, ok)

-- | A cut run: the steps the cut case had made reach the sink as the case
-- is closed, then its close, as cut.
sinkCut :: IO [Bool]
sinkCut = do
  putStrLn "a recorder's sink, a run cut:"
  pf0 <- newPlatform False True
  let pf = pf0 {stallAt = Just 2}
  (out, handedOver) <- collecting
  rec <- newRecorderTo out "toy" "orders" []
  _ <- timeout 1000000 $ reify (model pf) $ \(ps :: Proxy s) ->
    QC.quickCheckWithResult QC.stdArgs {QC.maxSuccess = 30, QC.chatty = False, QC.replay = Just (mkQCGen 7, 0), QC.maxDiscardRatio = 3}
      (propDLTraced rec ps (runWebApis emptyApp) (scenario @s pf 1))
  before <- handedOver
  closeCut rec "the run passed its time limit of 1 s"
  after <- handedOver
  RunTrace {cut} <- readRunTrace rec
  let cutSteps = maybe [] (\CaseTrace {steps} -> steps) cut
      lastPlay = either (const []) (take 1 . reverse) (plays after)
  sequence
    [ check "the cut case is open until it is closed" (either (const True) (const False) (plays before))
    , check ("its steps reach the sink, then its close, as cut: " <> show (length cutSteps)) (case lastPlay of
        [(_, sts, CaseClose {outcome = Cut, at, why = Just w})] -> sts == cutSteps && not (null sts) && at == Just (length sts) && "time limit" `isInfixOf` T.unpack w
        _ -> False)
    ]

-- ---------------------------------------------------------------------------
-- A move as its host is handed it (zenbridge #2715)

-- | An app of one route: it answers what its path names, echoed.
data EchoApp

type EchoR = EchoApp :// "echo" :/ Text

instance WebApi EchoApp where
  type Apis EchoApp = '[ Route '[GET] EchoR ]

instance ApiContract EchoApp GET EchoR where
  type ApiOut GET EchoR = Text
  type ApiErr GET EchoR = Text
  type OperationId GET EchoR = 'OpId EchoApp "echo"

data EchoImpl = EchoImpl

instance WebApiServer EchoImpl where
  type HandlerM EchoImpl = IO
  type ApiInterface EchoImpl = EchoApp

instance ApiHandler EchoImpl GET EchoR where
  handler _ req = respond ("echoed " <> pathParam req)

echoSessions :: WebApiSessionsConfig '[EchoApp]
echoSessions = addApp (serverAppTyped serverSettings EchoImpl) emptyApp

type EchoSt s = ApiState s () NoXState '[EchoApp]

-- | A glue step that gives the text.
glue :: Text -> Text -> DL (EchoSt s) (Val Text)
glue nm gives = fmap (Var id) $ action $ mkWebApiAction $
  EffectCall (T.unpack nm) (Just (info nm)) Set.empty (\_ -> pure (Right gives))

-- | A move that tracks nothing: only its steps matter here.
bare :: Text -> DL (EchoSt s) () -> DL (EchoSt s) ()
bare nm steps = move Move
  { name = nm, info = info nm, picked = [], makes = []
  , body = steps >> pure MoveBody {values = [], labels = Set.empty, uses = Set.empty}
  , tracks = M.empty, judge = noJudge, budget = 1 }

-- | A step before any move, then two moves: the first draws a fresh value,
-- calls the app with it and runs a glue step; the second runs one glue
-- step.
handedScenario :: DL (EchoSt s) ()
handedScenario = do
  void (freshM "outside")
  bare "first" $ do
    token <- freshM "po"
    void $ action $ successCall (ClientRequestVal
      {query = pure (), form = pure (), header = pure (), path = token, body = pure (), file = pure ()} :: ClientRequestVal GET EchoR)
    void (glue "stamp" "stamped")
  bare "second" (void (glue "seal" "sealed"))

-- | A step as the checks compare it: its kind, its operation, whether it
-- has a request, its value shown.
type StepShape = (StepKind, Text, Bool, String)

shapeOf :: MovePlayed -> (Text, [StepShape])
shapeOf MovePlayed {name, steps} =
  (name, [ (kind, operation, maybe False (const True) sent, show value) | MoveStep {kind, operation, sent, value} <- steps ])

-- | The scenario played once against a host that keeps the moves its
-- settle and its observe were handed, oldest first.
handedRun :: Bool -> IO (QC.Result, [MovePlayed], [MovePlayed])
handedRun traced = do
  toSettle <- newIORef []
  toObserve <- newIORef []
  let
    keeping = defMoveHost
      { settle = \mv _ -> liftIO (modifyIORef' toSettle (mv :)) >> pure (Right ())
      , observe = \mv -> liftIO (modifyIORef' toObserve (mv :)) >> pure (Right (SomeObservation ()))
      }
    echoModel :: WebApiGlobalStateModel () NoXState '[EchoApp]
    echoModel = WebApiGlobalStateModel
      { appAribitaryAction = \_ _ -> pure (Some (mkWebApiAction (ModelOnly "nothing" id)))
      , appInitState = ApiState
          { apiState = mempty, namedEntityTyped = mempty, defaultContext = Nothing, currentContext = Nothing
          , xActionState = initialState, generateFrom = Nothing, classTypes = mempty, prediction = emptyPrediction }
      , appStageOf = noStage
      , appRedraw = noRedraw
      , appMoveHost = keeping
      }
    args = QC.stdArgs {QC.maxSuccess = 1, QC.chatty = False, QC.replay = Just (mkQCGen 7, 0)}
  rec <- newRecorderFor "toy" "handed" []
  res <- reify echoModel $ \(ps :: Proxy s) -> QC.quickCheckWithResult args $
    if traced
      then propDLTraced rec ps (runWebApis echoSessions) (handedScenario @s)
      else propDL ps (runWebApis echoSessions) (handedScenario @s)
  (,,) res <$> (reverse <$> readIORef toSettle) <*> (reverse <$> readIORef toObserve)

-- | What a move's host is handed: the move by its name, with its own
-- steps in the order they ran and nothing else; a call's request and its
-- answer, read back by their types; and the same to its settle and to its
-- observe, with a recorder listening or none.
handedMoves :: IO [Bool]
handedMoves = do
  putStrLn "a move's own steps, handed to its host:"
  (res, settled, observed) <- handedRun False
  (resT, settledT, observedT) <- handedRun True
  unless (QC.isSuccess res) (putStrLn (QC.output res))
  let
    shapes = map shapeOf settled
    firstSteps = concat (take 1 [ steps | MovePlayed {name = "first", steps} <- settled ])
    drawn = [ st | st@MoveStep {kind = KindFresh} <- firstSteps ]
    calls = [ st | st@MoveStep {kind = KindCall} <- firstSteps ]
    token = concat [ t | Just t <- map (fmap (: []) . answeredAs @Text) (take 1 drawn) ]
    sentPaths = [ p | Just ClientRequest {path = p} <- map (sentAs @GET @EchoR) calls ]
    answers = [ a | Just a <- map (answeredAs @Text) calls ]
  sequence
    [ check "the property passes, with a recorder and without" (QC.isSuccess res && QC.isSuccess resT)
    , check ("each move is handed to its settle by its name: " <> show (map fst shapes)) (map fst shapes == ["first", "second"])
    , check ("a move has its own steps in order, without its begin and settle or a step outside it: " <> show (map (\(k, op, _, _) -> (k, op)) (concatMap snd (take 1 shapes))))
        (map (map (\(k, op, s, _) -> (k, op, s)) . snd) (take 1 shapes) == [[(KindFresh, "fresh po", False), (KindCall, "EchoApp/echo", True), (KindGlue, "stamp", False)]])
    , check ("the second move has only its own: " <> show (map snd (drop 1 shapes))) (map snd (drop 1 shapes) == [[(KindGlue, "seal", False, show ("sealed" :: Text))]])
    , check ("a call's request is read back as it went out: " <> show sentPaths) (not (null token) && sentPaths == token)
    , check ("and its answer by its type: " <> show answers) (not (null token) && answers == map ("echoed " <>) token)
    , check "a value of another type is not read" (all (\st -> answeredAs @Int st == Nothing) firstSteps && all (\st -> maybe True (const False) (sentAs @POST @EchoR st)) calls)
    , check "its observe is handed the same move" (map shapeOf observed == shapes)
    , check "a recorder listening changes nothing of it" (map strip (map shapeOf settledT) == map strip shapes && map strip (map shapeOf observedT) == map strip shapes)
    ]
  where
    -- a play's tokens are its own: the moves compared by all but their values
    strip (nm, sts) = (nm, [ (k, op, s) | (k, op, s, _) <- sts ])

-- ---------------------------------------------------------------------------
-- The host's word after the checks, its evidence, a case's beginning
-- (zenbridge #2715)

-- | The host's word on a move once its checks have run. The host here
-- doubts every replacement. On a sound platform every check holds, so a
-- case that replaces an order is inconclusive, its settle step keeping
-- what its checks left. On the buggy platform the replacement's check
-- fails, so the case fails, and the host's reason stays with the failure.
-- The narrowed case, replayed where its checks hold, is inconclusive:
-- never passed.
hostWord :: IO [Bool]
hostWord = do
  putStrLn "a host's word after the checks:"
  let
    reason = "its directive never fired" :: Text
    doubting pf seen = (host pf)
      { verdict = \MovePlayed {name} _ cs -> do
          liftIO (modifyIORef' seen ((name, cs) :))
          pure (if name == "replace850" then MoveVerdict {inconclusive = Just reason, evidence = []} else noVerdict) }
    allHeld cs = not (null cs) && all (\CheckRecord {held} -> held) cs
  -- a sound platform
  sound <- newPlatform False True
  seenS <- newIORef []
  (out, handedOver) <- collecting
  (resS, RunTrace {cases = casesS}) <- runHost (doubting sound seenS) (Just out) sound 1 30
  evs <- handedOver
  handedS <- readIORef seenS
  let
    undecided = [ st | StepEnded (Generated _) st@StepRecord {inconclusive = Just _} <- evs ]
    asChecksLeft StepRecord {kind, operation, checks, model = m, ok, inconclusive} =
      kind == KindSettle && operation == "replace850" && allHeld checks && m /= Nothing && not ok && inconclusive == Just reason
    replaced CaseSummary {moves = ms} = "orders.partner.replace850" `elem` ms
  -- a platform that never settles: inconclusive before any check ran
  (outU, handedU) <- collecting
  busy <- newPlatform False False
  _ <- runTo (Just outU) busy 1 5
  evsU <- handedU
  let unsettled = [ checks | StepEnded _ StepRecord {inconclusive = Just _, checks} <- evsU ]
  -- the buggy platform
  buggy <- newPlatform True True
  seenB <- newIORef []
  (resB, RunTrace {narrowed}) <- runHost (doubting buggy seenB) Nothing buggy 1 30
  handedB <- readIORef seenB
  let
    failing = [ st | Just CaseTrace {steps} <- [narrowed], st@StepRecord {ok = False} <- steps ]
    kept = [ c | Just CaseTrace {replayable = Just c} <- [narrowed] ]
    failedStage = [ () | ("replace850", cs) <- handedB, CheckRecord {check = "stage", held = False} <- cs ]
  -- the narrowed case, replayed where its checks hold
  sound2 <- newPlatform False True
  seenR <- newIORef []
  again <- replayHost (doubting sound2 seenR) sound2 kept
  sequence
    [ check "the property does not fail" (case resS of QC.Failure {} -> False; _ -> True)
    , check ("a case that replaced an order is inconclusive, saying the host's reason: " <> show (length (filter replaced casesS)))
        (any replaced casesS && all (\c@CaseSummary {outcome, why} -> if replaced c then outcome == Inconclusive && why == Just reason else outcome == Passed) casesS)
    , check ("its settle step keeps what its checks left, and is marked: " <> show (length undecided)) (not (null undecided) && all asChecksLeft undecided)
    , check ("a move that did not settle has no check of its own: " <> show (length unsettled)) (not (null unsettled) && all null unsettled)
    , check "the host is handed every check's record" (not (null handedS) && all (allHeld . snd) handedS)
    , check "on the buggy platform the property fails" (not (QC.isSuccess resB) && case resB of QC.Failure {} -> True; _ -> False)
    , check ("the host saw the check that failed: " <> show (length failedStage)) (not (null failedStage))
    , check "the failing step is not inconclusive, and keeps the host's reason as evidence"
        (map (\StepRecord {kind, operation, inconclusive, evidence} -> (kind, operation, inconclusive, evidence)) failing
          == [(KindSettle, "replace850", Nothing, [Evidence {kind = "otherwise inconclusive", what = reason, detail = Nothing}])])
    , check "the failure's text names the reason" (("evidence, otherwise inconclusive: " <> T.unpack reason) `isInfixOf` QC.output resB)
    , check ("the narrowed case replayed where its checks hold is inconclusive, never passed: " <> show (map (\ReplayRecord {replayOutcome} -> replayOutcome) again))
        (map (\ReplayRecord {replayOutcome, replayWhy} -> (replayOutcome, replayWhy)) again == [(ReplayInconclusive, Just reason)])
    ]

-- | The evidence a host attaches to a move: on the move's settle step and
-- on no other, handed to a sink with the step; counted by kind over the
-- cases that counted, the first three in full; listed with a failure; and
-- bounded.
evidenceRuns :: IO [Bool]
evidenceRuns = do
  putStrLn "the evidence a host attaches to a move:"
  let
    noted nm = Evidence {kind = "noted", what = "seen after " <> nm, detail = Just (toJSON nm)}
    replacement = Evidence {kind = "replacement", what = "a replacement went out", detail = Nothing}
    of' nm = noted nm : [ replacement | nm == "replace850" ]
    noting pf = (host pf) {verdict = \MovePlayed {name} _ _ -> pure MoveVerdict {inconclusive = Nothing, evidence = of' name}}
    moveOf StepRecord {info = i, operation} = maybe operation (\StepInfo {trail} -> T.intercalate "." trail) i
  -- a sound platform: every case passes
  sound <- newPlatform False True
  (out, handedOver) <- collecting
  (res, RunTrace {evidence = counted}) <- runHost (noting sound) (Just out) sound 1 30
  evs <- handedOver
  let
    passedPlays = [ (k, sts) | Right ps <- [plays evs], (Generated k, sts, CaseClose {outcome = Passed}) <- ps ]
    carried = [ (kind, operation, evidence) | StepEnded _ StepRecord {kind, operation, evidence} <- evs ]
    seenOf kd = [ (k, st, e) | (k, sts) <- passedPlays, st@StepRecord {evidence} <- sts, e@Evidence {kind = kd'} <- evidence, kd' == kd ]
    expected kd = EvidenceCount
      { kind = kd
      , cases = Set.size (Set.fromList [ k | (k, _, _) <- seenOf kd ])
      , moves = length (seenOf kd)
      , first = take 3 [ EvidenceSeen {caseNumber = k, move = moveOf st, what, detail} | (k, st, Evidence {what, detail}) <- seenOf kd ] }
  -- the buggy platform: the failure lists the failing move's evidence
  buggy <- newPlatform True True
  (resB, RunTrace {firstFailure}) <- runHost (noting buggy) Nothing buggy 1 30
  let failing = [ evidence | Just CaseTrace {steps} <- [firstFailure], StepRecord {ok = False, evidence} <- steps ]
  -- a host that attaches too much
  let
    long = T.replicate 5000 "x"
    big = toJSON (T.replicate 40000 "y")
    flooding pf = (host pf)
      { verdict = \_ _ _ -> pure MoveVerdict {inconclusive = Nothing, evidence =
          Evidence {kind = "many", what = long, detail = Just big}
            : [ Evidence {kind = "many", what = T.pack (show k), detail = Just (toJSON k)} | k <- [2 .. 150 :: Int] ]} }
  flooded <- newPlatform False True
  (outF, handedF) <- collecting
  _ <- runHost (flooding flooded) (Just outF) flooded 1 5
  evsF <- handedF
  let keptF = concat (take 1 [ evidence | StepEnded _ StepRecord {kind = KindSettle, evidence} <- evsF ])
  -- a host that doubts every move: its cases count nothing
  doubted <- newPlatform False True
  (outD, handedD) <- collecting
  (_, RunTrace {evidence = countedD}) <- runHost ((noting doubted) {verdict = \MovePlayed {name} _ _ -> pure MoveVerdict {inconclusive = Just "never sure", evidence = of' name}}) (Just outD) doubted 1 5
  evsD <- handedD
  let carriedD = [ evidence | StepEnded _ StepRecord {kind = KindSettle, evidence} <- evsD ]
  sequence
    [ check "the property passes" (QC.isSuccess res)
    , check "a move's settle step carries its evidence, and no other step carries any"
        (not (null carried) && all (\(k, op, ev) -> if k == KindSettle then ev == of' op else null ev) carried)
    , check ("the run counts it by kind, over its cases and moves: " <> show (map (\EvidenceCount {kind, cases, moves} -> (kind, cases, moves)) counted))
        (counted == [expected "noted", expected "replacement"] && all (\EvidenceCount {cases, moves} -> cases > 0 && moves >= cases) counted)
    , check "with the first three of a kind in full: where each was seen, its line and its detail"
        (map (\EvidenceCount {first} -> length first) counted == [3, 3] && take 1 [ d | EvidenceCount {kind = "noted", first} <- counted, EvidenceSeen {detail = d} <- first ] == [Just (toJSON ("send850" :: Text))])
    , check "a failure's text lists the failing move's evidence" (not (QC.isSuccess resB) && "evidence, noted: seen after replace850" `isInfixOf` QC.output resB && "evidence, replacement: a replacement went out" `isInfixOf` QC.output resB)
    , check "and its step carries it" (failing == [of' "replace850"])
    , check ("a move keeps a hundred items and counts the rest: " <> show (length keptF))
        (length keptF == 101 && map (\Evidence {kind, what} -> (kind, what)) (drop 100 keptF) == [("evidence not kept", "50 more items (a move keeps 100)")])
    , check "an item's line is clipped" (take 1 [ T.length what | Evidence {what} <- keptF ] == [4001])
    , check "a detail over the limit is replaced by a note of its size, a small one kept"
        (map (\Evidence {detail} -> detail) (take 2 keptF) == [Just (object ["dropped" .= (40002 :: Int)]), Just (toJSON (2 :: Int))])
    , check ("an inconclusive case counts no evidence, though its step carries it: " <> show (length carriedD)) (null countedD && not (null carriedD) && all (not . null) carriedD)
    ]

-- | The host is told that a case begins: once for every play (a generated
-- case, a narrowing try, a regression case replayed), before its first
-- step.
caseBeginsRuns :: IO [Bool]
caseBeginsRuns = do
  putStrLn "a host told that a case begins:"
  let
    -- each telling noted with what the sink had been handed last
    telling pf told handedOver = (host pf) {caseBegins = liftIO (handedOver >>= \es -> modifyIORef' told (take 1 (reverse es) :))}
    betweenPlays = all (\case [] -> True; [CaseEnded {}] -> True; _ -> False)
    closes es = length [ () | CaseEnded {} <- es ]
  sound <- newPlatform False True
  toldS <- newIORef []
  (out, handedOver) <- collecting
  _ <- runHost (telling sound toldS handedOver) (Just out) sound 1 30
  evs <- handedOver
  beganS <- readIORef toldS
  buggy <- newPlatform True True
  toldB <- newIORef []
  (outB, handedB) <- collecting
  (_, RunTrace {narrowed, narrowing}) <- runHost (telling buggy toldB handedB) (Just outB) buggy 1 30
  evsB <- handedB
  beganB <- readIORef toldB
  toldR <- newIORef []
  (outR, handedR) <- collecting
  recR <- newRecorderTo outR "toy" "orders" []
  reify (modelOf (telling buggy toldR handedR)) $ \(ps :: Proxy s) ->
    replayCasesDL recR ps (runWebApis emptyApp) (scenario @s buggy 1) [ c | Just CaseTrace {replayable = Just c} <- [narrowed] ]
  beganR <- readIORef toldR
  sequence
    [ check ("once for every generated case: " <> show (length beganS, closes evs)) (length beganS == closes evs && not (null beganS))
    , check "before the case's first step" (betweenPlays beganS)
    , check ("and for every narrowing try: " <> show (length beganB, closes evsB, length narrowing)) (length beganB == closes evsB && not (null narrowing) && betweenPlays beganB)
    , check ("and for a regression case replayed: " <> show (length beganR)) (length beganR == 1)
    ]

