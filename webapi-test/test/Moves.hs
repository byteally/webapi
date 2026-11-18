{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
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

import           Control.Concurrent (threadDelay)
import           Control.Exception (AllocationLimitExceeded (..), evaluate, try)
import           Control.Monad (unless, void, when)
import           System.Mem (getAllocationCounter)
import           Test.WebApi.Budget
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
import           System.Timeout (timeout)
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
host Platform {docs, settles, atSettle} = MoveHost
  { settle = \_ -> liftIO atSettle >> pure (if settles then Right () else Left "the platform is still busy")
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
  pure Platform {docs = r, orderCount = c, buggy = b, settles = s, stallAt = Nothing, heavy = False, atSettle = pure ()}

-- | The scenario run, traced: QuickCheck's result and the run's trace.
run :: Platform -> Double -> Int -> IO (QC.Result, RunTrace)
run = runTo Nothing

-- | 'run', what is recorded handed to a sink as it happens.
runTo :: Maybe (TraceEvent -> IO ()) -> Platform -> Double -> Int -> IO (QC.Result, RunTrace)
runTo out pf w n = do
  rec <- maybe newRecorderFor newRecorderTo out "toy" "orders" ["rush"]
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
    , cutRun
    , budgetRuns
    , sinkRuns
    , sinkCut
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
