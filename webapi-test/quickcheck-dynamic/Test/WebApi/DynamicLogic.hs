module Test.WebApi.DynamicLogic
  ( propDL
  , propDLTraced
  , replayCasesDL
  , prop_api
  -- , runWebApiTest
  -- , apiAction
  , apiForAllVar
  , getCtxAtTypeDL
  , arbitraryVal
  , module Test.WebApi.StateModel
  , Reifies
  , reify 
  ) where

import Test.WebApi.StateModel
import Test.WebApi
import Test.QuickCheck.StateModel
import Test.QuickCheck.DynamicLogic
-- import Data.Kind
import Data.Typeable
import Test.QuickCheck
import Test.QuickCheck.Monadic
import Test.QuickCheck.Monadic qualified as QC
import Data.Reflection
import Data.IORef
import Test.WebApi.Trace (Recorder, ReplayOutcome (..), ReplayRecord (..), StepRecord (..), beginCaseWith, noteReplay, recordedProperty, stepsFailure, takeSteps)
import Test.WebApi.Prediction (MoveInconclusive (..))
import Control.Exception (catch)
import Data.List (find)
import Test.WebApi.Case (CaseItem, CaseRecord (..))
import Test.WebApi.Replay (caseItemsOf, forAllDLRecorded, narrowThroughProgram, replayTest, webApiReplayHooks)
import Test.QuickCheck.DynamicLogic.Internal (DynFormula (..), withDLScript)
import qualified Test.QuickCheck as QC
import qualified Data.Text as T
-- import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad (forM_)
import Control.Applicative ((<|>))

propDL :: forall c xstate apps e s.
  ( DynLogicModel xstate
  , RunModel xstate IO
  , XActionError e ~ Error xstate IO
  , Typeable e
  , Show e
  , Reifies s (WebApiGlobalStateModel c xstate apps)
  ) => Proxy s -> (forall a. WebApiSessions apps a -> IO a) -> DL (ApiState s c xstate apps) () -> Property
propDL _ webapiRunner d = forAllDLRecorded narrowThroughProgram (\_ _ -> ()) d (\() -> propApiWith Nothing webapiRunner)

-- | 'propDL', each case it plays recorded into the recorder: the steps as
-- they run, the case closed when QuickCheck finishes it (a generated case
-- or a narrowing try alike; see "Test.WebApi.Trace"), and the case as a
-- regression case would keep it ("Test.WebApi.Case"), so the narrowed one
-- can be replayed. Narrowing goes through the program
-- ('narrowThroughProgram'): a shorter case is made again from its picks,
-- and a scenario's move is kept or dropped whole.
propDLTraced :: forall c xstate apps e s.
  ( DynLogicModel xstate
  , RunModel xstate IO
  , XActionError e ~ Error xstate IO
  , Typeable e
  , Show e
  , Reifies s (WebApiGlobalStateModel c xstate apps)
  ) => Recorder -> Proxy s -> (forall a. WebApiSessions apps a -> IO a) -> DL (ApiState s c xstate apps) () -> Property
propDLTraced rec _ webapiRunner d = recordedProperty rec $
  forAllDLRecorded narrowThroughProgram (\size test -> (size, caseItemsOf test)) d (\c -> propApiWithCase (Just rec) (Just c) webapiRunner)

-- | Each regression case played once against the program as it is now
-- (zenbridge #2713), before the property draws any of its own: how it
-- fared noted in the recorder ('Test.WebApi.Trace.replays'), its moves
-- in full. A case the program no longer plays is noted as such, with why.
replayCasesDL :: forall c xstate apps e s.
  ( DynLogicModel xstate
  , RunModel xstate IO
  , XActionError e ~ Error xstate IO
  , Typeable e
  , Show e
  , Reifies s (WebApiGlobalStateModel c xstate apps)
  ) => Recorder -> Proxy s -> (forall a. WebApiSessions apps a -> IO a) -> DL (ApiState s c xstate apps) () -> [CaseRecord] -> IO ()
replayCasesDL rec _ webapiRunner d kept = forM_ (zip [1 ..] kept) $ \(k, CaseRecord {size, items}) -> do
  let dl = unDynFormula (runDL initialAnnotatedState d) size
  case replayTest webApiReplayHooks dl items of
    Left why -> noteReplay rec ReplayRecord {caseNumber = k, replayOutcome = CantReplay, replayAt = Nothing, replayWhy = Just why, replaySteps = []}
    Right test -> do
      res <- QC.quickCheckWithResult QC.stdArgs {QC.maxSuccess = 1, QC.maxShrinks = 0, QC.chatty = False}
               (withDLScript dl (propApiWithCase (Just rec) Nothing webapiRunner) test)
      played <- takeSteps rec
      let (at, why) = stepsFailure played
          unsettled = find (\StepRecord {inconclusive} -> inconclusive /= Nothing) played
          qcWhy = case res of
            QC.Failure {QC.reason = r} -> Just (T.pack r)
            _ -> Nothing
          outcome = case (unsettled, res) of
            (Just _, _) -> ReplayInconclusive
            (_, QC.Success {}) -> ReplayPassed
            (_, QC.GaveUp {}) -> CantReplay
            _ -> ReplayFailed
          replayWhy = case outcome of
            CantReplay -> Just (T.pack "the program discards it")
            ReplayInconclusive -> unsettled >>= \StepRecord {inconclusive} -> inconclusive
            _ -> why <|> qcWhy
      noteReplay rec ReplayRecord
        { caseNumber = k, replayOutcome = outcome, replayAt = at
        , replayWhy
        , replaySteps = played }

prop_api :: forall c xstate apps e s.
  ( DynLogicModel xstate
  , RunModel xstate IO
  , XActionError e ~ Error xstate IO
  , Typeable e
  , Show e
  , Reifies s (WebApiGlobalStateModel c xstate apps)
  ) => IORef (Maybe (ApiState s c xstate apps))
  -> (forall a. WebApiSessions apps a -> IO a)
  -> Actions (ApiState s c xstate apps)
  -> Property
prop_api _newStRef = propApiWith Nothing

-- | The actions run, recorded when there is a recorder.
propApiWith :: forall c xstate apps e s.
  ( DynLogicModel xstate
  , RunModel xstate IO
  , XActionError e ~ Error xstate IO
  , Typeable e
  , Show e
  , Reifies s (WebApiGlobalStateModel c xstate apps)
  ) => Maybe Recorder
  -> (forall a. WebApiSessions apps a -> IO a)
  -> Actions (ApiState s c xstate apps)
  -> Property
propApiWith rec = propApiWithCase rec Nothing

-- | 'propApiWith', the case started with how a regression case keeps it.
propApiWithCase :: forall c xstate apps e s.
  ( DynLogicModel xstate
  , RunModel xstate IO
  , XActionError e ~ Error xstate IO
  , Typeable e
  , Show e
  , Reifies s (WebApiGlobalStateModel c xstate apps)
  ) => Maybe Recorder
  -> Maybe (Int, [CaseItem])
  -> (forall a. WebApiSessions apps a -> IO a)
  -> Actions (ApiState s c xstate apps)
  -> Property
propApiWithCase rec kept webapiRunner s =
  -- a move that did not settle stops the case: neither passed nor failed,
  -- so QuickCheck discards it (the trace says it was inconclusive)
  monadic (\m -> ioProperty (webapiRunner (runReaderT m (maybe initWebApiSessionsCxt tracingCxt rec)) `catch` inconclusive)) $ do
    mapM_ (\r -> QC.run (liftIO (beginCaseWith r kept))) rec
    monitor $ counterexample "\nExecution\n"
    (_anonSt, _env) <- runActions s
    -- let 
    --   newApiState = resolveNamedEntities env $ underlyingState anonSt
    -- liftIO $ writeIORef newStRef (Just newApiState)
    QC.assert True
  where
    inconclusive MoveInconclusive {move = mv, why} =
      pure (QC.counterexample ("move " ++ T.unpack mv ++ " is inconclusive: " ++ T.unpack why) (QC.property QC.Discard))

-- runWebApiTest :: forall r apps. WebApiGlobalStateModel apps -> (forall (s :: Type). Reifies s (WebApiGlobalStateModel apps) => Proxy s -> r) -> r
-- runWebApiTest gstate runner = reify gstate (\ps -> runner ps)

-- apiAction ::
--   ( Typeable a
--   , Eq (Action (ApiState s apps) a)
--   , Show (Action (ApiState s apps) a)
--   , ContextSwitch c
--   ) => ApiActionM c apps (DL (ApiState s apps)) (ApiAction apps a)
--   -> ApiActionM c apps (DL (ApiState s apps)) (Val a)
-- apiAction actM = do
--   ApiAction act <- actM
--   liftApiDL $ fmap (Var id) $ action act
  
 -- getModelStateDL >>= (\st -> fmap (Var id) . action $ act st)

-- apiAction' :: (Typeable a, Eq (Action s a), Show (Action s a)) => Action s a -> DL s (Val a)
-- apiAction' = fmap (Var id) . action

apiForAllVar :: forall a s. Typeable a => DL s (Val a)
apiForAllVar = fmap (Var id) forAllVar

getCtxAtTypeDL :: forall a s. Typeable a => DL s [Val a]
getCtxAtTypeDL = (fmap (Var id) . ctxAtType @a) <$> getVarContextDL

arbitraryVal :: Typeable a => VarContext -> Gen (Val a)
arbitraryVal = fmap (Var id) . arbitraryVar
