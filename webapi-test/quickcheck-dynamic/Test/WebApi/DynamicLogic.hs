module Test.WebApi.DynamicLogic
  ( propDL
  , propDLTraced
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
import Test.WebApi.Trace (Recorder, beginCase, recordedProperty)
-- import Control.Monad.IO.Class
import Control.Monad.Reader

propDL :: forall c xstate apps e s.
  ( DynLogicModel xstate
  , RunModel xstate IO
  , XActionError e ~ Error xstate IO
  , Typeable e
  , Show e
  , Reifies s (WebApiGlobalStateModel c xstate apps)
  ) => Proxy s -> (forall a. WebApiSessions apps a -> IO a) -> DL (ApiState s c xstate apps) () -> Property
propDL _ webapiRunner d = forAllDL d (prop_api undefined webapiRunner)

-- | 'propDL', each case it plays recorded into the recorder: the steps as
-- they run, the case closed when QuickCheck finishes it (a generated case
-- or a narrowing try alike; see "Test.WebApi.Trace").
propDLTraced :: forall c xstate apps e s.
  ( DynLogicModel xstate
  , RunModel xstate IO
  , XActionError e ~ Error xstate IO
  , Typeable e
  , Show e
  , Reifies s (WebApiGlobalStateModel c xstate apps)
  ) => Recorder -> Proxy s -> (forall a. WebApiSessions apps a -> IO a) -> DL (ApiState s c xstate apps) () -> Property
propDLTraced rec _ webapiRunner d = recordedProperty rec $ forAllDL d (propApiWith (Just rec) webapiRunner)

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
propApiWith rec webapiRunner s =
  monadic (ioProperty . webapiRunner . flip runReaderT (maybe initWebApiSessionsCxt tracingCxt rec)) $ do
    mapM_ (QC.run . liftIO . beginCase) rec
    monitor $ counterexample "\nExecution\n"
    (_anonSt, _env) <- runActions s
    -- let 
    --   newApiState = resolveNamedEntities env $ underlyingState anonSt
    -- liftIO $ writeIORef newStRef (Just newApiState)
    QC.assert True

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
