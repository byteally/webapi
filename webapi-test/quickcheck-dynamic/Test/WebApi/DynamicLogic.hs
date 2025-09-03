module Test.WebApi.DynamicLogic
  ( successCall
  , errorCall
  , someExceptionCall
  , propDL
  , prop_api
  , module Test.WebApi.StateModel
  ) where

import Test.WebApi.StateModel
import Test.WebApi
import Test.QuickCheck.StateModel
import Test.QuickCheck.DynamicLogic
import WebApi.Contract
import WebApi.Param
import WebApi.ContentTypes
import Control.Exception (SomeException)
import Data.Kind
import Data.Typeable
import Test.QuickCheck
import Test.QuickCheck.Monadic
import Test.QuickCheck.Monadic qualified as QC
import Test.QuickCheck.Extras

  
successCall :: forall meth r app apps. WebApiActionCxt apps meth app r =>
  ClientRequest meth (app :// r)
  -> DL (ApiState apps) (Var (ApiOut meth (app :// r)))
successCall creq = action (mkWebApiAction (SuccessCall creq))

errorCall :: forall meth r app apps.WebApiActionCxt apps meth app r =>
  ClientRequest meth (app :// r)
  -> DL (ApiState apps) (Var (ApiErr meth (app :// r)))
errorCall creq = action (mkWebApiAction (ErrorCall creq))

someExceptionCall :: forall meth r app apps. WebApiActionCxt apps meth app r =>
  ClientRequest meth (app :// r)
  -> DL (ApiState apps) (Var SomeException)
someExceptionCall creq = action (mkWebApiAction (SomeExceptionCall creq))
  
propDL :: (forall a. WebApiSessions apps a -> IO a) -> DL (ApiState apps) () -> Property
propDL webapiRunner d = forAllDL d (prop_api webapiRunner)

prop_api :: forall apps. (forall a. WebApiSessions apps a -> IO a) -> Actions (ApiState apps) -> Property
prop_api webapiRunner s =
  monadic (ioProperty . webapiRunner) $ do
    monitor $ counterexample "\nExecution\n"
    _ <- runActions s
    QC.assert True

{-
prop_api :: forall apps. WebApiSessionsConfig apps -> Actions (ApiState apps) -> Property
prop_api _ s =
  monadicIO $ do
    monitor $ counterexample "\nExecution\n"
    _ <- runPropertyStateT (runPropertyReaderT (hoistPropM (runWebApiSessions @apps) WebApiSessions $ runActions s) undefined) undefined
    QC.assert True


hoistPropM :: (forall x. m x -> n x) -> (forall x. n x -> m x) -> PropertyM m a -> PropertyM n a
hoistPropM fw bw p = MkPropertyM $ \hf -> fmap fw $ unPropertyM p ((fmap . fmap) bw hf)
-}
