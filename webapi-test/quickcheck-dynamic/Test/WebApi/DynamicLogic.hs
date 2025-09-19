module Test.WebApi.DynamicLogic
  ( propDL
  , prop_api
  , runWebApiTest
  , module Test.WebApi.StateModel
  , Reifies
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
import Data.Reflection

propDL :: forall apps s. Reifies s (WebApiGlobalStateModel apps) => Proxy s -> (forall a. WebApiSessions apps a -> IO a) -> DL (ApiState s apps) () -> Property
propDL _ webapiRunner d = forAllDL d (prop_api webapiRunner)

prop_api :: forall apps s. Reifies s (WebApiGlobalStateModel apps) => (forall a. WebApiSessions apps a -> IO a) -> Actions (ApiState s apps) -> Property
prop_api webapiRunner s =
  monadic (ioProperty . webapiRunner) $ do
    monitor $ counterexample "\nExecution\n"
    _ <- runActions s
    QC.assert True

runWebApiTest :: WebApiGlobalStateModel apps -> (forall (s :: Type). Reifies s (WebApiGlobalStateModel apps) => Proxy s -> r) -> r
runWebApiTest gstate runner = reify gstate (\ps -> runner ps)
