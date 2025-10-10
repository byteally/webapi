module Test.WebApi.Tasty
  ( apiProperty
  ) where

import Test.WebApi
import Test.Tasty
import Test.Tasty.QuickCheck
import Test.WebApi.DynamicLogic
import Test.QuickCheck.DynamicLogic
import Data.Proxy

apiProperty :: forall apps s. Reifies s (WebApiGlobalStateModel apps) =>
  Proxy s
  -> (forall a. WebApiSessions apps a -> IO a)
  -> DL (ApiState s apps) ()
  -> TestName
  -> TestTree
apiProperty _ _ _ tname = testProperty tname True  
  
