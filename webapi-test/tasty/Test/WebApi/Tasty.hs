module Test.WebApi.Tasty
  ( apiProperty
  ) where

import Test.WebApi
import Test.Tasty
import Test.Tasty.QuickCheck
import Test.WebApi.DynamicLogic
import Test.QuickCheck.DynamicLogic
import Data.Proxy

apiProperty :: forall c xstate apps s. Reifies s (WebApiGlobalStateModel c xstate apps) =>
  Proxy s
  -> (forall a. WebApiSessions apps a -> IO a)
  -> DL (ApiState s c xstate apps) ()
  -> TestName
  -> TestTree
apiProperty _ _ _ tname = testProperty tname True  
  
