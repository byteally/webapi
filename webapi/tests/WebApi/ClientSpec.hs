{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# LANGUAGE MultiParamTypeClasses, TypeFamilies, OverloadedStrings, DataKinds, TypeOperators, TypeSynonymInstances, FlexibleInstances, DeriveGeneric, DeriveAnyClass #-}

module WebApi.ClientSpec (spec) where

import Test.Hspec
import Test.Hspec.Wai (with)
import Data.Aeson (ToJSON (..), FromJSON (..))
import WebApi
import Data.Text (Text)
import qualified Network.Wai as Wai
import Network.Wai.Handler.Warp (run)
import Control.Concurrent (forkIO)
import GHC.Generics

withApp :: SpecWith () -> Spec
withApp = with (forkIO (run 8080 clientSpecApp) >> return ())

clientSpecApp :: Wai.Application
clientSpecApp = serverApp serverSettings ClientSpecImpl

data ClientSpec
data ClientSpecImpl = ClientSpecImpl

type Persons = "person" :/ "all"

data Person = Person { name    :: Text
                     , age     :: Age
                     , address :: Text
                     } deriving (Show, Eq, ToJSON, FromJSON, Generic)

newtype Age = Age { getAge :: Int }
            deriving (Show, Eq, ToJSON, FromJSON, Generic)

instance WebApi ClientSpec where
  type Apis ClientSpec = '[ Route '[GET] Persons ]

instance ApiContract ClientSpec GET Persons where
  type ApiOut GET Persons = [Person]

instance WebApiServer ClientSpecImpl where
  type ApiInterface ClientSpecImpl = ClientSpec

instance ApiHandler ClientSpecImpl GET Persons where
  handler _ _ = respond persons

persons :: [Person]
persons = [Person "foo" (Age 10) "10 1st baz"]

spec :: Spec
spec = withApp $ describe "Webapi client" $ do
  it "can create proper requests" $
    shouldReturn clientAct persons

clientAct :: IO [Person]
clientAct = do
  mgr <- newManager defaultManagerSettings
  let csett = ClientSettings { baseUrl           = "http://localhost:8080"
                             , connectionManager = mgr
                             }
  resp <- client csett (Request { pathParam   = ()
                                , queryParam  = ()
                                , formParam   = ()
                                , fileParam   = ()
                                , cookieIn    = ()
                                , headerIn    = ()
                                , requestBody = ()
                                } :: Request GET Persons
                       )
  case resp of
    Success _ d _ _ -> putStrLn "Success" >> return d
    Failure _       -> putStrLn "Failed"  >> return []

