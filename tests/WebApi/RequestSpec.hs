{-# LANGUAGE MultiParamTypeClasses, TypeFamilies, OverloadedStrings, DataKinds, TypeOperators, TypeSynonymInstances, FlexibleInstances, DeriveGeneric #-}
module WebApi.RequestSpec (spec) where

import WebApi
import WebApi.Internal
import Data.Text (Text)
import Network.HTTP.Types.Method (methodPost, methodPut, methodDelete, methodHead
                                 ,methodPatch, methodConnect, methodTrace, methodOptions)
import Test.Hspec
import Test.Hspec.Wai (with, request, shouldRespondWith, postHtmlForm, matchStatus)
import qualified Test.Hspec.Wai as Hspec.Wai (get)
import GHC.Generics
import Data.ByteString (ByteString, drop)
import Data.Monoid ((<>))
import Data.List (foldl')
import qualified Network.Wai as Wai
import Data.CaseInsensitive (mk)
import Network.HTTP.Types.Header (Header, hCookie)
import Prelude hiding (FilePath)

withApp :: SpecWith Wai.Application -> Spec
withApp = with (return reqSpecApp)

reqSpecApp :: Wai.Application
reqSpecApp = serverApp serverSettings ReqSpecImpl

data ReqSpec     = ReqSpec
data ReqSpecImpl = ReqSpecImpl

data Profile = Profile { name :: Text, age :: Age , desc :: Text }
             deriving (Show, Eq, Generic)

newtype Age = Age { unAge :: Int }
            deriving (Show, Eq, Generic)

data QP = QP { qp1 :: Int , qp2 :: Maybe Bool, qp3 :: Either Text Double }
        deriving (Show, Eq, Generic)

data FoP = FoP { fop :: ByteString }
         deriving (Show, Eq, Generic)

data CP = CP { cp :: Bool }
         deriving (Show, Eq, Generic)

data HP =  HP1 { hp1 :: Int }
         | HP2 { hp2 :: Bool }
         deriving (Show, Eq, Generic)

data FiP = FiP { fip :: FileInfo }
         deriving (Show, Eq, Generic)

instance FromParam QP 'QueryParam where
instance FromParam FoP 'FormParam where   
instance FromParam CP 'Cookie where
instance FromHeader HP where
instance FromParam FiP 'FileParam where

instance ToParam QP 'QueryParam where
instance ToParam FoP 'FormParam where   
instance ToParam CP 'Cookie where
instance ToHeader HP where
instance ToParam FiP 'FileParam where
  
type ApiR = Static "api"
type QuickCheckR = Static "autogen"

instance WebApi ReqSpec where
  type Version ReqSpec = MajorMinor '(0, 1)
  type Apis    ReqSpec = '[ Route GET               ApiR
                          , Route POST              ApiR
                          , Route PUT               ApiR  
                          , Route DELETE            ApiR
                          , Route HEAD              ApiR
                          , Route PATCH             ApiR
                          , Route TRACE             ApiR
                          , Route CONNECT           ApiR
                          , Route (CUSTOM ("TEST")) ApiR
                          -- , Route POST              QuickCheckR  
                          ]



instance WebApiImplementation ReqSpecImpl where
  type ApiInterface ReqSpecImpl = ReqSpec

instance ApiContract ReqSpec GET ApiR where
  type QueryParam GET ApiR  = QP
  type ApiOut GET ApiR      = ()

instance ApiContract ReqSpec POST ApiR where
  type QueryParam POST ApiR  = QP
  -- type FileParam POST ApiR   = FiP
  -- type HeaderIn POST ApiR    = HP
  -- type CookieIn POST ApiR    = CP
  type FormParam POST ApiR   = FoP
  type ApiOut POST ApiR      = ()
  type ApiErr POST ApiR      = Text

instance ApiContract ReqSpec PUT ApiR where
  -- type QueryParam PUT ApiR  = QP
  type HeaderIn PUT ApiR    = HP
  type CookieIn PUT ApiR    = CP
  -- type FormParam PUT ApiR   = FoP
  type ApiOut PUT ApiR      = ()
  type ApiErr PUT ApiR      = Text

instance ApiContract ReqSpec DELETE ApiR where
  -- type QueryParam DELETE ApiR  = QP
  -- type HeaderIn DELETE ApiR    = HP
  -- type CookieIn DELETE ApiR    = CP
  -- type FormParam DELETE ApiR   = FoP
  type ApiOut DELETE ApiR      = ()

instance ApiContract ReqSpec HEAD ApiR where
  -- type QueryParam HEAD ApiR = QP
  type ApiOut HEAD ApiR     = ()

instance ApiContract ReqSpec PATCH ApiR where
  -- type QueryParam PATCH ApiR  = QP
  -- type HeaderIn PATCH ApiR    = HP
  -- type CookieIn PATCH ApiR    = CP
  -- type FormParam PATCH ApiR   = FoP
  type ApiOut PATCH ApiR      = ()

instance ApiContract ReqSpec TRACE ApiR where
  -- type QueryParam TRACE ApiR  = QP
  -- type HeaderIn TRACE ApiR    = HP
  -- type CookieIn TRACE ApiR    = CP
  -- type FormParam TRACE ApiR   = FoP
  type ApiOut TRACE ApiR      = ()

instance ApiContract ReqSpec CONNECT ApiR where
  -- type QueryParam CONNECT ApiR  = QP
  -- type HeaderIn CONNECT ApiR    = HP
  -- type CookieIn CONNECT ApiR    = CP
  -- type FormParam CONNECT ApiR   = FoP
  type ApiOut CONNECT ApiR      = ()

instance ApiContract ReqSpec (CUSTOM "TEST") ApiR where
  -- type QueryParam (CUSTOM "TEST") ApiR = QP
  -- type HeaderIn (CUSTOM "TEST") ApiR   = HP
  -- type CookieIn (CUSTOM "TEST") ApiR   = CP
  -- type FormParam (CUSTOM "TEST") ApiR  = FoP
  type ApiOut (CUSTOM "TEST") ApiR     = ()

instance ApiHandler ReqSpecImpl (CUSTOM "TEST") ApiR where
  handler _ _ = respond ()
instance ApiHandler ReqSpecImpl CONNECT ApiR where
  handler _ _ = respond ()
instance ApiHandler ReqSpecImpl TRACE ApiR where
  handler _ _ = respond ()
instance ApiHandler ReqSpecImpl HEAD ApiR where
  handler _ _ = respond ()
instance ApiHandler ReqSpecImpl PATCH ApiR where
  handler _ _ = respond ()
instance ApiHandler ReqSpecImpl GET ApiR where
  handler _ _ = respond ()
instance ApiHandler ReqSpecImpl POST ApiR where
  handler _ _ = respond ()
instance ApiHandler ReqSpecImpl PUT ApiR where
  handler _ _ = respond ()
instance ApiHandler ReqSpecImpl DELETE ApiR where
  handler _ _ = respond ()

formHeaders :: [(ByteString, ByteString)] -> [(ByteString, ByteString)] -> [Header]
formHeaders headerKvs cookieKvs = map toHeader headerKvs <> [toCookie cookieKvs]
  where toHeader (k, v) = (mk k, v)
        toCookie kvs    = (hCookie, serializeCookie kvs)

        serializeCookie = foldl' (\acc (k, v) -> acc <> ";" <> k <> "=" <> v) ""
        
spec :: Spec
spec = withApp $ describe "WebApi request with payload" $ do
  context "GET Request" $ do
    it "should be 200 ok" $ do
      Hspec.Wai.get "api?qp1=5&qp2=True&qp3.Right=15.60" `shouldRespondWith` 200
  context "POST Request" $ do
    it "should be 200 ok" $ do
      postHtmlForm "api?qp1=5&qp2=True&qp3.Left=foo" [("fop", "foobar")] `shouldRespondWith` 200
  context "PUT Request" $ do
    it "should be 200 ok" $ do
      let headers = formHeaders [("HP1.hp1", "5")] [("cp", "True")]
      request methodPut "api" headers "" `shouldRespondWith` "[]" { matchStatus = 200 }
  context "DELETE Request" $ do
    it "should be 200 ok" $ do
      request methodDelete "api" [] "" `shouldRespondWith` "[]" { matchStatus = 200 }
  context "HEAD Request" $ do
    it "should be 200 ok" $ do
      request methodHead "api" [] "" `shouldRespondWith` "[]" { matchStatus = 200 }
  context "PATCH Request" $ do
    it "should be 200 ok" $ do
      request methodPatch "api" [] "" `shouldRespondWith` "[]" { matchStatus = 200 }
  context "TRACE Request" $ do
    it "should be 200 ok" $ do
      request methodTrace "api" [] "" `shouldRespondWith` "[]" { matchStatus = 200 }
  context "OPTIONS Request" $ do
    it "should be 200 ok" $ do
      request methodOptions "api" [] "" `shouldRespondWith` "[]" { matchStatus = 200 }
  context "CONNECT Request" $ do
    it "should be 200 ok" $ do
      request methodConnect "api" [] "" `shouldRespondWith` "[]" { matchStatus = 200 }
  context "CUSTOM TEST Request" $ do
    it "should be 200 ok" $ do
      request "TEST" "api" [] "" `shouldRespondWith` "[]" { matchStatus = 200 }
  context "When request is incomplete" $ do
    it "should be 400 ok" $ do
      let headers = formHeaders [("HP2.hp2", "True")] []
      request methodPut "api" headers "" `shouldRespondWith` "\"[NotFound \\\"cp\\\"]\"" { matchStatus = 400 }
