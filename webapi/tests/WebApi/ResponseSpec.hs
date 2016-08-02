{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# LANGUAGE MultiParamTypeClasses, TypeFamilies, OverloadedStrings, DataKinds, TypeOperators, TypeSynonymInstances, FlexibleInstances, DeriveGeneric #-}

module WebApi.ResponseSpec (spec) where

import GHC.Generics
import WebApi
import Test.Hspec
import qualified Network.Wai as Wai
import Test.Hspec.Wai (with, get, request, shouldRespondWith, matchStatus, (<:>), matchHeaders)
import Network.HTTP.Media.MediaType
import Network.HTTP.Types
import Data.Text
import Data.Aeson (ToJSON (..))

withApp :: SpecWith Wai.Application -> Spec
withApp = with (return respSpecApp)

respSpecApp :: Wai.Application
respSpecApp = serverApp serverSettings RespSpecImpl

data RespSpec
data RespSpecImpl = RespSpecImpl

data Out = Out { out :: Text }
         deriving (Show, Eq, Generic) 
data HOut = HOut { hOut :: Text }
         deriving (Show, Eq, Generic) 
data COut = COut { cOut :: Text }
         deriving (Show, Eq, Generic)
data Err = Err { err :: Text }
         deriving (Show, Eq, Generic)

instance ToJSON Err
instance ToJSON Out
instance ToHeader HOut

instance ToParam 'Cookie COut


instance ParamErrToApiErr Err where
  toApiErr = const (Err "fail")

type ApiResp        = Static "apiresp"
type ApiWithHeaders = Static "apih"
type ApiWithError   = Static "apierror"
type TextCType      = Static "text"
type LazyEncoding   = Static "lazyencoding"

instance WebApi RespSpec where
  type Apis    RespSpec = '[ Route '[GET] ApiResp
                           , Route '[GET] ApiWithHeaders 
                           , Route '[GET] ApiWithError 
                           , Route '[GET] TextCType
                           , Route '[GET] LazyEncoding]

instance WebApiImplementation RespSpecImpl where
  type ApiInterface RespSpecImpl = RespSpec
  type HandlerM     RespSpecImpl = IO

instance ApiContract RespSpec GET ApiResp where
  type ApiOut GET ApiResp = Out

instance ApiContract RespSpec GET ApiWithHeaders where
  type ApiOut    GET ApiWithHeaders = Out
  type HeaderOut GET ApiWithHeaders = HOut
  type CookieOut GET ApiWithHeaders = COut

instance ApiContract RespSpec GET ApiWithError where
  type ApiOut    GET ApiWithError = Out
  type ApiErr    GET ApiWithError = Err

instance ApiContract RespSpec GET TextCType where
  type ApiOut       GET TextCType = Text
  type ApiErr       GET TextCType = Text
  type ContentTypes GET TextCType = '[PlainText]

instance ApiContract RespSpec GET LazyEncoding where
  type ApiOut       GET LazyEncoding = Out
  type ContentTypes GET LazyEncoding = '[DummyCType, JSON]

instance ApiHandler RespSpecImpl GET ApiResp where
  handler _ _ = respond (Out "Done") 

instance ApiHandler RespSpecImpl GET ApiWithHeaders where
  handler _ _ = respondWith status200 (Out "Done") (HOut "header") (COut "cookie")  

instance ApiHandler RespSpecImpl GET ApiWithError where
  handler _ _ = do
    -- raise should short circuit
    _ <- (raise status500 (Err "fail") :: IO (Response GET ApiWithError)) 
    -- raiseWith' _ -- (ApiError status500 (Err "fail") Nothing Nothing) -- :: ApiError GET ApiWithError)
    -- which means respond will never get called
    respond (Out "Done")

instance ApiHandler RespSpecImpl GET TextCType where
  handler _ _ = respond "plaintext"

instance ApiHandler RespSpecImpl GET LazyEncoding where
  handler _ _ = respond (Out "Done")


data DummyCType
instance Accept DummyCType where
  contentType _ = "application" // "dummy"

instance Encode DummyCType a where
  encode _ = error "Dummy content type not implemented"

spec :: Spec
spec = withApp $ describe "WebApi response" $ do
  context "Simple Response" $ do
    it "should be 200 ok" $ do
      get "apiresp" `shouldRespondWith` 200
  context "Response with response header and cookies" $ do
    it "should be 200 ok" $ do
      get "apih" `shouldRespondWith` "{\"out\":\"Done\"}" { matchHeaders = [ "hOut" <:> "header"
                                                                           , "Set-Cookie" <:> "cOut=cookie"
                                                                           , "Content-Type" <:> "application/json"]
                                                          , matchStatus  = 200 }
  context "Response with api error" $ do
    it "should be 500 ok" $ do
      get "apierror" `shouldRespondWith` 500
  context "Response with text as content type" $ do
    it "should be 200 ok" $ do
      get "text" `shouldRespondWith` "plaintext" { matchHeaders = ["Content-Type" <:> "text/plain;charset=utf-8"]
                                                 , matchStatus  = 200 }
  context "Response should get encoded lazily" $ do
    it "should be 200 ok" $ do
      let h = [(hAccept, "application/json")]
      request methodGet "lazyencoding" h "" `shouldRespondWith` "{\"out\":\"Done\"}" { matchHeaders = ["Content-Type" <:> "application/json"]
                                                                                     , matchStatus  = 200 }

