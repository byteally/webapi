{-|
Module      : WebApi.Mock
License     : BSD3
Stability   : experimental

Once a contract is defined for a web api, a mock server and client for it can be obtained. 'Arbitrary' instances of the data types used in 'Request' and 'Response' is used to generate the request and response. Note that if a different mocking behaviour is required, it is easy enough to write a different implementation. Please take a look at the reference implementation of 'MockServer' for details. 
-}

{-# LANGUAGE TypeFamilies, FlexibleInstances, MultiParamTypeClasses, UndecidableInstances, DataKinds, FlexibleContexts, ScopedTypeVariables, DeriveGeneric #-}
module WebApi.Mock
       (
         -- * Mock Server
         mockServerSettings
       , mockResponse
       , mockServer
       , MockServer (..)
       , MockServerSettings (..)
       , MockServerException (..)  
       , GenerateResponse (..)
        
       -- * Mock Client
       , mockClient 
       ) where

import Control.Exception
import Data.Proxy (Proxy (..))
import Data.Text.Encoding (decodeUtf8)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import Network.HTTP.Types (Status, ok200)
import qualified Network.Wai as Wai
import WebApi.Internal
import WebApi.Contract
import WebApi.Server
import Test.QuickCheck (Arbitrary, generate, arbitrary)

data Route' m r = Route'

-- | Datatype representing a mock server. The parameterization over `p` allows it to be a mock server for any `p`.
newtype MockServer p = MockServer { mockServerSett :: MockServerSettings }
                     deriving (Eq, Show)

-- | Determine the data constructor of `Response` to be generated in `mockServer`.
data GenerateResponse = GenerateSuccess
                      | GenerateApiError Status
                      | GenerateOtherError
                      deriving (Eq, Show)

-- | Settings related to mock server.
data MockServerSettings = MockServerSettings { genResponse :: GenerateResponse }
                        deriving (Eq, Show)

-- | Default mock server settings. 
mockServerSettings :: MockServerSettings
mockServerSettings = MockServerSettings GenerateSuccess

instance (WebApi p) => WebApiImplementation (MockServer p) where
  type ApiInterface (MockServer p) = p

instance ( ApiContract p m r
         , Arbitrary (ApiOut m r)
         , Arbitrary (ApiErr m r) 
         , Arbitrary (HeaderOut m r)
         , Arbitrary (CookieOut m r)
         , Typeable m
         , Typeable r 
         ) => ApiHandler (MockServer p) m r where
  handler mock _ = mockResponse (Route' :: Route' m r) ((mockServerSett . unTagged) mock)

-- | Create a mock response from endpoint information and `MockServerSettings`
mockResponse :: forall route m r. ( Arbitrary (ApiOut m r)
                              , Arbitrary (HeaderOut m r)
                              , Arbitrary (CookieOut m r)
                              , Arbitrary (ApiErr m r)
                              , Typeable m
                              , Typeable r 
                              ) => route m r -> MockServerSettings -> IO (Response m r)
mockResponse _ msett = case genResponse msett of
  GenerateSuccess       -> mockSuccess
  GenerateApiError   st -> mockApiError st
  GenerateOtherError    -> mockOtherError

  where mockSuccess :: IO (Response m r)
        mockSuccess = do
          aout <- generate arbitrary
          hout <- generate arbitrary
          cout <- generate arbitrary
          respondWith ok200 aout hout cout

        mockApiError :: Status -> IO (Response m r)
        mockApiError status = do
          aerr <- generate arbitrary
          herr <- generate arbitrary
          cerr <- generate arbitrary
          raiseWith status aerr herr cerr

        mockOtherError :: IO (Response m r)
        mockOtherError = do
          oerr <- generate arbitrary
          return (Failure (Right (OtherError (SomeException $ MockServerException oerr))))

-- | Datatype representing a mock exception. This exception will be put inside `OtherError`.
data MockServerException = MockServerException { exceptionMsg :: String }
                         deriving (Show, Generic)

instance Exception MockServerException

-- | Create a mock server.
mockServer :: (Router (MockServer p) (Apis p) '(CUSTOM "", '[])) => ServerSettings -> MockServer p -> Wai.Application
mockServer = serverApp

-- | Create a mock client.
mockClient :: (  Arbitrary (PathParam m r)
               , Arbitrary (QueryParam m r)
               , Arbitrary (FormParam m r)
               , Arbitrary (FileParam m r)
               , Arbitrary (HeaderIn m r)
               , Arbitrary (CookieIn m r)
               , SingMethod m
               ) => route m r -> IO (Request m r)
mockClient r =
  Req  <$> generate arbitrary
       <*> generate arbitrary
       <*> generate arbitrary
       <*> generate arbitrary
       <*> generate arbitrary
       <*> generate arbitrary
       <*> pure (decodeUtf8 $ singMethod (reproxy r))

  where reproxy :: route m r -> Proxy m
        reproxy = const Proxy
