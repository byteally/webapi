{-|
Module      : WebApi.Server
License     : BSD3
Stability   : experimental

Provides the implementation of web api. Given a contract, an implementation of the web api can be provided by using 'WebApiServer' and 'ApiHandler'. 'WebApiServer' has the information pertaining to web api as a whole. 'ApiHandler' provides a way to write the handler for a particular API end point.

Comparing with the "WebApi.Contract", 'WebApi' and 'ApiContract' has the same relationship as 'WebApiServer' and 'ApiHandler'.
-}

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE ExplicitForAll        #-}
{-# LANGUAGE TypeOperators         #-}
module WebApi.Server
       (
       -- * Creating a WAI application
         serverApp
       , serverSettings
       , nestedServerApp
       , nestedApi
       , apiComponent
       , mount
       , ServerSettings

       -- * Implementation of Api
       , ApiHandler (..)
       , ApiException (..)
       , WebApiServer (..)
       , WebApiRequest
       , rawRequest
       , respond
       , respondWith
       , raise
       , raiseWith

       -- * Wrapping and unwrapping a 'Tagged'
       , unTagged
       , toTagged

       -- * Routing
       , module WebApi.Router
       ) where

import           Control.Monad.Catch
import           Data.Proxy
import           Data.Typeable
import           Network.HTTP.Types hiding (Query)
import qualified Network.Wai as Wai
import           WebApi.Contract
import           WebApi.Internal
import           WebApi.Router
import           GHC.TypeLits

-- | Creates a successful response from its components. It is assumed that 'HeaderOut' and 'CookieOut' has default definitions.
respond :: ( Monad handM
           , (HeaderOut m r) ~ ()
           , (CookieOut m r) ~ ()
           ) => ApiOut m r
             -> handM (Response m r)
respond out = respondWith ok200 out () ()

-- | Creates a successful response from its components.
{-# WARNING respondWith "The status argument is not used, it will be fetched from `ResponseCodes` " #-}
respondWith :: ( Monad handM
                ) => Status
                  -> ApiOut m r
                  -> HeaderOut m r
                  -> CookieOut m r
                  -> handM (Response m r)
respondWith _status out hdrs cook = do
  let responseCode = getResponseCode out
  return $ Success status out hdrs cook

-- | This function short circuits returning an `ApiError`.It is assumed that 'HeaderOut' and 'CookieOut' has default definitions.
raise :: ( MonadThrow handM
         , Typeable m
         , Typeable r
         ) => Status
           -> ApiErr m r
           -> handM (Response m r)
raise status errs = raiseWith' (ApiError status errs Nothing Nothing)

-- | This function short circuits returning an `ApiError`.
raiseWith :: ( MonadThrow handM
              , Typeable m
              , Typeable r
             ) => Status
               -> ApiErr m r
               -> HeaderOut m r
               -> CookieOut m r
               -> handM (Response m r)
raiseWith status errs hdrs cook = raiseWith' (ApiError status errs (Just hdrs) (Just cook))

raiseWith' :: ( MonadThrow handM
              , Typeable m
              , Typeable r  
             ) => ApiError m r
               -> handM (Response m r)
raiseWith' = throwM . ApiException

-- | Create a WAI application from the information specified in `WebApiServer`, `WebApi`, `ApiContract` and `ApiHandler` classes.
serverApp :: ( iface ~ (ApiInterface server)
             , Router server (Apis iface) '(CUSTOM "", '[])
             ) => ServerSettings -> server -> Wai.Application
serverApp _ server = toApplication $ router (apis server) server
  where apis :: server -> Proxy (Apis (ApiInterface server))
        apis = const Proxy


nestedServerApp ::
  ( Router (NestedApplication (app: apps)) 'NestedR '(CUSTOM "", '[])
  ) => NestedApplication (app: apps) -> Wai.Application
nestedServerApp apps = toApplication $ router (Proxy :: Proxy 'NestedR) apps

mount :: forall (ns :: Symbol) c apps.
         ApiComponent c
      -> NestedApplication apps
      -> NestedApplication ('(ns, ApiInterface c) ': apps)
mount (ApiComponent comp) (NestedApplication apps) = NestedApplication (comp:apps)

apiComponent :: ( Router server (Apis (ApiInterface server)) '(CUSTOM "", '[])
                ) => server -> ApiComponent server
apiComponent serv = ApiComponent $ serverApp ServerSettings serv
