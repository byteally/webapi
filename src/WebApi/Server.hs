{-|
Module      : WebApi.Server
License     : BSD3
Stability   : experimental
-}

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
module WebApi.Server
       (
       -- * Creating a WAI application  
         serverApp
       , serverSettings
       , ServerSettings

       -- * Implementation of Api 
       , ApiHandler (..)
       , ApiException (..)
       , WebApiImplementation (..)  
       , respond
       , respondWith
       , raise
       , raiseWith

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

-- | Creates a successful response from its components. It is assumed that `HeaderOut m r` and `CookieOut m r` has default definitions.
respond :: ( Monad handM
           , (HeaderOut m r) ~ ()
           , (CookieOut m r) ~ ()
           ) => ApiOut m r
             -> handM (Response m r)
respond out = respondWith ok200 out () ()

-- | Creates a successful response from its components.
respondWith :: ( Monad handM
                ) => Status
                  -> ApiOut m r
                  -> HeaderOut m r
                  -> CookieOut m r
                  -> handM (Response m r)
respondWith status out hdrs cook = return $ Success status out hdrs cook

-- | This function short circuits returning an `ApiError`.It is assumed that `HeaderOut m r` and `CookieOut m r` has default definitions.
raise :: ( MonadThrow handM
         , Typeable m
         , Typeable r
         ) => Status
           -> ApiErr m r
           -> handM (Response m r)
raise status errs = raiseWith' (ApiError status errs Nothing Nothing)

-- | This function short circuits returning an `ApiError`.
raiseWith :: ( Monad handM
              , MonadThrow handM
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

-- | Create a WAI application from the information specified in `WebApiImplementation`, `WebApi`, `ApiContract` and `ApiHandler` classes.
serverApp :: ( iface ~ (ApiInterface server)
             , Router server (Apis iface) '(CUSTOM "", '[])
             ) => ServerSettings -> server -> Wai.Application
serverApp _ server = toApplication $ router (apis server) server
  where apis :: server -> Proxy (Apis (ApiInterface server))
        apis = const Proxy
