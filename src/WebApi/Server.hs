{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
module WebApi.Server
       ( respond
       , respondWith
       , raise
       , raiseWith
       , toApplication
       , fromWaiRequest
       , toWaiResponse
       , serverApp
       , serverSettings
       , ServerSettings
       , Server (..)
       , ApiInterface
       , ApiException (..)  
       , module WebApi.Router
       ) where

import           Control.Exception
import           Control.Monad.Catch
import           Data.Proxy
import           Data.Typeable
import           Network.HTTP.Types hiding (Query)
import qualified Network.Wai as Wai
import           WebApi.Contract
import           WebApi.Internal
import           WebApi.Router


respond :: ( Monad handM
           , (HeaderOut m r) ~ ()
           , (CookieOut m r) ~ ()
           ) => ApiOut m r
             -> handM (Response m r)
respond out = respondWith ok200 out () ()

respondWith :: ( Monad handM
                ) => Status
                  -> ApiOut m r
                  -> HeaderOut m r
                  -> CookieOut m r
                  -> handM (Response m r)
respondWith status out hdrs cook = return $ Success status out hdrs cook

raise :: ( Monad handM
         , MonadThrow handM
         , Typeable m
         , Typeable r
         ) => Status
           -> ApiErr m r
           -> handM (Response m r)
raise status errs = raiseWith' (ApiError status errs Nothing Nothing)

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

raiseWith' :: ( Monad handM
              , MonadThrow handM
              , Typeable m
              , Typeable r  
             ) => ApiError m r
               -> handM (Response m r)
raiseWith' = throw . ApiException

serverApp :: ( iface ~ (ApiInterface server)
             , HandlerM iface ~ IO
             , Router server (Apis iface) '(CUSTOM "", '[])
             ) => ServerSettings -> server -> Wai.Application
serverApp _ server = toApplication $ router (apis server) server
  where apis :: server -> Proxy (Apis (ApiInterface server))
        apis = const Proxy
