{-# LANGUAGE TypeFamilies, KindSignatures, MultiParamTypeClasses, DataKinds, FlexibleContexts #-}
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
       , module WebApi.Router
       ) where

import           Data.Proxy
import           Http.Method
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
         , (HeaderOut m r) ~ ()
         , (CookieOut m r) ~ ()
         ) => Status
           -> ApiErr m r
           -> handM (Response m r)
raise status errs = raiseWith status errs () ()

raiseWith :: ( Monad handM
          --   , MonadThrow handM -- TODO: Short Circuit
             ) => Status
               -> ApiErr m r
               -> HeaderOut m r
               -> CookieOut m r
               -> handM (Response m r)
raiseWith status errs hdrs cook = return $ Failure
                                          $ Left
                                          $ ApiError status errs hdrs cook

serverApp :: ( iface ~ (ApiInterface server)
             , HandlerM iface ~ IO
             , Router server (Apis iface) '(CUSTOM "", '[])
             ) => ServerSettings -> server -> Wai.Application
serverApp _ server = toApplication $ router (apis server) server
  where apis :: server -> Proxy (Apis (ApiInterface server))
        apis = const Proxy
