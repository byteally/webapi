{-|
Module      : WebApi.Client
License     : BSD3
Stability   : experimental
-}

{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DefaultSignatures     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}
module WebApi.Client
       (
       -- * Client related functions
         client
       , fromClientResponse
       , toClientRequest
       -- * Types  
       , ClientSettings (..)
       , UnknownClientException
       ) where

import           Control.Exception
import           Data.ByteString                       (ByteString)
import           Data.Either                           (isRight)
import           Data.List                             (find)
import           Data.Proxy
import           Data.Text.Encoding                    (decodeUtf8)
import           Data.Typeable                         (Typeable) 
import qualified Network.HTTP.Client                   as HC
import qualified Network.HTTP.Client.MultipartFormData as HC
import           Network.HTTP.Media                    (mapContentMedia)
import           Network.HTTP.Types                    hiding (Query)
import           Network.Wai.Parse                     (fileContent)
import           WebApi.ContentTypes
import           WebApi.Contract
import           WebApi.Internal
import           WebApi.Param

-- | Datatype representing the settings related to Client
data ClientSettings = ClientSettings { baseUrl           :: String -- ^ base url of the API being called
                                     , connectionManager :: HC.Manager -- ^ connection manager for the connection
                                     }

data Proxy1 m r = Proxy1

-- | Creates the 'Response' type from the response body.
fromClientResponse :: forall m r.( FromHeader (HeaderOut m r)
                              , ParamErrToApiErr (ApiErr m r)
                              , Decodings (ContentTypes m r) (ApiOut m r)
                              , Decodings (ContentTypes m r) (ApiErr m r)  
                             ) => HC.Response HC.BodyReader -> IO (Response m r)
fromClientResponse hcResp = do
  let status   = HC.responseStatus hcResp
      hdrsOut  = HC.responseHeaders hcResp
      respBody = HC.responseBody hcResp
      respHdr  = fromHeader hdrsOut :: Validation [ParamErr] (HeaderOut m r)
      -- respCk   = fromCookie
  respBodyBS <- respBody
  return $ case statusIsSuccessful status of
    True  -> case Success <$> pure status
                         <*> (Validation $ toParamErr $ decode' (Proxy1 :: Proxy1 m r) respBodyBS)
                         <*> respHdr
                         <*> pure undefined of
      Validation (Right success) -> success
      Validation (Left errs) -> Failure $ Left $ ApiError status (toApiErr errs) Nothing Nothing
    False -> case ApiError
                  <$> pure status
                  <*> (Validation $ toParamErr $ decode' (Proxy1 :: Proxy1 m r) respBodyBS)
                  <*> (Just <$> respHdr)
                  -- TODO: Handle cookies
                  <*> pure Nothing of
               Validation (Right failure) -> (Failure . Left) failure
               Validation (Left _errs) -> Failure $ Right (OtherError (toException UnknownClientException))

    where toParamErr :: Either String a -> Either [ParamErr] a
          toParamErr (Left _str) = Left []
          toParamErr (Right r)  = Right r

          decode' :: ( Decodings (ContentTypes m r) a
                   ) => apiRes m r -> ByteString -> Either String a
          decode' r o = case getContentType hcResp of
            Just ctype -> let decs = decodings (reproxy r) o
                          in maybe (firstRight (map snd decs)) id (mapContentMedia decs ctype)
            Nothing    -> firstRight (map snd (decodings (reproxy r) o))

          reproxy :: apiRes m r -> Proxy (ContentTypes m r)
          reproxy = const Proxy

          firstRight :: [Either String b] -> Either String b
          firstRight = maybe (Left "Couldn't find matching Content-Type") id . find isRight

-- | Creates a request from the 'Request' type
toClientRequest :: forall m r.( ToParam (PathParam m r) 'PathParam
                          , ToParam (QueryParam m r) 'QueryParam
                          , ToParam (FormParam m r) 'FormParam
                          , ToHeader (HeaderIn m r)
                          , ToParam (FileParam m r) 'FileParam
                          , SingMethod m
                          , MkPathFormatString r
                          ) => HC.Request -> Request m r -> IO HC.Request
toClientRequest clientReq req = do
  let cReq' = clientReq
              { HC.method = singMethod (Proxy :: Proxy m)
              , HC.path = uriPath
              , HC.requestHeaders = toHeader $ headerIn req
              , HC.cookieJar = error "TODO: cookieJar"
              }
      cReqQP = HC.setQueryString queryPar cReq'
      cReqUE = if Prelude.null formPar
               then cReqQP
               else HC.urlEncodedBody formPar cReqQP
      cReqMP = if Prelude.null filePar
               then return cReqUE
               else HC.formDataBody (Prelude.map (\(pname, finfo) -> HC.partFileSource (decodeUtf8 pname) (fileContent finfo)) filePar) cReqUE
  cReqMP
  where queryPar = toQueryParam $ queryParam req
        formPar = toFormParam $ formParam req
        filePar = toFileParam $ fileParam req
        uriPath = renderUriPath (HC.path clientReq) (pathParam req) req

-- | Given a `Request` type, create the request and obtain a response. Gives back a 'Response'
client :: ( CookieOut m r ~ ()
          , ToParam (PathParam m r) 'PathParam
          , ToParam (QueryParam m r) 'QueryParam
          , ToParam (FormParam m r) 'FormParam
          , ToHeader (HeaderIn m r)
          , ToParam (FileParam m r) 'FileParam
          , FromHeader (HeaderOut m r)
          , Decodings (ContentTypes m r) (ApiOut m r)
          , Decodings (ContentTypes m r) (ApiErr m r)  
          , ParamErrToApiErr (ApiErr m r)
          , SingMethod m
          , MkPathFormatString r
          ) => ClientSettings -> Request m r -> IO (Response m r)
client sett req = do
  cReqInit <- HC.parseUrl (baseUrl sett)
  cReq <- toClientRequest cReqInit req
  HC.withResponse cReq (connectionManager sett) fromClientResponse

data UnknownClientException = UnknownClientException
                            deriving (Typeable, Show)

instance Exception UnknownClientException where
  
