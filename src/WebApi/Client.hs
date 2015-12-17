{-# LANGUAGE TypeFamilies, KindSignatures, MultiParamTypeClasses, DataKinds, FlexibleContexts, GADTs, TypeOperators, PolyKinds, UndecidableInstances, FlexibleInstances, DefaultSignatures, ScopedTypeVariables, ConstraintKinds, TemplateHaskell, OverloadedStrings, LambdaCase #-}
module WebApi.Client
       ( ClientSettings (..)
       , fromClientResponse
       , toClientRequest
       , client  
       ) where

import           Data.Proxy
import           Data.List (find)
import           Data.Either (isRight)
import qualified Data.Text as T (pack)
import           Data.Text.Encoding (decodeUtf8)
import           WebApi.Method
import           WebApi.Param
import qualified Network.HTTP.Client as HC
import qualified Network.HTTP.Client.MultipartFormData as HC
import           Network.HTTP.Types hiding (Query)
import           WebApi.Contract
import           WebApi.Internal
import           WebApi.ContentTypes
import           Data.ByteString (ByteString)
import           Network.HTTP.Media (mapContentMedia)

data ClientSettings = ClientSettings { baseUrl           :: String
                                     , connectionManager :: HC.Manager
                                     }

fromClientResponse :: forall m r.( FromParam (HeaderOut m r) 'Header
                              , Decodings (ContentTypes m r) (ApiOut m r)
                              , CookieOut m r ~ ()
                             ) => HC.Response HC.BodyReader -> IO (Response m r)
fromClientResponse hcResp = do
  let status   = HC.responseStatus hcResp
      hdrsOut  = HC.responseHeaders hcResp
      respBody = HC.responseBody hcResp
      respHdr  = fromHeader hdrsOut :: Validation [ParamErr] (HeaderOut m r)
  respBodyBS <- respBody
  return $ case statusIsSuccessful status of
    True  -> case Success <$> pure status
                         <*> (Validation $ toParamErr $ decode (undefined :: Request m r) respBodyBS) -- eitherDecodeStrict respBodyBS)
                         <*> respHdr
                         <*> pure () of
      Validation (Right success) -> success
      Validation (Left errs) -> Failure $ Right $ Prelude.error $ show errs
    False -> Failure $ Right $ (OtherError $ T.pack $ show status)
    where toParamErr :: Either String a -> Either [ParamErr] a
          toParamErr (Left _str) = Left []
          toParamErr (Right r)  = Right r

          decode :: ( Decodings (ContentTypes m r) a
                   ) => apiRes m r -> ByteString -> Either String a
          decode r o = case getContentType hcResp of
            Just ctype -> let decs = decodings (reproxy r) o
                          in maybe (firstRight (map snd decs)) id (mapContentMedia decs ctype) -- mimeRender (Proxy :: Proxy JSON) o
            Nothing    -> firstRight (map snd (decodings (reproxy r) o))

          reproxy :: apiRes m r -> Proxy (ContentTypes m r)
          reproxy = const Proxy

          firstRight :: [Either String b] -> Either String b
          firstRight = maybe (Left "Couldn't find matching Content-Type") id . find isRight

toClientRequest :: forall m r.( ToParam (PathParam m r) 'PathParam
                          , ToParam (QueryParam m r) 'QueryParam
                          , ToParam (FormParam m r) 'FormParam
                          , ToParam (HeaderIn m r) 'Header
                          , ToParam (FileParam m r) 'FileParam
                          , SingMethod m
                          , MkPathFormatString r  
                          ) => HC.Request -> Request m r -> IO HC.Request
toClientRequest clientReq req = do
  let cReq' = clientReq
              { HC.method = singMethod (Proxy :: Proxy m)
              , HC.path = uriPath
              , HC.requestHeaders = toHeader $ headerIn req
              }
      cReqQP = HC.setQueryString queryPar cReq'
      cReqUE = if Prelude.null formPar
               then cReqQP
               else HC.urlEncodedBody formPar cReqQP
      cReqMP = if Prelude.null filePar
               then return cReqUE
               else HC.formDataBody (Prelude.map (\(pname, fpath) -> HC.partFileSource (decodeUtf8 pname) fpath) filePar) cReqUE
  cReqMP
  where queryPar = toQueryParam $ queryParam req
        formPar = toFormParam $ formParam req
        filePar = toFileParam $ fileParam req
        uriPath = case HC.path clientReq of
          "/" -> renderPaths (pathParam req) req
          _   -> HC.path clientReq `mappend` renderPaths (pathParam req) req

client :: ( CookieOut m r ~ ()
          , ToParam (PathParam m r) 'PathParam
          , ToParam (QueryParam m r) 'QueryParam
          , ToParam (FormParam m r) 'FormParam
          , ToParam (HeaderIn m r) 'Header
          , ToParam (FileParam m r) 'FileParam
          , FromParam (HeaderOut m r) 'Header
          , Decodings (ContentTypes m r) (ApiOut m r)
          , SingMethod m
          , MkPathFormatString r
          ) => ClientSettings -> Request m r -> IO (Response m r)
client sett req = do
  cReqInit <- HC.parseUrl (baseUrl sett)
  cReq <- toClientRequest cReqInit req
  HC.withResponse cReq (connectionManager sett) fromClientResponse
