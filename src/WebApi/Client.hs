{-# LANGUAGE TypeFamilies, KindSignatures, MultiParamTypeClasses, DataKinds, FlexibleContexts, GADTs, TypeOperators, PolyKinds, UndecidableInstances, FlexibleInstances, DefaultSignatures, ScopedTypeVariables, ConstraintKinds, TemplateHaskell, OverloadedStrings, LambdaCase #-}
module WebApi.Client
       ( fromClientResponse
       , toClientRequest
       ) where

import           Data.Aeson (eitherDecodeStrict, FromJSON)
import           Data.Proxy
import           Data.Text.Encoding (decodeUtf8)
import           Http.Method
import           Http.Param
import qualified Network.HTTP.Client as HC
import qualified Network.HTTP.Client.MultipartFormData as HC
import           Network.HTTP.Types hiding (Query)
import           WebApi.Contract

fromClientResponse :: forall m r.( FromParam (HeaderOut m r) 'Header
                              , FromJSON (ApiOut m r)
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
                         <*> (Validation $ toParamErr $ eitherDecodeStrict respBodyBS)
                         <*> respHdr
                         <*> pure () of
      Validation (Right success) -> success
      Validation (Left errs) -> Failure $ Right $ Prelude.error $ show errs
    False -> Failure $ Right $ Prelude.error "TODO:"
    where toParamErr :: Either String a -> Either [ParamErr] a
          toParamErr (Left _str) = Left []
          toParamErr (Right r)  = Right r

toClientRequest :: forall m r.( ToParam (QueryParam m r) 'QueryParam
                          , ToParam (FormParam m r) 'FormParam
                          , ToParam (HeaderIn m r) 'Header
                          , ToParam (FileParam m r) 'FileParam
                          , SingMethod m
                          ) => HC.Request -> (Request m r) -> IO HC.Request
toClientRequest clientReq req = do
  let cReq' = clientReq
              { HC.method = singMethod (Proxy :: Proxy m)
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
