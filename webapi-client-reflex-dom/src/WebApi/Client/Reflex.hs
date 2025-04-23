{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PolyKinds #-}
module WebApi.Client.Reflex where

import           Control.Exception
import           Control.Monad
import           Data.Bifunctor
import           Data.ByteString (ByteString)
import           Data.ByteString.Builder (toLazyByteString)
import qualified Data.ByteString.Lazy as LBS (ByteString, fromStrict, toStrict)
import qualified Data.CaseInsensitive as CI
import           Data.Either
import           Data.Kind
import           Data.List (find)
import qualified Data.Map as Map
import           Data.Proxy
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           GHCJS.DOM.File
import           GHCJS.DOM.FormData
import           Language.Javascript.JSaddle
import           Network.HTTP.Media (mapContentMedia, renderHeader)
import           Network.HTTP.Types as HT
import           Reflex.Dom.Core hiding (Request, Response)
import           WebApi.ContentTypes
import           WebApi.Contract as WebApi
import           WebApi.Param as WebApi
import           WebApi.Util
--import Control.Monad.IO.Class


type family NamespaceOf (r :: Type) where
  NamespaceOf (ns :// (r :: k)) = ns

client :: forall meth r t m.
  ( DomBuilder t m
  , MonadJSM (Performable m)
  , MonadJSM m
  , PerformEvent t m
  , TriggerEvent t m
  , WebApi (NamespaceOf r)
  , SingMethod meth
  , MkPathFormatString r
  , ToParam 'PathParam (PathParam meth r)
  , ToParam 'QueryParam (QueryParam meth r)
  , ToParam 'FormParam (FormParam meth r)
  , ToHeader (HeaderIn meth r)
  , ToParam 'FileParam (FileParam meth r)
  , ToParam 'Cookie (CookieIn meth r)
  , FromHeader (HeaderOut meth r)
  , CookieOut meth r ~ () -- TODO: Http-Only cookie cannot be read from JS
  , PartEncodings (RequestBody meth r)
  , Decodings (ContentTypes meth r) (ApiOut meth r)
  , Decodings (ContentTypes meth r) (ApiErr meth r)
  , ToHListRecTuple (StripContents (RequestBody meth r))
  ) => Event t (Request meth r) -> m (Event t (Response meth r))
client req = do
  host <- getLocationHost
  clientOrigin host req

clientOrigin :: forall meth r t m.
  ( DomBuilder t m
  , MonadJSM (Performable m)
  , MonadJSM m
  , PerformEvent t m
  , TriggerEvent t m
  , WebApi (NamespaceOf r)
  , SingMethod meth
  , MkPathFormatString r
  , ToParam 'PathParam (PathParam meth r)
  , ToParam 'QueryParam (QueryParam meth r)
  , ToParam 'FormParam (FormParam meth r)
  , ToHeader (HeaderIn meth r)
  , ToParam 'FileParam (FileParam meth r)
  , ToParam 'Cookie (CookieIn meth r)
  , FromHeader (HeaderOut meth r)
  , CookieOut meth r ~ () -- TODO: Http-Only cookie cannot be read from JS
  , PartEncodings (RequestBody meth r)
  , Decodings (ContentTypes meth r) (ApiOut meth r)
  , Decodings (ContentTypes meth r) (ApiErr meth r)
  , ToHListRecTuple (StripContents (RequestBody meth r))
  ) => Text -> Event t (Request meth r) -> m (Event t (Response meth r))
clientOrigin baseUrl reqEvt = do
  let
    mkXhrReq :: (MonadJSM jsm) => Request meth r -> jsm (XhrRequest Text)
    mkXhrReq req = do
      let
        meth = singMethod (Proxy :: Proxy meth)
        reqHeaders' = if T.null formPar
                      then case partEncMay of
                           Just (mt, _) -> (hContentType, renderHeader mt) : (toHeader $ headerIn req)
                           Nothing -> toHeader $ headerIn req
                      else (hContentType, "application/x-www-form-urlencoded") : (toHeader $ headerIn req)
        reqHeaders = Map.fromList $ fmap (\(k,v) -> ( T.decodeUtf8 $ CI.original k
                                                    , T.decodeUtf8 v)) reqHeaders'
        formPar = T.decodeUtf8 $ renderSimpleQuery False $ toFormParam $ formParam req
                -- TODO: Should qpar be Maybe
        reqUrl = WebApi.link (Res :: Resource meth r) (Just (T.encodeUtf8 baseUrl)) (pathParam req) (Just $ queryParam req)
        cts     = Proxy :: Proxy (RequestBody meth r)
        cts'    = Proxy :: Proxy (StripContents (RequestBody meth r))
        -- NOTE: Handles only single request body
        partEncMay = case partEncodings cts (toRecTuple cts' (requestBody req)) of
          [[partEnc']] -> Just partEnc'
          _ -> Nothing
        sendData = case partEncMay of
                     Just (_, b) -> T.decodeUtf8 $ LBS.toStrict $ toLazyByteString b
                     _ -> formPar

      -- TODO: Handle Files
      forM_ (toFileParam $ fileParam req) $ \(fname, _finfo) -> do
        formData <- newFormData Nothing
        appendBlob formData (T.decodeUtf8 fname) (undefined :: File) (Nothing :: Maybe Text)
        pure ()

      pure XhrRequest
         { _xhrRequest_method = T.decodeUtf8 meth
         , _xhrRequest_url = T.decodeUtf8 reqUrl
         , _xhrRequest_config = XhrRequestConfig
           { _xhrRequestConfig_headers = reqHeaders
           , _xhrRequestConfig_responseHeaders = AllHeaders -- Parses all headers, can be more refined OnlyHeaders
           , _xhrRequestConfig_sendData = sendData
           , _xhrRequestConfig_responseType = Nothing
           , _xhrRequestConfig_user = Nothing
           , _xhrRequestConfig_password = Nothing
           , _xhrRequestConfig_withCredentials = False
           }
         }
  xhrReq <- performEvent $ mkXhrReq <$> reqEvt
  xhrRes <- performRequestAsyncWithError xhrReq
  let
    getContentType :: ResponseHeaders -> Maybe ByteString
    getContentType = fmap snd . find ((== hContentType) . fst)

    fromClientResponse :: ( FromHeader (HeaderOut meth r)
                          , Decodings (ContentTypes meth r) (ApiOut meth r)
                          ) => XhrResponse -> Response meth r
    fromClientResponse resp =
      let
        status = mkStatus (fromIntegral $ _xhrResponse_status resp) (T.encodeUtf8 $ _xhrResponse_statusText resp)

        respHdrRaw :: ResponseHeaders
        respHdrRaw = fmap (bimap (CI.map T.encodeUtf8) T.encodeUtf8)
                     $ Map.toList $ _xhrResponse_headers resp
        respHdr = fromHeader $ respHdrRaw :: Validation [ParamErr] (HeaderOut meth r)

        toParamErr :: Either String a -> Either [ParamErr] a
        toParamErr (Left str) = Left [ParseErr "" (T.pack $ str)]
        toParamErr (Right r)   = Right r

        decode' :: ( Decodings (ContentTypes meth r) a
               ) => apiRes meth r -> LBS.ByteString -> Either String a
        decode' r o = case getContentType (respHdrRaw) of
          Just ctype -> let decs = decodings (reproxy r) o
                        in maybe (firstRight (map snd decs)) id (mapContentMedia decs ctype)
          Nothing    -> firstRight (map snd (decodings (reproxy r) o))

        reproxy :: apiRes meth r -> Proxy (ContentTypes meth r)
        reproxy = const Proxy

        firstRight :: [Either String b] -> Either String b
        firstRight = maybe (Left "Couldn't find matching Content-Type") id . find isRight

        -- TODO: Handle other xhr response case
        xhrResp = LBS.fromStrict $ T.encodeUtf8 $ maybe T.empty id  $ _xhrResponse_responseText resp

      in case WebApi.Success <$> pure status
               <*> (Validation $ toParamErr $ decode' (Res :: Resource meth r) xhrResp)
               <*> respHdr
               <*> pure () of
      Validation (Right success) -> success
      Validation (Left errs1) ->
        case ApiError
             <$> pure status
             <*> (Validation $ toParamErr $ decode' (Res :: Resource meth r) xhrResp)
             <*> (Just <$> respHdr)
             <*> (Just <$> (pure ())) of
          Validation (Right failure) -> (WebApi.Failure . Left) failure
          Validation (Left errs2) -> 
            let errs = case HT.statusCode status of
                  200 -> errs1
                  _ -> errs2
            in WebApi.Failure $ Right (OtherError (toException $ ApiErrParseFailException status $ T.intercalate "\n" $ fmap (T.pack . show) errs))

  pure $ ffor xhrRes $ \case
    Left e -> WebApi.Failure $ Right $ OtherError $ toException e
    Right r -> fromClientResponse r

data ContentDecodeException
  = ContentDecodeException
  deriving (Show, Eq)

instance Exception ContentDecodeException
