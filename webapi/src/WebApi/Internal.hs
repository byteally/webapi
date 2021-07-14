{-# LANGUAGE CPP                       #-}
{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DefaultSignatures         #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE KindSignatures            #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeOperators             #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE UndecidableInstances      #-}


#if __GLASGOW_HASKELL__ >= 800
{-# LANGUAGE UndecidableSuperClasses #-}
#endif

module WebApi.Internal where

import           Data.Text.Encoding                 (encodeUtf8Builder)
import           Control.Exception
import           Control.Monad.Catch                (MonadCatch)
import           Control.Monad.IO.Class             (MonadIO)
import           Control.Monad.Trans.Resource       (runResourceT,
                                                     withInternalState)
import           Data.ByteString                    (ByteString)
import           Data.ByteString.Builder            (toLazyByteString, Builder)
import           Data.ByteString.Lazy               as LBS (toStrict)
import           Data.List                          (find)
#if !(MIN_VERSION_base(4,8,0))
import           Data.Monoid                        ((<>))
#endif
import           Data.Maybe                         (fromMaybe)
import           Data.Proxy
import qualified Data.Text                          as T (pack)
import           Data.Typeable                      (Typeable)
import           Network.HTTP.Media                 (MediaType, mapAcceptMedia,
                                                     matchAccept, matchContent
                                                    , mapAccept)
import           Network.HTTP.Media.RenderHeader    (renderHeader)
import           Network.HTTP.Types                 hiding (Query)
import qualified Network.Wai                        as Wai
import qualified Network.Wai.Parse                  as Wai
import           Web.Cookie
import           WebApi.ContentTypes
import           WebApi.Contract
import           WebApi.Param
import           WebApi.Util
import qualified Data.Text.Encoding                 as TE
import GHC.TypeLits

data RouteResult a = NotMatched | Matched a

type RoutingApplication = Wai.Request -> (RouteResult Wai.Response -> IO Wai.ResponseReceived) -> IO Wai.ResponseReceived

toApplication :: RoutingApplication -> Wai.Application
toApplication app request respond =
  app request $ \routeResult -> case routeResult of
    Matched result -> respond result
    NotMatched -> respond (Wai.responseLBS notFound404 [] "")

type FromWaiRequestCtx m r =
  ( FromParam 'QueryParam (QueryParam m r)
  , FromParam 'FormParam (FormParam m r)
  , FromParam 'FileParam (FileParam m r)
  , FromHeader (HeaderIn m r)
  , FromParam 'Cookie (CookieIn m r)
  , ToHListRecTuple (StripContents (RequestBody m r))
  , PartDecodings (RequestBody m r)
  , SingMethod m
  , EncodingType m r (ContentTypes m r)
  )

fromWaiRequest :: forall m r. FromWaiRequestCtx m r =>
                   Wai.Request
                 -> PathParam m r
                 -> (Request m r -> IO (Response m r))
                 -> IO (Validation [ParamErr] (Response m r))
fromWaiRequest waiReq pathPar handlerFn = do
  let mContentTy = getContentType $ Wai.requestHeaders waiReq
  case hasFormData mContentTy of
    Just _ -> do
      runResourceT $ withInternalState $ \internalState -> do
        (formPar, filePar) <- Wai.parseRequestBody (Wai.tempFileBackEnd internalState) waiReq
        let request = Request <$> pure pathPar
                              <*> (fromQueryParam $ Wai.queryString waiReq)
                              <*> (fromFormParam formPar)
                              <*> (fromFileParam (fmap fromWaiFile filePar))
                              <*> (fromHeader $ Wai.requestHeaders waiReq)
                              <*> (fromCookie $ maybe [] parseCookies (getCookie waiReq))
                              <*> (fromBody [])
        handler' (acceptHeaderType accHdr <$> request)
    Nothing -> do
      bdy <- Wai.lazyRequestBody waiReq
      let rBody = [(fromMaybe (renderHeader $ contentType (Proxy :: Proxy OctetStream)) mContentTy, bdy)]
      let request = Request <$> pure pathPar
                            <*> (fromQueryParam $ Wai.queryString waiReq)
                            <*> (fromFormParam [])
                            <*> (fromFileParam [])
                            <*> (fromHeader $ Wai.requestHeaders waiReq)
                            <*> (fromCookie $ maybe [] parseCookies (getCookie waiReq))
                            <*> (fromBody rBody)
      handler' (acceptHeaderType accHdr <$> request)
  where
    accHdr = getAcceptType (Wai.requestHeaders waiReq)
    handler' (Validation (Right req)) = handlerFn req >>= \resp -> return $ Validation (Right resp)
    handler' (Validation (Left parErr)) = return $ Validation (Left parErr)
    hasFormData x = matchContent [contentType (Proxy :: Proxy MultipartFormData), contentType (Proxy :: Proxy UrlEncoded)] =<< x
    acceptHeaderType :: Maybe ByteString -> Request m r -> Request m r
    acceptHeaderType (Just x) r =
      fromMaybe r $ 
      (\(Encoding t _ _) -> setAcceptHeader t r) <$> matchAcceptHeaders x r
    acceptHeaderType Nothing r = r
    fromBody x = Validation $ either (\e -> Left [NotFound (bspack e)]) (Right . fromRecTuple (Proxy :: Proxy (StripContents (RequestBody m r)))) $ partDecodings (Proxy :: Proxy (RequestBody m r)) x
    bspack = TE.encodeUtf8 . T.pack
    fromWaiFile :: Wai.File FilePath -> (ByteString, FileInfo)
    fromWaiFile (fname, waiFileInfo) = (fname, FileInfo
                                         { fileName        = Wai.fileName waiFileInfo
                                         , fileContentType = Wai.fileContentType waiFileInfo
                                         , fileContent     = Wai.fileContent waiFileInfo
                                         })

toWaiResponse :: ( ToHeader (HeaderOut m r)
                  , ToParam 'Cookie (CookieOut m r)
                  , Encodings (ContentTypes m r) (ApiOut m r)
                  , Encodings (ContentTypes m r) (ApiErr m r)
                  ) => Wai.Request -> Response m r -> Wai.Response
toWaiResponse wreq resp = case resp of
  Success status out hdrs cookies -> case encode' resp out of
    Just (ctype, o') -> let hds = (hContentType, renderHeader ctype) : handleHeaders' (toHeader hdrs) (toCookie cookies)
                        in Wai.responseBuilder status hds o'
    Nothing -> Wai.responseBuilder notAcceptable406 [] "Matching content type not found"
  Failure (Left (ApiError status errs hdrs cookies)) -> case encode' resp errs of
    Just (ctype, errs') -> let hds = (hContentType, renderHeader ctype) : handleHeaders (toHeader <$> hdrs) (toCookie <$> cookies)
                           in Wai.responseBuilder status hds errs'
    Nothing -> Wai.responseBuilder notAcceptable406 [] "Matching content type not found"
  Failure (Right (OtherError ex)) -> Wai.responseBuilder internalServerError500 [] (encodeUtf8Builder (T.pack (displayException ex)))

  where encode' :: ( Encodings (ContentTypes m r) a
                 ) => apiRes m r -> a -> Maybe (MediaType, Builder)
        encode' r o = case getAccept wreq of
          Just acc -> let ecs = encodings (reproxy r) o
                      in (,) <$> matchAccept (map fst ecs) acc <*> mapAcceptMedia ecs acc
          Nothing  -> case encodings (reproxy r) o of
            (x : _)  -> Just x
            _        -> Nothing

        reproxy :: apiRes m r -> Proxy (ContentTypes m r)
        reproxy = const Proxy

        handleHeaders :: Maybe [Header] -> Maybe [(ByteString, CookieInfo ByteString)] -> [Header]
        handleHeaders hds cks = handleHeaders' (maybe [] id hds) (maybe [] id cks)

        handleHeaders' :: [Header] -> [(ByteString, CookieInfo ByteString)] -> [Header]
        handleHeaders' hds cookies = let ckHs = map (\(ck, cv) -> (hSetCookie , renderSC ck cv)) cookies
                                     in hds <> ckHs
        renderSC k v = toStrict . toLazyByteString . renderSetCookie $ def
          { setCookieName = k
          , setCookieValue = cookieValue v
          , setCookiePath = cookiePath v
          , setCookieExpires = cookieExpires v
          , setCookieMaxAge  = cookieMaxAge v
          , setCookieDomain = cookieDomain v
          , setCookieHttpOnly = fromMaybe False (cookieHttpOnly v)
          , setCookieSecure   = fromMaybe False (cookieSecure v)
          }


-- | Describes the implementation of a single API end point corresponding to @ApiContract (ApiInterface p) m r@
class (ApiContract (ApiInterface p) m r) => ApiHandler (p :: *) (m :: *) (r :: *) where
  -- | Handler for the API end point which returns a 'Response'.
  --
  -- TODO : 'query' type parameter is an experimental one used for trying out dependently typed params.
  -- This parameter will let us refine the 'ApiOut' to the structure that is requested by the client.
  -- for eg : graph.facebook.com/bgolub?fields=id,name,picture
  --
  -- This feature is not finalized and might get changed \/ removed.
  -- Currently the return type of handler is equivalent to `Response m r`
  --
  handler :: (query ~ '[])
            => Tagged query p
            -> Request m r
            -> HandlerM p (Query (Response m r) query)

type family Query (t :: *) (query :: [*]) :: * where
  Query t '[] = t

-- | Binds implementation to interface and provides a pluggable handler monad for the endpoint handler implementation.
class ( MonadCatch (HandlerM p)
      , MonadIO (HandlerM p)
      , WebApi (ApiInterface p)
      ) => WebApiServer (p :: *) where
  -- | Type of the handler 'Monad'. It should implement 'MonadCatch' and 'MonadIO' classes. Defaults to 'IO'.
  type HandlerM p :: * -> *
  type ApiInterface p :: *
  -- provides common defaulting information for api handlers

  -- | Create a value of @IO a@ from @HandlerM p a@.
  toIO :: p -> WebApiRequest -> HandlerM p a -> IO a

  default toIO :: (HandlerM p ~ IO) => p -> WebApiRequest -> HandlerM p a -> IO a
  toIO _ _ = id

  type HandlerM p = IO

-- | Type of settings of the server.
data ServerSettings = ServerSettings

-- | Default server settings.
serverSettings :: ServerSettings
serverSettings = ServerSettings

newtype NestedApplication (apps :: [(Symbol, *)])
  = NestedApplication [Wai.Application]

unconsNesApp :: NestedApplication (app ': apps) -> (Wai.Application, NestedApplication apps)
unconsNesApp (NestedApplication (app : apps)) = (app, NestedApplication apps)
unconsNesApp (NestedApplication []) = error "Panic: cannot happen by construction"

newtype ApiComponent c = ApiComponent Wai.Application

nestedApi :: NestedApplication '[]
nestedApi = NestedApplication []

data NestedR = NestedR 

-- | Type of Exception raised in a handler.
data ApiException m r = ApiException { apiException :: ApiError m r }

instance Show (ApiException m r) where
  show (ApiException _) = "ApiException"

instance (Typeable m, Typeable r) => Exception (ApiException m r) where

handleApiException :: (query ~ '[], Monad (HandlerM p)) => p -> ApiException m r -> (HandlerM p) (Query (Response m r) query)
handleApiException _ = return . Failure . Left . apiException

handleSomeException :: (query ~ '[], Monad (HandlerM p)) => p -> SomeException -> (HandlerM p) (Query (Response m r) query)
handleSomeException _ = return . Failure . Right . OtherError

getCookie :: Wai.Request -> Maybe ByteString
getCookie = fmap snd . find ((== hCookie) . fst) . Wai.requestHeaders

getAccept :: Wai.Request -> Maybe ByteString
getAccept = fmap snd . find ((== hAccept) . fst) . Wai.requestHeaders

hSetCookie :: HeaderName
hSetCookie = "Set-Cookie"

getContentType :: ResponseHeaders -> Maybe ByteString
getContentType = fmap snd . find ((== hContentType) . fst)

getAcceptType :: ResponseHeaders -> Maybe ByteString
getAcceptType = fmap snd . find ((== hAccept) . fst)

newtype Tagged (s :: [*]) b = Tagged { unTagged :: b }

toTagged :: Proxy s -> b -> Tagged s b
toTagged _ = Tagged

class EncodingType m r (ctypes :: [*]) where
  getEncodingType :: Proxy ctypes -> [(MediaType, Encoding m r)]

instance ( Accept ctype
         , Elem ctype (ContentTypes m r)
         , EncodingType m r ctypes
         ) => EncodingType m r (ctype ': ctypes) where
  getEncodingType _ =
    (contentType (Proxy :: Proxy ctype), Encoding (Proxy :: Proxy ctype) Proxy Proxy) :
    getEncodingType (Proxy :: Proxy ctypes)

instance EncodingType m r '[] where
  getEncodingType _ = []

matchAcceptHeaders :: forall m r ctypes.
                       ( EncodingType m r ctypes
                       , ctypes ~ ContentTypes m r
                       ) => ByteString -> Request m r -> Maybe (Encoding m r)
matchAcceptHeaders b _ =
  mapAccept (getEncodingType (Proxy :: Proxy ctypes)) b

data WebApiRequest = WebApiRequest { rawRequest :: Wai.Request }
