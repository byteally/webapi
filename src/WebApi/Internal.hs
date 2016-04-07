{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DefaultSignatures     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
module WebApi.Internal where

import           Blaze.ByteString.Builder           (Builder, toByteString)
import qualified Blaze.ByteString.Builder.Char.Utf8 as Utf8 (fromText)
import           Control.Exception
import           Control.Monad.Catch                (MonadCatch)
import           Control.Monad.IO.Class             (MonadIO)
import           Control.Monad.Trans.Resource       (runResourceT,
                                                     withInternalState)
import           Data.ByteString                    (ByteString)
import           Data.ByteString.Char8              (pack, unpack)
import           Data.List                          (find, foldl')
import           Data.Monoid                        ((<>))
import           Data.Maybe                         (fromMaybe)
import           Data.Proxy
import           Data.Text                          (Text)
import qualified Data.Text                          as T (pack)
import           Data.Text.Encoding                 (decodeUtf8)
import           Data.Typeable                      (Typeable)
import           Network.HTTP.Media                 (MediaType, mapAcceptMedia,
                                                     matchAccept)
import           Network.HTTP.Media.RenderHeader    (renderHeader)
import           Network.HTTP.Types                 hiding (Query)
import           Network.URI                        (URI (..))
import qualified Network.Wai                        as Wai
import qualified Network.Wai.Parse                  as Wai
import           Web.Cookie
import           WebApi.ContentTypes
import           WebApi.Contract
import           WebApi.Param
import           WebApi.Util


data RouteResult a = NotMatched | Matched a

type RoutingApplication = Wai.Request -> (RouteResult Wai.Response -> IO Wai.ResponseReceived) -> IO Wai.ResponseReceived

toApplication :: RoutingApplication -> Wai.Application
toApplication app request respond =
  app request $ \routeResult -> case routeResult of
    Matched result -> respond result
    NotMatched -> respond (Wai.responseLBS notFound404 [] "")

fromWaiRequest :: forall m r.
                   ( FromParam (QueryParam m r) 'QueryParam
                   , FromParam (FormParam m r) 'FormParam
                   , FromParam (FileParam m r) 'FileParam
                   , FromHeader (HeaderIn m r)
                   , FromParam (CookieIn m r) 'Cookie
                   , ToHListRecTuple (StripContents (RequestBody m r))
                   , PartDecodings (RequestBody m r)
                   , SingMethod m
                   ) => Wai.Request
                 -> PathParam m r
                 -> IO (Validation [ParamErr] (Request m r))
fromWaiRequest waiReq pathPar = do
  let mContentTy = getContentType $ Wai.requestHeaders waiReq
  (formPar, filePar, rBody) <- case hasFormData mContentTy of
    Just _ -> do
      (formPar, filePar) <- runResourceT $ withInternalState $
                              \internalState -> Wai.parseRequestBody (Wai.tempFileBackEnd internalState) waiReq
      print $ hasFormData $ getContentType $ Wai.requestHeaders waiReq
      return (formPar, filePar, [])
    Nothing -> do
      bdy <- Wai.requestBody waiReq
      print bdy
      return ([], [], [(fromMaybe (renderHeader $ contentType (Proxy :: Proxy OctetStream)) mContentTy, bdy)])

  return $ Request <$> pure pathPar
    <*> (fromQueryParam $ Wai.queryString waiReq)
    <*> (fromFormParam formPar)
    <*> (fromFileParam filePar)
    <*> (fromHeader $ Wai.requestHeaders waiReq)
    <*> (fromCookie $ maybe [] parseCookies (getCookie waiReq))
    <*> (fromBody rBody)
  where
    hasFormData x = matchAccept [contentType (Proxy :: Proxy MultipartFormData), contentType (Proxy :: Proxy UrlEncoded)] =<< x
    fromBody x = Validation $ either (const (Left [NotFound "415"])) (Right . fromRecTuple (Proxy :: Proxy (StripContents (RequestBody m r)))) $ partDecodings (Proxy :: Proxy (RequestBody m r)) x

toWaiResponse :: ( ToHeader (HeaderOut m r)
                  , ToParam (CookieOut m r) 'Cookie
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
  Failure (Right (OtherError ex)) -> Wai.responseBuilder internalServerError500 [] (Utf8.fromText (T.pack (displayException ex)))

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

        handleHeaders :: Maybe [Header] -> Maybe [(ByteString, ByteString)] -> [Header]
        handleHeaders hds cks = handleHeaders' (maybe [] id hds) (maybe [] id cks)

        handleHeaders' :: [Header] -> [(ByteString, ByteString)] -> [Header]
        handleHeaders' hds cookies = let ckHs = map (\(ck, cv) -> (hSetCookie , renderSC ck cv)) cookies
                                     in hds <> ckHs
        renderSC k v = toByteString (renderSetCookie (def { setCookieName = k, setCookieValue = v }))

-- | Generate a type safe URL for a given route type. The URI can be used for setting a base URL if required.
link :: ( ToParam (QueryParam m r) 'QueryParam
        , MkPathFormatString r
        , ToParam (PathParam m r) 'PathParam
        ) =>
          route m r
        -> URI
        -> PathParam m r
        -> Maybe (QueryParam m r)
        -> URI
link r base paths query = base
                          { uriPath = unpack $ renderUriPath (pack $ uriPath base) paths r
                          , uriQuery = maybe "" renderQuery' query
                          }
  where renderQuery' :: (ToParam query 'QueryParam) => query -> String
        renderQuery' = unpack . renderQuery True . toQueryParam

renderUriPath ::  ( ToParam path 'PathParam
                   , MkPathFormatString r
                   ) => ByteString -> path -> route m r -> ByteString
renderUriPath basePth p r = case basePth of
          ""  -> renderPaths p r
          "/" -> renderPaths p r
          _   -> basePth `mappend` renderPaths p r

renderPaths :: ( ToParam path 'PathParam
                , MkPathFormatString r
                ) => path -> route m r -> ByteString
renderPaths p r = toByteString
                  $ encodePathSegments $ uriPathPieces (toPathParam p)

  where uriPathPieces :: [ByteString] -> [Text]
        uriPathPieces dynVs = reverse $ fst $ foldl' (flip fillHoles) ([], dynVs) (mkPathFormatString (toRoute r))

        fillHoles :: PathSegment -> ([Text], [ByteString]) -> ([Text], [ByteString])
        fillHoles (StaticSegment t) (segs, dynVs)    = (t : segs, dynVs)
        fillHoles  Hole             (segs, dynV: xs) = (decodeUtf8 dynV : segs, xs)
        fillHoles  Hole             (_segs, [])      = error "Panic: fewer pathparams than holes"

        toRoute :: (MkPathFormatString r) => route m r -> Proxy r
        toRoute = const Proxy

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
      ) => WebApiImplementation (p :: *) where
  -- | Type of the handler 'Monad'. It should implement 'MonadCatch' and 'MonadIO' classes. Defaults to 'IO'.
  type HandlerM p :: * -> *
  type ApiInterface p :: *
  -- provides common defaulting information for api handlers

  -- | Create a value of @IO a@ from @HandlerM p a@.
  toIO :: p -> HandlerM p a -> IO a

  default toIO :: (HandlerM p ~ IO) => p -> HandlerM p a -> IO a
  toIO _ = id

  type HandlerM p = IO

-- | Type of settings of the server.
data ServerSettings = ServerSettings

-- | Default server settings.
serverSettings :: ServerSettings
serverSettings = ServerSettings

-- | Type of segments of a Path.
data PathSegment = StaticSegment Text -- ^ A static segment
                 | Hole -- ^ A dynamic segment
                 deriving (Show, Eq)

-- | Describe representation of the route.
class MkPathFormatString r where
  -- | Given a route, this function should produce the @[PathSegment]@ of that route. This gives the flexibility to hook in a different routing system into the application.
  mkPathFormatString :: Proxy r -> [PathSegment]

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

newtype Tagged (s :: [*]) b = Tagged { unTagged :: b }

toTagged :: Proxy s -> b -> Tagged s b
toTagged _ = Tagged

