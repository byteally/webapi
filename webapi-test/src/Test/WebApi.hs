{-# LANGUAGE OverloadedStrings #-}
module Test.WebApi
  ( runWebApis
  , runWebApis'
  , testClients
  , emptyApp
  , addApp
  , runWebApi
  , testClient
  , fromClientRequest
  , toWaiRequest
  , runWebApiSessions
  , WebApiSessions (..)
  , WebApiSessionsConfig
  , WebApiSession
  , ClientRequest (ClientRequest, path, query, form, file, header, body)
  , UnknownClientException (..)
  , AppIsElem
  ) where

import Network.Wai hiding (Request, Response)
import qualified Network.Wai as Wai
import Network.Wai.Test hiding (getClientCookies, modifyClientCookies, setClientCookie, deleteClientCookie)
import qualified Network.Wai.Test as WaiTest
import qualified Network.Wai.Test.Internal as WaiInt
import qualified Network.HTTP.Types as H
import           Network.HTTP.Media                    (mapContentMedia)
import Control.Monad.Reader
import Control.Monad.State
import WebApi.Contract
import WebApi.Param
import WebApi.Util
import WebApi.ContentTypes
import WebApi.Param
import WebApi.Internal (getContentType)
import WebApi.Server (WebApiWaiApp, getWaiApp)
import Web.Cookie
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Char8 as Char8
import Data.Kind
import Data.Function
import Data.Coerce
import Data.Either
-- import           Data.List (find)
import Data.Bifunctor
import qualified Data.Text.Encoding as T
import Data.Text (Text)
import qualified Data.Text as T
import Data.Typeable
import Control.Exception (toException, Exception)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
-- import Data.Maybe
import Data.Foldable
import GHC.TypeLits
import Data.IORef

newtype WebApiSession app a = WebApiSession (Session a)
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader Application, MonadState WaiInt.ClientState)

runWebApiSession :: WebApiSession app a -> Session a
runWebApiSession = coerce

type family NamespaceOf (r :: Type) where
  NamespaceOf (ns :// (r :: k)) = ns

toWaiRequest :: forall meth r.
  ( ToParam 'PathParam (PathParam meth r)
  , ToParam 'QueryParam (QueryParam meth r)
  , ToParam 'FormParam (FormParam meth r)
  , ToParam 'FileParam (FileParam meth r)
  , ToHeader (HeaderIn meth r)
  , PartEncodings (RequestBody meth r)
  , ToHListRecTuple (StripContents (RequestBody meth r))
  , MkPathFormatString r
  , SingMethod meth
  ) => Request meth r
  -> IO Wai.Request
toWaiRequest req@Request {queryParam, pathParam, formParam, fileParam, headerIn, requestBody} = do
  hasValRef <- newIORef True
  pure (defaultRequest
        & flip setRawPathInfo uriPath
        & setWaiQP
        & urlEncodedBody hasValRef formPar)
  where
    accHeader = maybe [] (:[]) ((H.hAccept,) <$>  (getRawAcceptHeader req))
    partEncs = partEncodings (Proxy @(RequestBody meth r)) (toRecTuple (Proxy @(StripContents (RequestBody meth r))) requestBody)
    firstPart = head . head $ partEncs -- TODO: Check head
    formPar = toFormParam formParam
    filePar = toFileParam fileParam
    uriPath = renderUriPath "" [] pathParam req
    meth = singMethod (Proxy :: Proxy meth)
    qitms = toQueryParam queryParam
    setWaiQP r = r { queryString = qitms
                   , rawQueryString = H.renderQuery True qitms
                   , requestMethod = meth
                   , requestHeaders = accHeader ++ toHeader headerIn
                   }

urlEncodedBody :: IORef Bool -> [(ByteString, ByteString)] -> Wai.Request -> Wai.Request
urlEncodedBody _ [] req = req
urlEncodedBody hasValRef headers req = req
    { requestMethod = "POST"
    , requestHeaders =
        (ct, "application/x-www-form-urlencoded")
      : filter (\(x, _) -> x /= ct) (requestHeaders req)
    } & setRequestBodyChunks body
  where
    ct = "Content-Type"
    body = atomicModifyIORef' hasValRef $ \case
      False -> (False, "")
      True -> (False, H.renderSimpleQuery False headers)

fromResponse :: forall meth r.
  ( FromHeader (HeaderOut meth r)
  , FromParam Cookie (CookieOut meth r)
  , Decodings (ContentTypes meth r) (ApiOut meth r)
  , Decodings (ContentTypes meth r) (ApiErr meth r)
  ) => SResponse -> Session (Response meth r)
fromResponse SResponse {simpleStatus=status, simpleHeaders=hdrs, simpleBody=respBodyBS} = do
  clientcooks <- WaiTest.getClientCookies
  toWebApiResponse status hdrs (M.toList $ fmap renderSetCookieBS clientcooks) respBodyBS

-- TODO: Move it to core
toWebApiResponse :: forall meth r m.
  ( Applicative m
  , FromHeader (HeaderOut meth r)
  , FromParam Cookie (CookieOut meth r)
  , Decodings (ContentTypes meth r) (ApiOut meth r)
  , Decodings (ContentTypes meth r) (ApiErr meth r)
  ) => H.Status
  -> [H.Header]
  -> [(ByteString, ByteString)] -- TODO: Maybe it could be [(ByteString, SetCookie)]
  -> LBS.ByteString
  -> m (Response meth r)
toWebApiResponse status hdrs cookiesKV respBodyBS = do
  let
    respHdr = fromHeader hdrs :: Validation [ParamErr] (HeaderOut meth r)
    respCk = fromCookie cookiesKV
    decodeResp :: Decodings (ContentTypes meth r) a => LBS.ByteString -> Either Text a
    decodeResp o = case getContentType hdrs of
      Just ctype -> let decs = decodings (Proxy :: Proxy (ContentTypes meth r)) o
                    in maybe (firstRight o (map snd decs)) (first T.pack) (mapContentMedia decs ctype)
      Nothing    -> firstRight o (map snd (decodings (Proxy :: Proxy (ContentTypes meth r)) o))

    toParamErr :: Either Text a -> Either [ParamErr] a
    toParamErr (Left str) = Left [ParseErr "" str]
    toParamErr (Right r)   = Right r

    firstRight :: LBS.ByteString -> [Either String b] -> Either Text b
    firstRight resp = maybe (Left (T.decodeUtf8 $ LBS.toStrict $ resp)) (first T.pack) . find isRight
    
  case H.statusIsSuccessful status || H.statusIsRedirection status of
    True ->
      let res = Success
            <$> pure status
            <*> (Validation $ toParamErr $ decodeResp respBodyBS)
            <*> respHdr
            <*> respCk
      in pure $ case res of
           Validation (Right success) -> success
           Validation (Left errs) -> Failure $ Right (OtherError (toException $ UnknownClientException $ T.intercalate "\n" $ fmap (T.pack . show) errs))

    False ->
      let res = ApiError
              <$> pure status
              <*> (Validation $ toParamErr $ decodeResp respBodyBS)
              <*> (Just <$> respHdr)
              <*> (Just <$> respCk)
      in pure $ case res of
        Validation (Right failure) -> (Failure . Left) failure
        Validation (Left errs) -> Failure $ Right (OtherError (toException $ UnknownClientException $ (T.pack (show status)) <> ": " <> (T.intercalate "\n" $ fmap (T.pack . show) errs)))

testClient :: forall meth r app.
  ( WebApi app
  , app ~ NamespaceOf r
  , SingMethod meth
  , ToParam 'PathParam (PathParam meth r)
  , ToParam 'QueryParam (QueryParam meth r)
  , ToParam 'FormParam (FormParam meth r)
  , ToParam 'FileParam (FileParam meth r)
  , ToHeader (HeaderIn meth r)
  , FromHeader (HeaderOut meth r)
  , FromParam Cookie (CookieOut meth r)
  , Decodings (ContentTypes meth r) (ApiOut meth r)
  , Decodings (ContentTypes meth r) (ApiErr meth r)
  , PartEncodings (RequestBody meth r)
  , ToHListRecTuple (StripContents (RequestBody meth r))
  , MkPathFormatString r
  ) =>
  ClientRequest meth r
  -> WebApiSession app (Response meth r)
testClient (MKClientRequest creq) = do
  waiReq <- liftIO $ toWaiRequest creq
  sresp <- WebApiSession $ request waiReq
  WebApiSession $ fromResponse sresp

runWebApi :: forall app a.
  WebApiSession app a
  -> Application
  -> IO a
runWebApi (WebApiSession sess) app = runSession sess app

newtype ClientsState = ClientsState (Map TypeRep WaiInt.ClientState)

data Applications = Applications
  { native :: Map TypeRep Application
  , external :: Map TypeRep ()
  }

initApps :: Applications
initApps = Applications
  { native = mempty
  , external = mempty
  }

initClientsState :: ClientsState
initClientsState = ClientsState mempty

newtype WebApiSessions (apps :: [Type]) a = WebApiSessions (ReaderT Applications (StateT ClientsState IO) a)
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader Applications, MonadState ClientsState)

runWebApiSessions :: WebApiSessions apps a -> ReaderT Applications (StateT ClientsState IO) a
runWebApiSessions = coerce

testClients :: forall meth r app apps.
  ( WebApi app
  , SingMethod meth
  , ToParam 'PathParam (PathParam meth (app://r))
  , ToParam 'QueryParam (QueryParam meth (app://r))
  , ToParam 'FormParam (FormParam meth (app://r))
  , ToParam 'FileParam (FileParam meth (app://r))
  , ToHeader (HeaderIn meth (app://r))
  , FromHeader (HeaderOut meth (app://r))
  , FromParam Cookie (CookieOut meth (app://r))
  , Decodings (ContentTypes meth (app://r)) (ApiOut meth (app://r))
  , Decodings (ContentTypes meth (app://r)) (ApiErr meth (app://r))
  , PartEncodings (RequestBody meth (app://r))
  , ToHListRecTuple (StripContents (RequestBody meth (app://r)))
  , MkPathFormatString (app://r)
  , Typeable app
  , AppIsElem app apps
  ) =>
  ClientRequest meth (app://r)
  -> WebApiSessions apps (Response meth (app://r))
testClients creq = do
  let
    appRep = typeRep (Proxy :: Proxy app)
  WebApiSessions $ ReaderT $ \Applications {native, external} -> case M.lookup appRep native of
    Just app -> do
      ClientsState css <- get
      case M.lookup appRep css of
        Nothing -> error $ "Panic: app not found for: " <> show appRep
        Just cstate -> do
          liftIO $ print $ WaiInt.clientCookies cstate
          (a, s) <- liftIO $ runStateT (runReaderT (runWebApiSession $ testClient creq) app) cstate
          modify' $ \(ClientsState css') -> ClientsState $ M.insert appRep s css'
          pure a
    Nothing -> case M.lookup appRep external of
      Just () -> error "TODO: External Application not support yet!"
      Nothing -> error $ "Panic: app not found for: " <> show appRep

type AppIsElem :: Type -> [Type] -> Constraint
type AppIsElem (app :: Type) (apps :: [Type]) = AppIsElem' apps app apps
  
type family AppIsElem' (appUniv :: [Type]) (app :: Type) (apps :: [Type]) :: Constraint where
  AppIsElem' _ app (app ': _) = ()
  AppIsElem' appUniv app (app1 ': apps) = AppIsElem' appUniv app apps
  AppIsElem' appUniv app '[] = TypeError ('Text "App " ':<>: 'ShowType app ':<>: 'Text " is not a member of " ':<>: 'ShowType appUniv)
  
data WebApiSessionsConfig (apps :: [Type]) = WebApiSessionsConfig
  { applications :: Applications
  , clientsState :: ClientsState
  }

emptyApp :: WebApiSessionsConfig '[]
emptyApp = WebApiSessionsConfig
  { applications = initApps
  , clientsState = initClientsState
  }

addApp :: forall app apps.Typeable app => WebApiWaiApp app -> WebApiSessionsConfig apps -> WebApiSessionsConfig (app ': apps)
addApp waapp WebApiSessionsConfig {applications, clientsState} =
  let
    app = getWaiApp waapp
    appRep = typeRep (Proxy @app)
    Applications {native, external} = applications
    ClientsState cstate = clientsState
  in WebApiSessionsConfig
     { applications = Applications { native = M.insert appRep app native, external}
     , clientsState = ClientsState $ M.insert appRep WaiInt.initState cstate
     }

runWebApis :: WebApiSessionsConfig apps -> WebApiSessions apps a -> IO a
runWebApis WebApiSessionsConfig {applications, clientsState} (WebApiSessions sess) =
  evalStateT (runReaderT sess applications) clientsState

runWebApis' :: WebApiSessionsConfig apps -> WebApiSessions apps a -> IO (a, WebApiSessionsConfig apps)
runWebApis' WebApiSessionsConfig {applications, clientsState} (WebApiSessions sess) = do
  (a, clientsStateNew) <- runStateT (runReaderT sess applications) clientsState
  pure (a, WebApiSessionsConfig {applications, clientsState = clientsStateNew})


-- getClientCookies :: WebApiSessions apps ClientCookies
-- getClientCookies = WaiInt.clientCookies <$> lift get

-- modifyClientCookies :: (ClientCookies -> ClientCookies) -> WebApiSessions apps ()
-- modifyClientCookies f =
--     lift (modify' (\cs -> cs{WaiInt.clientCookies = f $ WaiInt.clientCookies cs}))

-- setClientCookie :: SetCookie -> WebApiSessions apps ()
-- setClientCookie c =
--     modifyClientCookies $
--         M.insert (setCookieName c) c

-- deleteClientCookie :: ByteString -> WebApiSessions apps ()
-- deleteClientCookie =
--     modifyClientCookies . M.delete  

newtype ClientRequest meth r = MKClientRequest (Request meth r)

pattern ClientRequest :: forall meth r.
  (SingMethod meth) =>
  PathParam meth r
  -> QueryParam meth r
  -> FormParam meth r
  -> FileParam meth r
  -> HeaderIn meth r
  -> HListToTuple (StripContents (RequestBody meth r))
  -> ClientRequest meth r
pattern ClientRequest
  { path
  , query
  , form
  , file
  , header
  , body
  } <- MKClientRequest (Request {pathParam=path, queryParam=query, formParam=form, fileParam=file, headerIn=header, requestBody=body}) where
  ClientRequest pp qp fp fip hi rb = MKClientRequest $ Request pp qp fp fip hi (error "Cookies are not expected to be explicity set by client") rb
{-# COMPLETE ClientRequest #-}  


fromClientRequest :: ClientRequest meth r -> Request meth r
fromClientRequest = coerce

-- | This exception is used to signal an irrecoverable error while deserializing the response.
data UnknownClientException = UnknownClientException Text
                            deriving (Typeable, Show)

instance Exception UnknownClientException where

{-  
-- Predicates as defined servant-quickcheck


-- | [__Best Practice__]
--
-- @500 Internal Server Error@ should be avoided - it may represent some
-- issue with the application code, and it moreover gives the client little
-- indication of how to proceed or what went wrong.
--
-- This function checks that the response code is not 500.
--
-- /Since 0.0.0.0/

not500 :: Bool
not500 = False

-- | [__Optional__]
--
-- This function checks that the response from the server does not take longer
-- than the specified number of nanoseconds.
--
-- /Since 0.0.0.0/
notLongerThan :: Integer -> Bool
notLongerThan _maxAllowed = False

-- | [__Best Practice__]
--
-- Returning anything other than an object when returning JSON is considered
-- bad practice, as:
--
--   (1) it is hard to modify the returned value while maintaining backwards
--   compatibility
--   (2) many older tools do not support top-level arrays
--   (3) whether top-level numbers, booleans, or strings are valid JSON depends
--   on what RFC you're going by
--   (4) there are security issues with top-level arrays
--
-- This function checks that any @application/json@ responses only return JSON
-- objects (and not arrays, strings, numbers, or booleans) at the top level.
--
-- __References__:
--
--   * JSON Grammar: <https://tools.ietf.org/html/rfc7159#section-2 RFC 7159 Section 2>
--   * JSON Grammar: <https://tools.ietf.org/html/rfc4627#section-2 RFC 4627 Section 2>
--
-- /Since 0.0.0.0/
onlyJsonObjects :: Bool
onlyJsonObjects = False

-- | __Optional__
--
-- When creating a new resource, it is good practice to provide a @Location@
-- header with a link to the created resource.
--
-- This function checks that every @201 Created@ response contains a @Location@
-- header, and that the link in it responds with a 2XX response code to @GET@
-- requests.
--
-- This is considered optional because other means of linking to the resource
-- (e.g. via the response body) are also acceptable; linking to the resource in
-- some way is considered best practice.
--
-- __References__:
--
--   * 201 Created: <https://tools.ietf.org/html/rfc7231#section-6.3.2 RFC 7231 Section 6.3.2>
--   * Location header: <https://tools.ietf.org/html/rfc7231#section-7.1.2 RFC 7231 Section 7.1.2>
--
-- /Since 0.0.0.0/
createContainsValidLocation :: Bool
createContainsValidLocation = False


-- | [__Optional__]
--
-- The @Last-Modified@ header represents the time a resource was last
-- modified. It is used to drive caching and conditional requests.
--
-- When using this mechanism, the server adds the @Last-Modified@ header to
-- responses. Clients may then make requests with the @If-Modified-Since@
-- header to conditionally request resources. If the resource has not
-- changed since that date, the server responds with a status code of 304
-- (@Not Modified@) without a response body.
--
-- The @Last-Modified@ header can also be used in conjunction with the
-- @If-Unmodified-Since@ header to drive optimistic concurrency.
--
-- The @Last-Modified@ date must be in RFC 822 format.
--
-- __References__:
--
--   * 304 Not Modified: <https://tools.ietf.org/html/rfc7232#section-4.1 RFC 7232 Section 4.1>
--   * Last-Modified header: <https://tools.ietf.org/html/rfc7232#section-2.2 RFC 7232 Section 2.2>
--   * If-Modified-Since header: <https://tools.ietf.org/html/rfc7232#section-3.3 RFC 7232 Section 3.3>
--   * If-Unmodified-Since header: <https://tools.ietf.org/html/rfc7232#section-3.4 RFC 7232 Section 3.4>
--   * Date format: <https://tools.ietf.org/html/rfc2616#section-3.3 RFC 2616 Section 3.3>
--
-- /Since 0.0.0.0/
getsHaveLastModifiedHeader :: Bool
getsHaveLastModifiedHeader = False

-- | [__RFC Compliance__]
--
-- When an HTTP request has a method that is not allowed,
-- a 405 response should be returned. Additionally, it is good practice to
-- return an @Allow@
-- header with the list of allowed methods.
--
-- This function checks that every @405 Method Not Allowed@ response contains
-- an @Allow@ header with a list of standard HTTP methods.
--
-- Note that 'servant' itself does not currently set the @Allow@ headers.
--
-- __References__:
--
--   * @Allow@ header: <https://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html RFC 2616 Section 14.7>
--   * Status 405: <https://www.w3.org/Protocols/rfc2616/rfc2616-sec10.html RFC 2616 Section 10.4.6>
--   * Servant Allow header issue: <https://github.com/haskell-servant/servant/issues/489 Issue #489>
--
-- /Since 0.0.0.0/
notAllowedContainsAllowHeader :: Bool
notAllowedContainsAllowHeader = False
  
-- | [__RFC Compliance__]
--
-- When a request contains an @Accept@ header, the server must either return
-- content in one of the requested representations, or respond with @406 Not
-- Acceptable@.
--
-- This function checks that every *successful* response has a @Content-Type@
-- header that matches the @Accept@ header. It does *not* check that the server
-- matches the quality descriptions of the @Accept@ header correctly.
--
-- __References__:
--
--   * @Accept@ header: <https://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html RFC 2616 Section 14.1>
--
-- /Since 0.0.0.0/
honoursAcceptHeader :: Bool
honoursAcceptHeader = False

-- | [__Best Practice__]
--
-- Whether or not a representation should be cached, it is good practice to
-- have a @Cache-Control@ header for @GET@ requests. If the representation
-- should not be cached, used @Cache-Control: no-cache@.
--
-- This function checks that @GET@ responses have @Cache-Control@ header.
-- It does NOT currently check that the header is valid.
--
-- __References__:
--
--   * @Cache-Control@ header: <https://tools.ietf.org/html/rfc7234#section-5.2 RFC 7234 Section 5.2>
--
-- /Since 0.0.0.0/
getsHaveCacheControlHeader :: Bool
getsHaveCacheControlHeader = False

-- | [__Best Practice__]
--
-- Like 'getsHaveCacheControlHeader', but for @HEAD@ requests.
--
-- /Since 0.0.0.0/
headsHaveCacheControlHeader :: Bool
headsHaveCacheControlHeader = False

-- | [__RFC Compliance__]
--
-- Any @401 Unauthorized@ response must include a @WWW-Authenticate@ header.
--
-- This function checks that, if a response has status code 401, it contains a
-- @WWW-Authenticate@ header.
--
-- __References__:
--
--   * @WWW-Authenticate@ header: <https://tools.ietf.org/html/rfc7235#section-4.1 RFC 7235 Section 4.1>
--
-- /Since 0.0.0.0/
unauthorizedContainsWWWAuthenticate :: Bool
unauthorizedContainsWWWAuthenticate = False

-- | [__RFC Compliance__]
--
-- [An HTML] document will start with exactly this string: <!DOCTYPE html>
--
-- This function checks that HTML documents (those with `Content-Type: text/html...`)
-- include a DOCTYPE declaration at the top. We do not enforce capital case for the string `DOCTYPE`.
--
-- __References__:
--
--  * HTML5 Doctype: <https://tools.ietf.org/html/rfc7992#section-6.1 RFC 7992 Section 6.1>
-- /Since 0.0.0.0/
htmlIncludesDoctype :: Bool
htmlIncludesDoctype = False
-}

