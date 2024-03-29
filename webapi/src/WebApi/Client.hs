{-|
Module      : WebApi.Client
License     : BSD3
Stability   : experimental

Provides a client for a web api for a given contract.
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
{-# LANGUAGE CPP                   #-}
{-# LANGUAGE TupleSections         #-}

module WebApi.Client
       (
         -- * Client related functions
         client
       , fromClientResponse
       , toClientRequest
       , link
       , gclient
       , get
       , get'
       , post
       , post'
       , put
       

         -- * Types
       , ToParams, FromParams
       , ClientSettings (..)
       , GClientSettings
       , gConnectionManager       
       , UnknownClientException

         -- * Connection manager
       , HC.Manager
       , HC.newManager
       , HC.closeManager
       , HC.withManager
       , HC.HasHttpManager (..)

         -- ** Connection manager settings
       , HC.ManagerSettings
       , HC.defaultManagerSettings
       , HC.tlsManagerSettings
       ) where

import           Control.Exception
import           Control.Monad ( (>=>) )
import qualified Control.Monad.Catch as C
import           Data.Bifunctor
import qualified Data.ByteString as B
import           Data.ByteString.Builder (toLazyByteString)
import           Data.ByteString.Lazy (ByteString, fromStrict, toStrict)
import           Data.Either (isRight)
import           Data.Kind
import           Data.List (find)
import           Data.Maybe ( fromMaybe )
import           Data.Proxy
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Text.Encoding (decodeUtf8)
import qualified Data.Text.Encoding as T
import           Data.Time.Clock ( getCurrentTime, addUTCTime, nominalDay )
import           Data.Typeable (Typeable)
import           Data.Word
import           GHC.Exts
import qualified Network.HTTP.Client as HC
import qualified Network.HTTP.Client.MultipartFormData as HC
import qualified Network.HTTP.Client.TLS as HC (tlsManagerSettings)
import           Network.HTTP.Media                    (RenderHeader (..),
                                                        mapContentMedia)
import           Network.HTTP.Types hiding (Query)
import           UnliftIO ( MonadIO (..) )
import qualified UnliftIO as U
import qualified WebApi.AnonClient as Anon
import           WebApi.ContentTypes
import           WebApi.Contract
import           WebApi.Internal
import           WebApi.Param
import           WebApi.Util

-- | Datatype representing the settings related to client.
data ClientSettings io =
  ClientSettings { baseUrl           :: String     -- ^ base url of the API being called.
                 , connectionManager :: HC.Manager -- ^ connection manager for the connection.
                 , requestHook       :: HC.Request -> io HC.Request -- ^ http request hook
                 , responseHook      :: HC.Response ByteString -> io (HC.Response ByteString) -- ^ http response hook
                 , extraUnreserved   :: [Word8] -- ^ Reserved characters to be considered as unreserved.
                 }

data Route' m r = Route'

-- | Creates the 'Response' type from the response body.
fromClientResponse ::
  forall m r io.
  ( FromHeader (HeaderOut m r)
  , Decodings (ContentTypes m r) (ApiOut m r)
  , Decodings (ContentTypes m r) (ApiErr m r)
  , FromParam 'Cookie (CookieOut m r)
  , MonadIO io
  ) => HC.Response ByteString -> io (Response m r)
fromClientResponse hcResp = do
  let status   = HC.responseStatus hcResp
      hdrsOut  = HC.responseHeaders hcResp
      respBodyBS = HC.responseBody hcResp
      respHdr  = fromHeader hdrsOut :: Validation [ParamErr] (HeaderOut m r)
      respCj   = (HC.destroyCookieJar (HC.responseCookieJar hcResp))
      respCk   = fromCookie (cookieBS respCj)
      -- respCk   = fromCookie
  case statusIsSuccessful status of
    True ->
      let res = Success
            <$> pure status
            <*> (Validation $ toParamErr $ decode' (Route' :: Route' m r) respBodyBS)
            <*> respHdr
            <*> respCk
      in pure $ case res of
           Validation (Right success) -> success
           Validation (Left errs) -> Failure $ Right (OtherError (toException $ UnknownClientException $ T.intercalate "\n" $ fmap (T.pack . show) errs))

    False ->
      let res = ApiError
              <$> pure status
              <*> (Validation $ toParamErr $ decode' (Route' :: Route' m r) respBodyBS)
              <*> (Just <$> respHdr)
              <*> (Just <$> respCk)
      in pure $ case res of
        Validation (Right failure) -> (Failure . Left) failure
        Validation (Left errs) -> Failure $ Right (OtherError (toException $ UnknownClientException $ T.intercalate "\n" $ fmap (T.pack . show) errs))

    where toParamErr :: Either Text a -> Either [ParamErr] a
          toParamErr (Left _str) = Left [ParseErr "" _str]
          toParamErr (Right r)   = Right r

          decode' :: ( Decodings (ContentTypes m r) a
                   ) => apiRes m r -> ByteString -> Either Text a
          decode' r o = case getContentType (HC.responseHeaders hcResp) of
            Just ctype -> let decs = decodings (reproxy r) o
                          in maybe (firstRight o (map snd decs)) (first T.pack) (mapContentMedia decs ctype)
            Nothing    -> firstRight o (map snd (decodings (reproxy r) o))

          reproxy :: apiRes m r -> Proxy (ContentTypes m r)
          reproxy = const Proxy

          firstRight :: ByteString -> [Either String b] -> Either Text b
          firstRight resp = maybe (Left (T.decodeUtf8 $ toStrict $ resp)) (first T.pack) . find isRight

          cookieBS :: [HC.Cookie] -> [(B.ByteString, B.ByteString)]
          cookieBS = map (\ck -> (HC.cookie_name ck, HC.cookie_value ck))

-- | Creates a request from the 'Request' type.
toClientRequest ::
  forall m r io.
  ( ToParam 'PathParam (PathParam m r)
  , ToParam 'QueryParam (QueryParam m r)
  , ToParam 'FormParam (FormParam m r)
  , ToHeader (HeaderIn m r)
  , ToParam 'FileParam (FileParam m r)
  , ToParam 'Cookie (CookieIn m r)
  , SingMethod m
  , MkPathFormatString r
  , PartEncodings (RequestBody m r)
  , ToHListRecTuple (StripContents (RequestBody m r))
  , MonadIO io
  ) => [Word8] -> HC.Request -> Request m r -> io HC.Request
toClientRequest extraUnres clientReq req = do
  now <- liftIO getCurrentTime
  let accHeader = maybe [] singleton ((hAccept,) <$>  (getRawAcceptHeader req))
      singleton x = [x]
      cReqQP = HC.setQueryString queryPar clientReq
      ckJar  = HC.createCookieJar (cks now)
      cReqUE = if Prelude.null formPar
               then cReqQP
               else HC.urlEncodedBody formPar cReqQP
      fileParts = if Prelude.null filePar
                  then []
                  else Prelude.map (\(pname, finfo) -> HC.partFileSource (decodeUtf8 pname) (fileContent finfo)) filePar
      cReqMP = if Prelude.null filePar && Prelude.null formPar
               then case partEncs of
                 (_ : _) -> do
                    let (mt, b) = firstPart
                    return cReqUE { HC.requestHeaders = HC.requestHeaders cReqUE ++ [(hContentType, renderHeader mt)]
                                  , HC.requestBody = HC.RequestBodyLBS $ toLazyByteString b
                                  }
                 [] -> return cReqUE
               else if not (Prelude.null filePar) then HC.formDataBody fileParts cReqUE else return cReqUE
  s <- cReqMP
  let s' = s { HC.method = singMethod (Proxy :: Proxy m)
             , HC.path = uriPath
             , HC.requestHeaders = HC.requestHeaders s ++ accHeader ++ (toHeader $ headerIn req)
             , HC.cookieJar = Just ckJar
            }
  return s'            
  where queryPar = toQueryParam $ queryParam req
        formPar = toFormParam $ formParam req
        filePar = toFileParam $ fileParam req
        uriPath = renderUriPath (HC.path clientReq) extraUnres (pathParam req) req
        firstPart = head . head $ partEncs
        partEncs = partEncodings cts (toRecTuple cts' (requestBody req))
        cts     = Proxy :: Proxy (RequestBody m r)
        cts'     = Proxy :: Proxy (StripContents (RequestBody m r))
        cks now = map (uncurry (mkCookieVal now)) (toCookie $ cookieIn req)
        mkCookieVal now k ci =
          HC.Cookie { HC.cookie_name = k
                    , HC.cookie_value = cookieValue ci
                    , HC.cookie_expiry_time = fromMaybe (addUTCTime nominalDay now) (cookieExpires ci)
                    , HC.cookie_domain = fromMaybe "" (cookieDomain ci)
                    , HC.cookie_path = fromMaybe "/" (cookiePath ci)
                    , HC.cookie_creation_time = now
                    , HC.cookie_last_access_time = now
                    , HC.cookie_persistent = False
                    , HC.cookie_host_only = False
                    , HC.cookie_secure_only = fromMaybe False (cookieSecure ci)
                    , HC.cookie_http_only = fromMaybe False (cookieHttpOnly ci)
                    }

-- | Given a `Request` type, create the request and obtain a response. Gives back a 'Response'.
client :: forall m r io.
          ( ToParam 'PathParam (PathParam m r)
          , ToParam 'QueryParam (QueryParam m r)
          , ToParam 'FormParam (FormParam m r)
          , ToParam 'Cookie (CookieIn m r)
          , ToHeader (HeaderIn m r)
          , ToParam 'FileParam (FileParam m r)
          , FromHeader (HeaderOut m r)
          , FromParam 'Cookie (CookieOut m r)
          , Decodings (ContentTypes m r) (ApiOut m r)
          , Decodings (ContentTypes m r) (ApiErr m r)
          , SingMethod m
          , MkPathFormatString r
          , PartEncodings (RequestBody m r)
          , ToHListRecTuple (StripContents (RequestBody m r))
          , U.MonadUnliftIO io
          , C.MonadThrow io
          ) => ClientSettings io -> Request m r -> io (Response m r)
client sett req = do
  cReqInit <- HC.parseRequest (baseUrl sett)
  cReq' <- toClientRequest (extraUnreserved sett) cReqInit req 
  cReq <- requestHook sett cReq'
  U.catches (U.withRunInIO $ \k -> HC.withResponse cReq (connectionManager sett) (k . (go >=> responseHook sett >=> fromClientResponse)))

    [ U.Handler (\(ex :: HC.HttpException) -> do
                case ex of
#if MIN_VERSION_http_client(0,5,0)
                  HC.HttpExceptionRequest _req (HC.StatusCodeException resp _) -> do
                    let resHeaders = HC.responseHeaders resp
                        status = HC.responseStatus resp
#else
                  (HC.StatusCodeException status resHeaders _) -> do
#endif
                    let mBody = find ((== "X-Response-Body-Start") . fst) resHeaders
                        bdy   = case mBody of
                                  Nothing -> "[]"
                                  Just (_, body) -> body
                        removeExtraHeaders = filter (\(x, _) -> (x /= "X-Request-URL") || (x /= "X-Response-Body-Start"))
                    return $ case ApiError
                          <$> pure status
                          <*> (Validation $ toParamErr $ decode' resHeaders (Route' :: Route' m r) (fromStrict bdy))
                          <*> (Just <$> (fromHeader . removeExtraHeaders $ resHeaders))
                          -- TODO: Handle cookies
                          <*> pure Nothing of
                       Validation (Right failure) -> (Failure . Left) failure
                       Validation (Left errs) -> Failure $ Right (OtherError (toException $ ApiErrParseFailException status $ T.intercalate "\n" $ fmap (T.pack . show) errs))
                  _ -> return . Failure . Right . OtherError $ toException ex
                      )
    , U.Handler (\(ex :: IOException) -> return . Failure . Right . OtherError $ toException ex)
    ]
    where toParamErr :: Either String a -> Either [ParamErr] a
          toParamErr (Left _str) = Left [ParseErr "" $ T.pack _str]
          toParamErr (Right r)   = Right r

          decode' :: ( Decodings (ContentTypes m r) a
                   ) => [Header] -> apiRes m r -> ByteString -> Either String a
          decode' h r o = case getContentType h of
            Just ctype -> let decs = decodings (reproxy r) o
                          in maybe (firstRight (map snd decs)) id (mapContentMedia decs ctype)
            Nothing    -> firstRight (map snd (decodings (reproxy r) o))

          reproxy :: apiRes m r -> Proxy (ContentTypes m r)
          reproxy = const Proxy

          firstRight :: [Either String b] -> Either String b
          firstRight = maybe (Left "Couldn't find matching Content-Type") id . find isRight

          go resp = liftIO $ do
            -- NOTE: Consuming body strictly
            respBodyBSS <- HC.brConsume (HC.responseBody resp)
            let respBodyBS = fromStrict $ B.concat respBodyBSS
            pure (respBodyBS <$ resp)

-- | This exception is used to signal an irrecoverable error while deserializing the response.
data UnknownClientException = UnknownClientException Text
                            deriving (Typeable, Show)

instance Exception UnknownClientException where

-- | Client settings for anonymous (contractless) client
---  Currently duplicated from ClientSettings to avoid a breaking change  
data GClientSettings (m :: Type) (r :: Type) =
  GClientSettings { gBaseUrl           :: String           -- ^ base url of the API being called.
                  , gConnectionManager :: Maybe HC.Manager -- ^ connection manager for the connection.
                  }

type family ToParams m r xs :: Constraint where
  ToParams m r xs =
    ( ToHeader (Anon.LookupHParam Anon.HeaderParamLabel xs)
    , ToParam 'PathParam (HListToTuple (FilterDynP (ToPieces r)))
    , ToParam 'QueryParam (Anon.LookupHParam Anon.QueryParamLabel xs)
    , ToParam 'FormParam (Anon.LookupHParam Anon.FormParamLabel xs)
    , ToParam 'Cookie (Anon.LookupHParam Anon.CookieParamLabel xs)
    , ToParam 'FileParam (Anon.LookupHParam Anon.FileParamLabel xs)
    , PartEncodings (Anon.UnProxyReqBody
                     (Anon.LookupHParam Anon.ReqBodyParamLabel xs))
    , ToHListRecTuple (StripContents
                             (Anon.UnProxyReqBody
                                (Anon.LookupHParam Anon.ReqBodyParamLabel xs)))
    , MkPathFormatString r
    )

type family FromParams o e xs co ho :: Constraint where
  FromParams o e xs co ho =
    ( Decodings (Anon.UnProxyContentType (Anon.LookupHParam Anon.ContentTypeLabel xs)) o
    , Decodings (Anon.UnProxyContentType (Anon.LookupHParam Anon.ContentTypeLabel xs)) e
    , FromHeader ho
    , FromParam 'Cookie co
    )

-- | Generic anonymous client
gclient :: forall m r xs e o co ho.
          ( Anon.ParamCtx m r o e xs co ho
          , ToParams m r xs
          , FromParams o e xs co ho
          ) => GClientSettings m r -> Anon.HParam xs -> IO (Anon.Response o e co ho)
gclient acls hparams = do
  cls <- mkClientSettings acls
  Anon.fromAnonResponse <$> client cls (Anon.toAnonRequest (Proxy :: Proxy m) (Proxy :: Proxy r) hparams)

instance IsString (GClientSettings m r) where
  fromString url = GClientSettings { gBaseUrl           = url
                                   , gConnectionManager = Nothing
                                   }

get :: forall r xs e o.
      ( Anon.ParamCtx GET r o e xs () ()
      , ToParams GET r xs
      , FromParams o e xs () ()
      ) => GClientSettings GET r -> Anon.HParam xs -> IO (Either e o)
get cls par = gclient cls par >>= respToEither

get' :: forall r xs e o co ho.
      ( Anon.ParamCtx GET r o e xs co ho
      , ToParams GET r xs
      , FromParams o e xs co ho
      ) => GClientSettings GET r -> Anon.HParam xs -> IO (Anon.Response o e co ho)
get' = gclient

post :: forall r xs e o.
       ( Anon.ParamCtx POST r o e xs () ()
       , ToParams POST r xs
       , FromParams o e xs () ()
       ) => GClientSettings POST r -> Anon.HParam xs -> IO (Either e o)
post cls par = gclient cls par >>= respToEither

post' :: forall r xs e o co ho.
       ( Anon.ParamCtx POST r o e xs co ho
       , ToParams POST r xs
       , FromParams o e xs co ho
       ) => GClientSettings POST r -> Anon.HParam xs -> IO (Anon.Response o e co ho)
post' = gclient

put :: forall r xs e o co ho.
       ( Anon.ParamCtx PUT r o e xs co ho
       , ToParams PUT r xs
       , FromParams o e xs co ho
       ) => GClientSettings PUT r -> Anon.HParam xs -> IO (Anon.Response o e co ho)
put = gclient

respToEither :: Anon.Response o e () () -> IO (Either e o)
respToEither (Anon.Success _ o _ _)       = pure (Right o)
respToEither (Anon.ServerFailure _ e _ _) = pure (Left e)
respToEither (Anon.ClientFailure (OtherError ex)) = throwIO ex

mkClientSettings ::
  ( Applicative io
  ) => GClientSettings m r -> IO (ClientSettings io)
mkClientSettings acls = do
  let bUrl = gBaseUrl acls
      conMgr = gConnectionManager acls
  case conMgr of
    Nothing -> do
      mgr <- HC.newManager HC.defaultManagerSettings
      pure (ClientSettings { baseUrl           = bUrl
                           , connectionManager = mgr
                           , requestHook       = pure
                           , responseHook      = pure
                           , extraUnreserved   = []
                           }
           )
    Just mgr -> do
      pure (ClientSettings { baseUrl           = bUrl
                           , connectionManager = mgr
                           , requestHook       = pure
                           , responseHook      = pure
                           , extraUnreserved   = []
                           }
           )

