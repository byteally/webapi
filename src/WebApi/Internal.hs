{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
module WebApi.Internal where

import           Blaze.ByteString.Builder     (Builder, toByteString)
import qualified Blaze.ByteString.Builder.Char.Utf8 as Utf8 (fromText)
import           Control.Exception
import           Control.Monad.Catch          (MonadThrow)
import           Control.Monad.IO.Class       (MonadIO)
import           Control.Monad.Trans.Resource (runResourceT, withInternalState)
import           Data.ByteString              (ByteString)
import           Data.ByteString.Char8        (unpack)
import           Data.List                    (find, foldl')
import           Data.Monoid                  ((<>))
import           Data.Proxy
import           Data.Text                    (Text, pack)
import           Data.Text.Encoding           (decodeUtf8)
import qualified Network.HTTP.Client          as HC
import           Network.HTTP.Media           (mapAcceptMedia)
import           Network.HTTP.Types           hiding (Query)
import           Network.URI                  (URI (..))
import qualified Network.Wai                  as Wai
import qualified Network.Wai.Parse            as Wai
import           Web.Cookie
import           WebApi.ContentTypes
import           WebApi.Contract
import           WebApi.Param


data RouteResult a = NotMatched | Matched a

type RoutingApplication = Wai.Request -> (RouteResult Wai.Response -> IO Wai.ResponseReceived) -> IO Wai.ResponseReceived

toApplication :: RoutingApplication -> Wai.Application
toApplication app request respond =
  app request $ \routeResult -> case routeResult of
    Matched result -> respond result
    NotMatched -> respond (Wai.responseLBS notFound404 [] "")

fromWaiRequest :: ( FromParam (QueryParam m r) 'QueryParam
                   , FromParam (FormParam m r) 'FormParam
                   , FromParam (FileParam m r) 'FileParam
                   , FromHeader (HeaderIn m r)
                   , FromParam (CookieIn m r) 'Cookie
                   ) => Wai.Request
                 -> PathParam m r
                 -> IO (Validation [ParamErr] (Request m r))
fromWaiRequest waiReq pathPar = do
  (formPar, filePar) <- runResourceT $ withInternalState $
                        \internalState -> Wai.parseRequestBody (Wai.tempFileBackEnd internalState) waiReq
                          
  return $ Req <$> pure pathPar
    <*> (fromQueryParam $ Wai.queryString waiReq)
    <*> (fromFormParam formPar)
    <*> (fromFileParam filePar)
    <*> (fromHeader $ Wai.requestHeaders waiReq)
    <*> (fromCookie $ maybe [] parseCookies (getCookie waiReq))
    <*> (pure $ decodeUtf8 $ Wai.requestMethod waiReq)

toWaiResponse :: ( ToHeader (HeaderOut m r)
                  , ToParam (CookieOut m r) 'Cookie 
                  , Encodings (ContentTypes m r) (ApiOut m r)
                  , Encodings (ContentTypes m r) (ApiErr m r)
                  ) => Wai.Request -> Response m r -> Wai.Response
toWaiResponse wreq resp = case resp of
  Success status out hdrs cookies -> case encode resp out of
    Just o' -> Wai.responseBuilder status (handleHeaders' (toHeader hdrs) (toCookie cookies)) o'
    Nothing -> Wai.responseBuilder notAcceptable406 [] "Matching content type not found"
  Failure (Left (ApiError status errs hdrs cookies)) -> case encode resp errs of
    Just errs' -> Wai.responseBuilder status (handleHeaders (toHeader <$> hdrs) (toCookie <$> cookies)) errs'
    Nothing -> Wai.responseBuilder notAcceptable406 [] "Matching content type not found"
  Failure (Right (OtherError ex)) -> Wai.responseBuilder internalServerError500 [] (Utf8.fromText (pack (displayException ex)))

  where encode :: ( Encodings (ContentTypes m r) a
                 ) => apiRes m r -> a -> Maybe Builder
        encode r o = case getAccept wreq of
          Just acc -> mapAcceptMedia (encodings (reproxy r) o) acc
          Nothing  -> case encodings (reproxy r) o of
            (x : _)  -> Just (snd x)
            _        -> Nothing

        reproxy :: apiRes m r -> Proxy (ContentTypes m r)
        reproxy = const Proxy

        handleHeaders :: Maybe [Header] -> Maybe [(ByteString, ByteString)] -> [Header]
        handleHeaders hds cks = handleHeaders' (maybe [] id hds) (maybe [] id cks)
 
        handleHeaders' :: [Header] -> [(ByteString, ByteString)] -> [Header]
        handleHeaders' hds cookies = let ckHs = map (\(ck, cv) -> (hSetCookie , renderSC ck cv)) cookies
                                     in hds <> ckHs
        renderSC k v = toByteString (renderSetCookie (def { setCookieName = k, setCookieValue = v }))

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
                          { uriPath = unpack $ renderPaths paths r
                          , uriQuery = maybe "" renderQuery' query
                          }
  where renderQuery' :: (ToParam query 'QueryParam) => query -> String
        renderQuery' q = unpack $ renderQuery False $ toQueryParam q

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

type family ApiInterface (p :: *) :: *

class (API (ApiInterface p) m r) => Server (p :: *) (m :: *) (r :: *) where
  handler :: (query ~ '[], HandlerM (ApiInterface p) ~ IO)
            => p
            -> Proxy query
            -> Request m r
            -> (HandlerM (ApiInterface p)) (Query (Response m r) query)


type family Query (t :: *) (query :: [*]) :: * where
  Query t '[] = t

class ( MonadThrow (HandlerM p)
      , MonadIO (HandlerM p)  
      ) => ApiProvider (p :: *) where
  type HandlerM p :: * -> *
  type ApiMeta p :: *
  type ApiPager p :: *

  type ApiMeta p = ()
  type ApiPager p = ()

data ServerSettings = ServerSettings

serverSettings :: ServerSettings
serverSettings = ServerSettings

data PathSegment = StaticSegment Text
                 | Hole
                 deriving Show

class MkPathFormatString r where
  mkPathFormatString :: Proxy r -> [PathSegment]

getCookie :: Wai.Request -> Maybe ByteString
getCookie = fmap snd . find ((== hCookie) . fst) . Wai.requestHeaders

getAccept :: Wai.Request -> Maybe ByteString
getAccept = fmap snd . find ((== hAccept) . fst) . Wai.requestHeaders

hSetCookie :: HeaderName
hSetCookie = "Set-Cookie"

getContentType :: HC.Response a -> Maybe ByteString
getContentType = fmap snd . find ((== hContentType) . fst) . HC.responseHeaders
