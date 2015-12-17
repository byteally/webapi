{-# LANGUAGE TypeFamilies, KindSignatures, MultiParamTypeClasses, DataKinds, FlexibleContexts, OverloadedStrings, ScopedTypeVariables , ConstraintKinds #-}
module WebApi.Internal where

import           Blaze.ByteString.Builder (toByteString, Builder)
import           Data.ByteString (ByteString)
import           Data.ByteString.Char8 (unpack)
import           Data.Proxy
import           Data.List (foldl', find)
import           Data.Text (Text)
import           Data.Text.Encoding (decodeUtf8)
import           WebApi.Param
import qualified Network.HTTP.Client as HC
import           Network.HTTP.Media (mapAcceptMedia)
import           Network.HTTP.Types hiding (Query)
import           Network.URI (URI (..))
import qualified Network.Wai as Wai
import qualified Network.Wai.Parse as Wai
import           WebApi.Contract
import           WebApi.ContentTypes

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
                   , FromParam (HeaderIn m r) 'Header
                   , CookieIn m r ~ ()
                   ) => Wai.Request
                 -> PathParam m r
                 -> IO (Validation [ParamErr] (Request m r))
fromWaiRequest waiReq pathPar = do
  (formPar, _filePar) <- Wai.parseRequestBody Wai.lbsBackEnd waiReq
  return $ Req <$> pure pathPar
    <*> (fromQueryParam $ Wai.queryString waiReq)
    <*> (fromFormParam $ formPar)
    <*> (fromFileParam $ undefined) --TODO:
    <*> (fromHeader undefined) --TODO:
    <*> pure () -- TODO: FixMe
    <*> (pure $ decodeUtf8 $ Wai.requestMethod waiReq)

toWaiResponse :: ( ToParam (HeaderOut m r) 'Header
                  , Encodings (ContentTypes m r) (ApiOut m r)
                  , Encodings (ContentTypes m r) (ApiErr m r) 
                  ) => Wai.Request -> Response m r -> Wai.Response
toWaiResponse wreq resp = case resp of
  Success status out hdrs _ -> case encode resp out of
    Just o' -> Wai.responseBuilder status (toHeader hdrs) o'
    Nothing -> Prelude.error "TODO: @ toWaiResponse"
  Failure (Left (ApiError status errs hdrs _)) -> case encode resp errs of
    Just errs' -> Wai.responseBuilder status (toHeader hdrs) errs'
    Nothing -> Prelude.error "TODO: @ toWaiResponse"
  Failure (Right _ex) -> Prelude.error "TODO: @ toWaiResponse"

  where encode :: ( Encodings (ContentTypes m r) a
                 ) => apiRes m r -> a -> Maybe Builder
        encode r o = case getAccept wreq of
          Just acc -> mapAcceptMedia (encodings (reproxy r) o) acc
          Nothing  -> case encodings (reproxy r) o of
            (x : _)  -> Just (snd x)
            _        -> Nothing

        reproxy :: apiRes m r -> Proxy (ContentTypes m r)
        reproxy = const Proxy

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

class ApiProvider (p :: *) where
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

getAccept :: Wai.Request -> Maybe ByteString
getAccept = fmap snd . find ((== hAccept) . fst) . Wai.requestHeaders

getContentType :: HC.Response a -> Maybe ByteString
getContentType = fmap snd . find ((== hContentType) . fst) . HC.responseHeaders
