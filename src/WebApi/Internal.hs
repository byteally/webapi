{-# LANGUAGE TypeFamilies, KindSignatures, MultiParamTypeClasses, DataKinds, FlexibleContexts, OverloadedStrings, ScopedTypeVariables #-}
module WebApi.Internal where

import           Blaze.ByteString.Builder (toByteString)
import           Data.Aeson (ToJSON, toJSON)
import           Data.Aeson.Encode (encodeToByteStringBuilder)
import           Data.ByteString.Char8 (unpack)
import           Data.Proxy
import           Data.Text.Encoding (decodeUtf8)
import           Http.Param
import           Network.HTTP.Types hiding (Query)
import           Network.URI (URI (..))
import qualified Network.Wai as Wai
import qualified Network.Wai.Parse as Wai
import           WebApi.Contract

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
  (formPar, filePar) <- Wai.parseRequestBody Wai.lbsBackEnd waiReq
  return $ Req <$> pure pathPar
    <*> (fromQueryParam $ Wai.queryString waiReq)
    <*> (fromFormParam $ formPar)
    <*> (fromFileParam undefined) --TODO:
    <*> (fromHeader undefined) --TODO:
    <*> pure () -- TODO: FixMe
    <*> (pure $ decodeUtf8 $ Wai.requestMethod waiReq)

toWaiResponse :: ( ToParam (HeaderOut m r) 'Header
                  , ToJSON (ApiOut m r)
                  , ToJSON (ApiErr m r)
                  ) => Response m r -> Wai.Response
toWaiResponse resp = case resp of
  Success status out hdrs _ -> Wai.responseBuilder status (toHeader hdrs) (encodeToByteStringBuilder $ toJSON out)
  Failure (Left (ApiError status errs hdrs _)) -> Wai.responseBuilder status (toHeader hdrs) (encodeToByteStringBuilder $ toJSON errs)
  Failure (Right _ex) -> Prelude.error "TODO: @ toWaiResponse"


link :: ( ToParam (QueryParam m r) 'QueryParam
        , ToParam (PathParam m r) 'PathParam
        ) =>
          route m r
        -> URI
        -> PathParam m r
        -> Maybe (QueryParam m r)
        -> URI
link _ base paths query = base
                          { uriPath = renderPaths paths
                          , uriQuery = maybe "" renderQuery' query
                          }
  where renderPaths :: ToParam path 'PathParam => path -> String
        renderPaths p = unpack $ toByteString
                               $ encodePathSegments $ map decodeUtf8 (toPathParam p)
        renderQuery' :: (ToParam query 'QueryParam) => query -> String
        renderQuery' q = unpack $ renderQuery False $ toQueryParam q

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

