{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DefaultSignatures     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE DerivingStrategies       #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecursiveDo                #-}

module WebApi.Reflex.Dom.Router
  ( respond
  , raise
  , navigate
  , defUIRequest
  , modifyUIRequest
  , uiApp
  , WebUIServer (..)
  , UIHandler (..)
  , Dom
  , DomResponse (..)
  , UIRequestRep
  , mkUIRequestRep

  , UIRequest
  , compactUIServer
  , navigateToAppLink
  , appLink
  , AppLink (..)
  , MountPoint
  , ApiMount (..)
  ) where

import           Control.Monad.Fix
import           Control.Monad.IO.Class
import           Data.Functor
import           Data.Kind
import           Data.Proxy
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Text.Encoding (encodeUtf8)
import           Data.Typeable
import           GHC.TypeLits
import           Network.HTTP.Types.Status
import           Reflex hiding (Request, Response, HList(..))
import           Reflex.Dom.Contrib.MonadRouted
import           Reflex.Dom.Core hiding ( Request, Response, Namespace, HList (..), link )
import qualified Reflex.Dom.Core as Dom
import           Reflex.Network ( networkView, networkHold )
import           WebApi.ContentTypes
import           WebApi.Contract hiding ( Route )
import           WebApi.Param hiding ( link )
import           WebApi.Util hiding ( Route )
import qualified WebApi.Util as W
import qualified WebApi.Param as W
import qualified Data.Text.Encoding as T
import Network.HTTP.Types.URI
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as Char8
import Data.Typeable
import Data.Maybe
import GHC.Stack
import Control.Exception.Safe (SomeException (..), Exception(..))

-- Compact Start
import           GHC.Records
import           GHC.TypeLits
import           Data.Type.Equality
-- Compact End

data Dom m

instance SingMethod m => SingMethod (Dom m) where
  singMethod _ = singMethod (Proxy :: Proxy m)

class WebUIServer (s :: *) where
  type UIInterface s :: *
  type UIInterface s = s

class UIHandler w (t :: *) s m r where
  handler :: s -> Dynamic t (Request m r) -> w (Response m r)

newtype UIRequestRep =
  UIRequestRep { getUIRequestRep :: TypeRep
               } deriving newtype (Show, Eq)

compactUIServer :: forall api m server t. server t (RouteT t m) -> CompactUIServer api (server t (RouteT t m))
compactUIServer = CompactUIServer

mkUIRequestRep ::
  forall route (m :: *) (r :: *).
  ( Typeable m
  , Typeable r
  ) => route m r -> UIRequestRep
mkUIRequestRep _ = UIRequestRep { getUIRequestRep = typeOf (undefined :: UIRequest m r) }

data DomRequest = DomRequest
  { pathInfo :: [Text]
  , queryPathInfo :: [(BS.ByteString, Maybe BS.ByteString)]
  }

data DomResponse =
    DomSuccess UIRequestRep
  | DomFailure (Either [ParamErr] BS.ByteString)
  deriving (Show)

type ReflexDomApplication t m = (UIRequestRep -> Event t DomRequest) -> Dynamic t DomRequest -> DomRequest -> RouteResult (UIRequestRep, m (Event t DomResponse))

class Monad w => Router (w :: * -> *) (t :: *) (server :: *) (r :: k) (pr :: (*, [*])) where
  route :: Proxy '(r, t) -> server -> ParsedRoute t pr -> (Dynamic t [ParamErr] -> w ()) -> ReflexDomApplication t w

instance ( SingMethod (m :: *)
         , Router w t s r '(Dom m, '[])
         , MonadWidget t w
         ) => Router w t s (W.Route '[Dom m] r) pr where
  route _ _s parsedRoute page400 getDomReqUpdEv reqDyn req =
    route (Proxy :: Proxy '(r, t)) _s (Nil (Proxy :: Proxy (Dom m))) page400 getDomReqUpdEv reqDyn req

instance ( MonadWidget t w ) => Router w t s (W.Route '[] r) pr where
  route _ _ _ _ _ _ _ = NotMatched

instance
  ( Router w t s route pr
  , Router w t s routes pr
  , Reflex t
  ) => Router w t s ((route :: *) ': routes) pr where
  route _ _s parsedRoute page400 getDomReqUpdEv request req =
     (<>)
     (route (Proxy :: Proxy '(route, t)) _s parsedRoute page400 getDomReqUpdEv request req)
     (route (Proxy :: Proxy '(routes, t)) _s parsedRoute page400 getDomReqUpdEv request req)

instance (Monad w, Reflex t) => Router w t s '[] pr where
  route _ _ _ _ _ _ _ = NotMatched

instance (Monad w, Router w t s rest '(m, pp :++ '[Namespace ns])) => Router w t s ((ns :: *) :// (rest :: *)) '(m, pp) where
  route _ _s parsedRoute page400 getDomReqUpdEv request req =
    route (Proxy :: Proxy '(rest, t)) _s (snocParsedRoute parsedRoute $ NSPiece (Proxy :: Proxy ns)) page400 getDomReqUpdEv request req

instance (Monad w, Router w t s (MarkDyn rest) '(m, (pp :++ '[DynamicPiece piece])), DecodeParam piece, Reflex t)
                      => Router w t s ((piece :: *) :/ (rest :: *)) '(m, pp) where
  route _ _s parsedRoute page400 getDomReqUpdEv reqDyn req = case pathInfo req of
    (lpth : rpths)  -> case (decodeParam (encodeUtf8 lpth) :: Maybe piece) of
      Just dynPiece -> route (Proxy :: Proxy '((MarkDyn rest), t)) _s (snocParsedRoute parsedRoute $ DPiece dynPiece) page400 getDomReqUpdEv reqDyn (req {pathInfo = rpths})
      Nothing -> NotMatched
    _ -> NotMatched

instance (Reflex t, Monad w, Router w t s (MarkDyn rest) '(m, (pp :++ '[StaticPiece piece])), KnownSymbol piece)
  => Router w t s ((piece :: Symbol) :/ (rest :: *)) '(m, pp) where
  route _ _s parsedRoute page400 getDomReqUpdEv reqDyn req = case pathInfo req of
    (lpth : rpths) | lpieceTxt == lpth -> route (Proxy :: Proxy '((MarkDyn rest), t)) _s (snocParsedRoute parsedRoute $ SPiece (Proxy :: Proxy piece)) page400 getDomReqUpdEv reqDyn (req {pathInfo = rpths})
    _ -> NotMatched

    where lpieceTxt = symTxt (Proxy :: Proxy piece)

-- Base Cases

instance ( KnownSymbol lpiece
         , KnownSymbol rpiece
         , paths ~ (pp :++ '[StaticPiece lpiece, StaticPiece rpiece])
         , paths ~ ((pp :++ '[StaticPiece lpiece]) :++ '[StaticPiece rpiece])
         , route ~ (FromPieces paths)
         , UIHandler w t s m route
         , PathParam m route ~ HListToTuple (FilterDynP paths)
         , AllDecodeParam (FilterDynP paths)
         , Encodings (ContentTypes m route) (ApiErr m route)
         , Encodings (ContentTypes m route) (ApiOut m route)
         , ToHeader (HeaderOut m route)
         , ToParam 'Cookie (CookieOut m route)
         , ParamErrToApiErr (ApiErr m route)
         , Typeable m
         , Typeable route
         , WebUIServer s
         , Monad w
         , SingMethod m
         , Reflex t
         , MonadWidget t w
         , FromParam 'QueryParam (QueryParam m route)
         , FormParam m route ~ ()
         , FileParam m route ~ ()
         , CookieIn m route ~ ()
         , HeaderIn m route ~ ()
         , RequestBody m route ~ '[]
         ) => Router w t s ((lpiece :: Symbol) :/ (rpiece :: Symbol)) '(m, pp) where
  route _ serv parsedRoute page400 getDomReqUpdEv reqDyn req = case pathInfo req of
    (lpth : rpth : [])
      | lpieceTxt == lpth && rpieceTxt == rpth ->
          Matched (mkUIRequestRep (undefined :: UIRequest m route), getResponse)
      | otherwise -> NotMatched
    _ -> NotMatched

    where
      lpieceTxt = symTxt (Proxy :: Proxy lpiece)
      rpieceTxt = symTxt (Proxy :: Proxy rpiece)
      pRoute :: ParsedRoute t '(m, paths)
      pRoute = snocParsedRoute (snocParsedRoute parsedRoute $ SPiece (Proxy :: Proxy lpiece)) $ SPiece (Proxy :: Proxy rpiece)
      pathPar = fromParsedRoute pRoute
      getResponse =
        toDomResponse reqDyn (fromDomRequest @m @route req (fromParsedRoute pRoute) reqDyn (getPathParamCtor pRoute) (apiHandler serv) page400 getDomReqUpdEv)

instance ( KnownSymbol rpiece
         , paths ~ (pp :++ '[DynamicPiece lpiece, StaticPiece rpiece])
         , paths ~ ((pp :++ '[DynamicPiece lpiece]) :++ '[StaticPiece rpiece])
         , route ~ (FromPieces paths)
         , UIHandler w t s m route
         , PathParam m route ~ HListToTuple (FilterDynP paths)
         , AllDecodeParam (FilterDynP paths)
         , Encodings (ContentTypes m route) (ApiErr m route)
         , Encodings (ContentTypes m route) (ApiOut m route)
         , ToHeader (HeaderOut m route)
         , ToParam 'Cookie (CookieOut m route)
         , DecodeParam lpiece
         , ParamErrToApiErr (ApiErr m route)
         , Typeable m
         , Typeable route
         , WebUIServer s
         , Monad w
         , SingMethod m
         , Reflex t
         , MonadWidget t w
         , FromParam 'QueryParam (QueryParam m route)
         , FormParam m route ~ ()
         , FileParam m route ~ ()
         , CookieIn m route ~ ()
         , HeaderIn m route ~ ()
         , RequestBody m route ~ '[]
         ) => Router w t s ((lpiece :: *) :/ (rpiece :: Symbol)) '(m, pp) where
  route _ serv parsedRoute page400 getDomReqUpdEv reqDyn req = case pathInfo req of
    (lpth : rpth : [])
      | rpieceTxt == rpth -> case (decodeParam (encodeUtf8 lpth) :: Maybe lpiece) of
          Just dynVal -> Matched (mkUIRequestRep (undefined :: UIRequest m route), getResponse dynVal)
          Nothing     -> NotMatched
    _ -> NotMatched
    where
      rpieceTxt = symTxt (Proxy :: Proxy rpiece)
      getResponse dynVal =
        let pRoute :: ParsedRoute t '(m, paths)
            pRoute = snocParsedRoute (snocParsedRoute parsedRoute $ DPiece dynVal) $ SPiece (Proxy :: Proxy rpiece)
        in toDomResponse reqDyn $ fromDomRequest @m @route req (fromParsedRoute pRoute) reqDyn (getPathParamCtor pRoute) (apiHandler serv) page400 getDomReqUpdEv

instance ( route ~ (FromPieces (pp :++ '[DynamicPiece t0]))
         , UIHandler w t s m route
         , PathParam m route ~ HListToTuple (FilterDynP (pp :++ '[DynamicPiece t0]))
         , AllDecodeParam (FilterDynP (pp :++ '[DynamicPiece t0]))
         , Encodings (ContentTypes m route) (ApiErr m route)
         , Encodings (ContentTypes m route) (ApiOut m route)
         , ToHeader (HeaderOut m route)
         , ToParam 'Cookie (CookieOut m route)
         , DecodeParam t0
         , ParamErrToApiErr (ApiErr m route)
         , Typeable m
         , Typeable route
         , WebUIServer s
         , Monad w
         , SingMethod m
         , Reflex t
         , MonadWidget t w
         , FromParam 'QueryParam (QueryParam m route)
         , FormParam m route ~ ()
         , FileParam m route ~ ()
         , CookieIn m route ~ ()
         , HeaderIn m route ~ ()
         , RequestBody m route ~ '[]
         ) => Router w t s (DynamicPiece t0) '(m, pp) where
  route _ serv parsedRoute page400 getDomReqUpdEv reqDyn req = case pathInfo req of
    (lpth : []) -> case (decodeParam (encodeUtf8 lpth) :: Maybe t0) of
      Just dynVal -> Matched (mkUIRequestRep (undefined :: UIRequest m route), getResponse dynVal)
      Nothing     -> NotMatched
    _           -> NotMatched
    where
      getResponse dynVal =
        let
          pRoute :: ParsedRoute t '(m, (pp :++ '[DynamicPiece t0]))
          pRoute = snocParsedRoute parsedRoute $ DPiece dynVal
        in toDomResponse reqDyn
           $ fromDomRequest @m @route req (fromParsedRoute pRoute) reqDyn (getPathParamCtor pRoute) (apiHandler serv) page400 getDomReqUpdEv

instance ( PathParam m (ns :// piece) ~ ()
         , ParamErrToApiErr (ApiErr m (ns :// piece))
         , KnownSymbol piece
         , SingMethod m
         , WebUIServer s
         , Typeable ns
         , Typeable m
         , Typeable piece
         , UIHandler w t s m (ns :// piece)
         , ApiContract (UIInterface s) m (ns :// piece)
         , ToHeader (HeaderOut m (ns :// piece))
         , ToParam 'Cookie (CookieOut m (ns :// piece))
         , Encodings (ContentTypes m (ns :// piece)) (ApiErr m (ns :// piece))
         , Encodings (ContentTypes m (ns :// piece)) (ApiOut m (ns :// piece))
         , Monad w
         , Reflex t
         , MonadWidget t w
         , route ~ (ns :// piece)
         , FromParam 'QueryParam (QueryParam m route)
         , FormParam m route ~ ()
         , FileParam m route ~ ()
         , CookieIn m route ~ ()
         , HeaderIn m route ~ ()
         , RequestBody m route ~ '[]
         ) => Router w t s ((ns :: *) :// (piece :: Symbol)) '(m, pp) where
  route _ serv _ page400 getDomReqUpdEv reqDyn req = case pathInfo req of
    (pth : []) | symTxt (Proxy :: Proxy piece) == pth -> Matched (mkUIRequestRep (undefined :: UIRequest m route), getResponse)
    [] | T.null $ symTxt (Proxy :: Proxy piece) -> NotMatched
    _ ->  NotMatched

    where
      getResponse = toDomResponse reqDyn
                    $ fromDomRequest @m @route req () reqDyn (const ()) (apiHandler serv) page400 getDomReqUpdEv

router ::
  forall apis server m t.
  ( Router m t server apis '(CUSTOM "", '[])
  , MonadWidget t m
  ) => Proxy apis -> server -> (Dynamic t [ParamErr] -> m ()) -> ReflexDomApplication t m
router _ s page400 getDomReqUpdEv = route (Proxy :: Proxy '(apis, t)) s emptyParsedRoutes page400 getDomReqUpdEv

fromDomRequest :: forall m r w t.
  ( Functor w
  , SingMethod m
  , MonadWidget t w
  , FromParam 'QueryParam (QueryParam m r)
  , FormParam m r ~ ()
  , FileParam m r ~ ()
  , CookieIn m r ~ ()
  , HeaderIn m r ~ ()
  , RequestBody m r ~ '[]
  , Typeable r
  , Typeable m
  ) =>
  DomRequest ->
  PathParam m r ->
  Dynamic t DomRequest ->
  (DomRequest -> PathParam m r) ->
  (Dynamic t (Request m r) -> w (Response m r)) ->
  (Dynamic t [ParamErr] ->  w ()) ->
  (UIRequestRep -> Event t DomRequest) ->
  w (Event t (Response m r))
fromDomRequest initDomReq initPP domReqDyn ppFromDReq handlerFn page400 getDomReqUpdEv = do
  let
    reqRep = mkUIRequestRep (undefined :: UIRequest m r)
  let
    eReqUpdate = ffor (getDomReqUpdEv reqRep) $ \domreq -> toWebApiReqVal (ppFromDReq domreq) domreq
  reqValDyn <- holdDyn (toWebApiReqVal initPP initDomReq) eReqUpdate
  factoredErrSuccDyn <- eitherDyn reqValDyn
  -- Handles toggling of params decoding status between success & failure within same route.
  networkView $ ffor factoredErrSuccDyn $ \case
    Left errs -> do
      page400 errs
      pure (Failure $ Right $ OtherError $ toException $ BadRequestError)
    Right waReqDyn -> handlerFn waReqDyn
  where
    toWebApiReqVal :: PathParam m r -> DomRequest -> Either [ParamErr] (Request m r)
    toWebApiReqVal pp domReq =
      case (\qp ->
              Request { pathParam = pp
                      , queryParam = qp
                      , formParam = ()
                      , fileParam = ()
                      , cookieIn = ()
                      , headerIn = ()
                      , requestBody = ()
                      }
           ) <$> (fromQueryParam $ queryPathInfo domReq) of
        Validation valE -> valE

toDomResponse ::
  forall w t m r.
  ( ToHeader (HeaderOut m r)
  , ToParam 'Cookie (CookieOut m r)
  , Encodings (ContentTypes m r) (ApiOut m r)
  , Encodings (ContentTypes m r) (ApiErr m r)
  , Reflex t
  , Applicative w
  , Typeable m
  , Typeable r
  ) => Dynamic t DomRequest
  -> w (Event t (Response m r))
  -> w (Event t (DomResponse))
toDomResponse _domReq res =
  fmap (fmap go) res

  where
    go (Success _ ao _ _) = DomSuccess (mkUIRequestRep (undefined :: UIRequest m r))
    go (Failure (Left (ApiError { err }))) = DomFailure (Right "")
    go (Failure (Right (OtherError se))) = case fromException se of
      Just BadRequestError -> DomFailure (Right "Bad Request")
      _ -> DomFailure (Right "")

apiHandler ::
  ( UIHandler w t s m r
  ) => s -> Dynamic t (Request m r) -> w (Response m r)
apiHandler = handler

data BadRequestError = BadRequestError
  deriving (Show)

instance Exception BadRequestError

toUIApplication ::
  forall t app m r meth ac mp.
  ( DomBuilder t m
  , Monad m
  , Reflex t
  , MonadRouted t m
  , MonadFix m
  , MonadHold t m
  , PostBuild t m
  , MkPathFormatString (app :// r)
  , ToParam 'PathParam (PathParam meth (app :// r))
  , ToParam 'QueryParam (QueryParam meth (app :// r))
  , ('ApiMount ac mp) ~ MountPoint app
  , KnownSymbol mp
  ) => UIRequest meth (app:// r) -> m () -> ReflexDomApplication t m -> m (Event t DomResponse)
toUIApplication r@UIRequest { uiPathParam, uiQueryParam } page404 app = withPathSegment $ \dPathInfo -> do
  (initSegs, _) <- sample $ current dPathInfo
  pb <- getPostBuild
  if isRoot initSegs && (not $ T.null mountPath)
    then redirectInternal (mountPath <$ pb)
    else pure ()
  res <- networkView =<<
    (do
        domReqMayDyn <- maybeDyn (go <$> dPathInfo)
        rec
          dRepMay'Handler <- pure $ do
            domReqMayDyn >>= \case
              Nothing -> pure (Nothing, Nothing <$ page404)
              Just domReqDyn -> fmap (\domreq -> (domreq, app eNewRouteRep domReqDyn domreq)) domReqDyn >>= \case
                (_, NotMatched) -> pure (Nothing, Nothing <$ page404)
                (domreq, Matched (reqRep, dom)) -> pure (Just (reqRep, domreq), Just <$> dom)
          let eNewRouteRep uirep = fforMaybe (updated $ fmap fst dRepMay'Handler) $ \case
                Just (reqRep, domreq) -> if reqRep == uirep
                                         then Just domreq
                                         else Nothing
                Nothing -> Nothing
        dUniqRepMay'Handler <- holdUniqDynBy (\old new -> (fst <$> fst old) == (fst <$> fst new)) dRepMay'Handler
        pure $ fmap snd dUniqRepMay'Handler
    )
  switchHold never $ fmapMaybe id res
  where
    isRoot [] = True
    isRoot [""] = True
    isRoot _ = False
    mountPath = T.pack $ symbolVal (Proxy :: Proxy mp)
    go (pSegs, qps) =
      let reqParMay = case (filter (not . T.null) pSegs, T.null mountPath)  of
            ([], True) -> Just (routePaths r uiPathParam, toQueryParam uiQueryParam)
            (xs, True) -> Just (xs, go0 qps)
            ([], False) -> Nothing
            ([mp], False) | mp == mountPath -> Just (routePaths r uiPathParam, toQueryParam uiQueryParam)
            (mp:xs, False) | mp == mountPath -> Just (xs, go0 qps)
                           | otherwise -> Nothing
      in fmap (\(segs, qps0) -> DomRequest segs qps0) reqParMay

    go0 =
        map (\(k, v) -> (T.encodeUtf8 k, pure $ T.encodeUtf8 v))

uiApp :: forall (t :: *) server m app r meth ac mp.
  ( MonadWidget t m
  , MkPathFormatString (app :// r)
  , ToParam 'PathParam (PathParam meth (app :// r))
  , ToParam 'QueryParam (QueryParam meth (app :// r))
  , Router (RouteT t m) t server (Apis (UIInterface server)) '(CUSTOM "", '[])
  , app ~ (UIInterface server)
  , ('ApiMount ac mp) ~ MountPoint app
  , KnownSymbol mp
  ) => UIRequest meth (app :// r) -- ^ Default route
    -> Maybe (Event t (AppLink (UIInterface server))) -- ^ Global Nav
    -> RouteT t m () -- ^ Page 404
    -> (Dynamic t [ParamErr] -> RouteT t m ()) -- ^ Page 400
    -> server
    -> m (Event t DomResponse)
uiApp def globalNavMay page404 page400 server =
  routeApp "" $ do
  maybe (pure ()) navigateToAppLink globalNavMay
  toUIApplication def page404 $ router (Proxy :: Proxy (Apis (UIInterface server))) server page400

-- Shared: Server Router

symTxt :: KnownSymbol sym => proxy sym -> Text
symTxt sym = T.pack (symbolVal sym)

emptyParsedRoutes :: ParsedRoute t '(CUSTOM "", '[])
emptyParsedRoutes = Nil Proxy


data PieceType :: * -> * -> * where
  SPiece  :: Proxy (p :: Symbol) -> PieceType t (StaticPiece p)
  NSPiece :: Proxy (ns :: *) -> PieceType t (Namespace ns)
  DPiece  :: !val -> PieceType t (DynamicPiece val)

data ParsedRoute :: * -> (*, [*]) -> * where
  Nil              :: Proxy method -> ParsedRoute t '(method, '[])
  ConsStaticPiece  :: Proxy (p :: Symbol) -> ParsedRoute t '(method, ps) -> ParsedRoute t '(method, ((StaticPiece p) ': ps))
  ConsNSPiece      :: Proxy (ns :: *) -> ParsedRoute t '(method, ps) -> ParsedRoute t '(method, ((Namespace ns) ': ps))
  ConsDynamicPiece :: !v -> ParsedRoute t '(method, ps) -> ParsedRoute t '(method, ((DynamicPiece v) ': ps))

data RouteResult a =
  NotMatched | Matched a
  deriving (Show, Eq)

-- NOTE: We pick the leftmost
instance Semigroup (RouteResult a) where
  NotMatched <> m = m
  Matched a <> _ = Matched a

type family MarkDyn (pp :: *) :: * where
  MarkDyn (p1 :/ t)  = (p1 :/ t)
  MarkDyn (p :// t)  = (p :// t)
  MarkDyn (t :: *)   = DynamicPiece t

snocParsedRoute :: ParsedRoute t '(method, ps) -> PieceType t pt -> ParsedRoute t '(method, ps :++ '[pt])
snocParsedRoute nil@Nil{} (SPiece sym)   = sym `ConsStaticPiece` nil
snocParsedRoute nil@Nil{} (NSPiece prov) = prov `ConsNSPiece` nil
snocParsedRoute nil@Nil{} (DPiece val)   = val `ConsDynamicPiece` nil
snocParsedRoute (ConsStaticPiece sym routes) pt  = (ConsStaticPiece sym $ snocParsedRoute routes pt)
snocParsedRoute (ConsNSPiece prov routes) pt     = (ConsNSPiece prov $ snocParsedRoute routes pt)
snocParsedRoute (ConsDynamicPiece sym routes) pt = (ConsDynamicPiece sym $ snocParsedRoute routes pt)

fromParsedRoute :: ((PathParam m (FromPieces pths) ~ HListToTuple (FilterDynP pths)))
                  => ParsedRoute t '(m, pths) -> PathParam m (FromPieces pths)
fromParsedRoute proutes = fromParsedRoute' (dropStaticPiece proutes) proutes

fromParsedRoute' :: ((PathParam m (FromPieces pths) ~ HListToTuple (FilterDynP pths)))
                  => HList t (FilterDynP pths) -> ParsedRoute t '(m, pths) -> PathParam m (FromPieces pths)
fromParsedRoute' pathHL _proutes = case pathHL of
  HNil -> ()
  p1 :* HNil -> p1
  p1 :* p2 :* HNil -> (p1, p2)
  p1 :* p2 :* p3 :* HNil -> (p1, p2, p3)
  p1 :* p2 :* p3 :* p4 :* HNil -> (p1, p2, p3, p4)
  p1 :* p2 :* p3 :* p4 :* p5 :* HNil -> (p1, p2, p3, p4, p5)
  p1 :* p2 :* p3 :* p4 :* p5 :* p6 :* HNil -> (p1, p2, p3, p4, p5, p6)
  p1 :* p2 :* p3 :* p4 :* p5 :* p6 :* p7 :* HNil -> (p1, p2, p3, p4, p5, p6, p7)
  p1 :* p2 :* p3 :* p4 :* p5 :* p6 :* p7 :* p8 :* HNil -> (p1, p2, p3, p4, p5, p6, p7, p8)
  p1 :* p2 :* p3 :* p4 :* p5 :* p6 :* p7 :* p8 :* p9 :* HNil -> (p1, p2, p3, p4, p5, p6, p7, p8, p9)
  _ -> error "Panic: Unable to parse routes. Only 25 path parameter are supported"

dropStaticPiece :: ParsedRoute t '(m, pths) -> HList t (FilterDynP pths)
dropStaticPiece (Nil _)                 = HNil
dropStaticPiece (ConsStaticPiece _ ps)  = dropStaticPiece ps
dropStaticPiece (ConsNSPiece _ ps)      = dropStaticPiece ps
dropStaticPiece (ConsDynamicPiece p ps) = p :* dropStaticPiece ps

getPathParamCtor :: (HasCallStack, AllDecodeParam (FilterDynP pths), (PathParam m (FromPieces pths) ~ HListToTuple (FilterDynP pths))) => ParsedRoute t '(m, pths) -> DomRequest -> PathParam m (FromPieces pths)
getPathParamCtor proutes domreq = fromParsedRoute' (parseDynPiece (pathInfo domreq) proutes) proutes
  where
    unsafeDecodePar :: forall v.DecodeParam v => v -> Text -> v
    unsafeDecodePar _ pp = fromMaybe (error "Panic: Unreachable code") (decodeParam @v (encodeUtf8 pp))
    parseDynPiece :: (AllDecodeParam (FilterDynP pths)) => [Text] -> ParsedRoute t '(m, pths) -> HList t (FilterDynP pths)
    parseDynPiece _ (Nil _) = HNil
    parseDynPiece (_:pths) (ConsStaticPiece _ ps) = parseDynPiece pths ps
    parseDynPiece pths (ConsNSPiece _ ps)     = parseDynPiece pths ps
    parseDynPiece (p : pths) (ConsDynamicPiece _v ps) = unsafeDecodePar _v p :* parseDynPiece pths ps

type family AllDecodeParam (dpcs :: [*]) :: Constraint where
  AllDecodeParam '[] = ()
  AllDecodeParam (t ': ts) = (DecodeParam t, AllDecodeParam ts)

data HList :: * -> [*] -> * where
  HNil :: HList t '[]
  (:*) :: !a -> HList t as -> HList t (a ': as)
infixr 5 :*

-- Compact server
data CompactUIServer (api :: *) (server :: *) = CompactUIServer server

instance (WebApi api) => WebUIServer (CompactUIServer api s) where
  type UIInterface (CompactUIServer api s) = api

instance ( ApiContract api m r
         , opname ~ GetOpIdName api (OperationId m r)
         , HasField (GetOpIdName api (OperationId m r)) server handler
         , handler ~ (Dynamic t (Request m r) -> w (Response m r))
         -- , UnifyHandler (handler == (Dynamic t (Request m r) -> (Dynamic t (w (Response m r))))) server opname handler (Dynamic t (Request m r) -> Dynamic t (w (Response m r)))
         ) => UIHandler w t (CompactUIServer api server) m r where
  handler (CompactUIServer server) req = hdl req
    -- (unifyHandler @((handler == (Dynamic t (Request m r) -> Dynamic t (w (Response m r))))) @server @opname $ hdl) req

    where
      hdl :: handler
      hdl = getField @(GetOpIdName api (OperationId m r)) server

class UnifyHandler (isEq :: Bool) (server :: *) (fn :: Symbol) handlerAct handlerExp where
  unifyHandler :: handlerAct -> handlerExp

instance (handlerAct ~ handlerExp) => UnifyHandler 'True s fn handlerAct handlerExp where
  unifyHandler = id
  {-# INLINE unifyHandler #-}

instance (TypeError
          ( 'Text "Type mismatch in the handler field of server: " ':<>: 'ShowType server ':$$:
            'Text "Expected: " ':<>: ('Text fn) ':<>: 'Text " :: " ':<>: 'ShowType handlerExp ':$$:
            'Text "Actual: " ':<>: ('Text fn) ':<>: 'Text " :: " ':<>: 'ShowType handlerAct
          )) => UnifyHandler 'False server fn handlerAct handlerExp where
  unifyHandler = error "Panic: Unreachable code"
  {-# INLINE unifyHandler #-}

type family GetOpIdName api (opId :: OpId) :: Symbol where
  GetOpIdName _ ('OpId _ n) = n
  GetOpIdName api ('UndefinedOpId m r) = TypeError ('Text "Compact Server requires OperationId to be defined for every ApiContract instance of " ':<>: 'ShowType api ':$$:
                                                     'Text "Fix: Define OperationId for instance ApiContract " ':<>: 'ShowType m ':<>: 'Text " (" ':<>: 'ShowType r ':<>: 'Text ")" ':$$:
                                                     'Text "Example: type OperationId " ':<>: 'ShowType m ':<>: 'Text " (" ':<>: 'ShowType r ':<>: 'Text ") = 'OpId " ':<>: 'ShowType api ':<>: 'Text " <operation-name>"
                                               )


-- Request Builder
{-
data RequestP pp qp fp hd fu bd = RequestP
  { pathParam   :: pp
  , queryParam  :: qp
  , formParam   :: fp
  , headerIn    :: hd
  , fileParam   :: fu
  , requestBody :: bd
  }

emptyReq :: RequestP () () () () () ()
emptyReq = RequestP
  { queryParam  = ()
  , formParam   = ()
  , headerIn    = ()
  , pathParam   = ()
  , fileParam   = ()
  , requestBody = ()
  }

mkReq :: RequestP pp qp fp hd fu db -> Request m r
mkReq RequestP {..} = Request {..}

-- mkReq emptyReq {query = foo, form = bar}


class MkReq t m r where
  mkReq :: t -> Request m r

instance {-# OVERLAPPABLE #-} MkReq (p -> r) m r where
  mkReq = undefined
  {-# INLINE mkReq #-}

instance {-# OVERLAPPING #-} (m ~ m1, r ~ r1) => MkReq (Request m1 r1) m r where
  mkReq = id
  {-# INLINE mkReq #-}

-}

data UIRequest m r =
  UIRequest { uiPathParam :: PathParam m r
            , uiQueryParam :: QueryParam m r
            }

defUIRequest :: forall m r. PathParam m r -> QueryParam m r -> UIRequest m r
defUIRequest uiPathParam uiQueryParam =
  UIRequest { uiPathParam, uiQueryParam }

modifyUIRequest :: (Reflex t, SingMethod meth) => Dynamic t (Request meth r) -> Event t a -> (a -> Request meth r -> Request meth r) -> Event t (UIRequest meth r)
modifyUIRequest reqDyn e setter =
  (\Request {pathParam, queryParam}
    -> UIRequest { uiPathParam = pathParam
                 , uiQueryParam = queryParam}
  ) <$> attachPromptlyDynWith (flip setter) reqDyn e


-- | Navigate to the specified route
navigate ::
  ( Monad m
  , Reflex t
  , MonadRouted t m
  , MkPathFormatString (app://r)
  , ToParam 'QueryParam (QueryParam meth (app://r))
  , ToParam 'PathParam (PathParam meth (app://r))
  , ('ApiMount ac mp) ~ MountPoint app
  , KnownSymbol mp
  ) => Event t (UIRequest meth (app://r)) -> m ()
navigate = redirectInternal . fmap linkText

linkText :: forall app meth r ac mp.
  ( MkPathFormatString (app://r)
  , ToParam 'QueryParam (QueryParam meth (app://r))
  , ToParam 'PathParam (PathParam meth (app://r))
  , ('ApiMount ac mp) ~ MountPoint app
  , KnownSymbol mp
  ) => UIRequest meth (app://r) -> Text
linkText r@UIRequest { uiPathParam, uiQueryParam } =
  T.decodeUtf8 $ W.link r startupURI uiPathParam (Just uiQueryParam)
  where startupURI = case symbolVal (Proxy :: Proxy mp) of
          "" -> Nothing
          uri -> Just $ Char8.pack uri

respond ::
  ( HeaderOut meth r ~ ()
  , CookieOut meth r ~ ()
  , ApiOut meth r ~ ()
  , Applicative m
  ) => ApiOut meth r -> m (Response meth r)
respond out =
  pure (Success status200 out () ())

raise ::
  ( HeaderOut meth r ~ ()
  , CookieOut meth r ~ ()
  , Applicative m
  , ApiErr meth r ~ ()
  ) => Status -> ApiErr meth r -> m (Response meth r)
raise status err =
  pure (Failure (Left (ApiError { code = status, err = err, headerOut = Nothing, cookieOut = Nothing })))


newtype AppLink app = AppLink {getAppLink :: Text}

appLink ::
  ( MkPathFormatString (app://r)
  , ToParam 'QueryParam (QueryParam meth (app://r))
  , ToParam 'PathParam (PathParam meth (app://r))
  , ('ApiMount ac mp) ~ MountPoint app
  , KnownSymbol mp
  ) => UIRequest meth (app://r)
  -> AppLink app
appLink uireq = (AppLink . linkText) uireq

navigateToAppLink :: (Reflex t, MonadRouted t m, Monad m) => Event t (AppLink app) -> m ()
navigateToAppLink = redirectInternal . fmap getAppLink

data ApiMount = ApiMount Type Symbol
type family MountPoint (app :: Type) = (r :: ApiMount) | r -> app
