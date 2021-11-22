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
{-# LANGUAGE AllowAmbiguousTypes       #-}
module WebApi.Reflex.Dom.Router where

import           Control.Monad.Fix
import           Data.Functor
import           Data.Kind
import           Data.Proxy
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Text.Encoding (encodeUtf8)
import           Data.Typeable
import qualified Debug.Trace as DT
import           GHC.TypeLits
import           Network.HTTP.Types.Status
import           Reflex hiding (Request, Response, HList(..))
import           Reflex.Dom.Contrib.MonadRouted
import           Reflex.Dom.Core hiding ( Request, Response, Namespace, HList (..) )
import           Reflex.Network ( networkView, networkHold )
import           WebApi.ContentTypes
import           WebApi.Contract
import           WebApi.Param
import           WebApi.Util

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
  -- type HandlerM s :: * -> *

class UIHandler w (t :: *) s m r where
  handler :: Proxy s -> Dynamic t (Request m r) -> Dynamic t (w (Response m r))

data DomRequest = DomRequest {pathInfo :: [Text]}
data DomResponse =
  DomResponse
  deriving (Show)
  
type ReflexDomApplication t m a = Dynamic t DomRequest -> (Dynamic t (m (RouteResult DomResponse)) -> m a) -> m a

class Monad w => Router (w :: * -> *) (t :: *) (server :: *) (r :: k) (pr :: (*, [*])) where
  route :: Proxy '(r, t) -> server -> ParsedRoute pr -> ReflexDomApplication t w a


{-
-- Dynamic t DomRequest -> m () -- (Dynamic t (RouteResult DomResponse))

dynRes <- route ...
dynRes2 <- route ...


-}

instance
  ( Router w t s route pr
  , Router w t s routes pr
  , Reflex t
  ) => Router w t s ((route :: *) ': routes) pr where
  route _ _s parsedRoute request respond =
    route (Proxy :: Proxy '(route, t)) _s parsedRoute request (fmap go)

    where
      go ma = do
        domRes <- ma
        case domRes of
          Matched a -> respond $ Matched a
          NotMatched -> route (Proxy :: Proxy '(routes, t)) _s parsedRoute request respond

instance (Monad w, Reflex t) => Router w t s '[] pr where
  route _ _s _ _ respond = respond never

instance (Monad w, Router w t s rest '(m, pp :++ '[Namespace ns])) => Router w t s ((ns :: *) :// (rest :: *)) '(m, pp) where
  route _ _s parsedRoute request respond = route (Proxy :: Proxy '(rest, t)) _s (snocParsedRoute parsedRoute $ NSPiece (Proxy :: Proxy ns)) request respond

instance (Monad w, Router w t s (MarkDyn rest) '(m, (pp :++ '[DynamicPiece piece])), DecodeParam piece)
                      => Router w t s ((piece :: *) :/ (rest :: *)) '(m, pp) where
  route _ _s parsedRoute request respond = undefined
{-    
    case pathInfo request of
      (lpth : rpths)  -> case (decodeParam (encodeUtf8 lpth) :: Maybe piece) of
        Just dynPiece -> route (Proxy :: Proxy '((MarkDyn rest), t)) _s (snocParsedRoute parsedRoute $ DPiece dynPiece) request {pathInfo = rpths} respond
        Nothing -> respond NotMatched
      _ -> respond $ NotMatched  
-}
    
instance (Monad w, Router w t s (MarkDyn rest) '(m, (pp :++ '[StaticPiece piece])), KnownSymbol piece)
  => Router w t s ((piece :: Symbol) :/ (rest :: *)) '(m, pp) where
  route _ _s parsedRoute request respond = undefined
{-  
    case pathInfo request of
      (lpth : rpths) | lpieceTxt == lpth -> route (Proxy :: Proxy '((MarkDyn rest), t)) _s (snocParsedRoute parsedRoute $ SPiece (Proxy :: Proxy piece)) request {pathInfo = rpths} respond
      _ -> respond $ NotMatched
    where lpieceTxt = symTxt (Proxy :: Proxy piece)
-}
-- Base Cases

instance ( KnownSymbol lpiece
         , KnownSymbol rpiece
         , paths ~ (pp :++ '[StaticPiece lpiece, StaticPiece rpiece])
         , paths ~ ((pp :++ '[StaticPiece lpiece]) :++ '[StaticPiece rpiece])
         , route ~ (FromPieces paths)
         , UIHandler w t s m route
         , PathParam m route ~ HListToTuple (FilterDynP paths)
         , Encodings (ContentTypes m route) (ApiErr m route)
         , Encodings (ContentTypes m route) (ApiOut m route)
         , ToHeader (HeaderOut m route)
         , ToParam 'Cookie (CookieOut m route)
         , ParamErrToApiErr (ApiErr m route)
--         , FromWaiRequestCtx m route
         , Typeable m
         , Typeable route
         , WebUIServer s
         , Monad w
         , SingMethod m
         ) => Router w t s ((lpiece :: Symbol) :/ (rpiece :: Symbol)) '(m, pp) where
  route _ serv parsedRoute request respond = undefined
{-  
    case pathInfo request of
      (lpth : rpth : []) | lpieceTxt == lpth && rpieceTxt == rpth -> respond . Matched =<< getResponse
      _ -> respond NotMatched
    where lpieceTxt = symTxt (Proxy :: Proxy lpiece)
          rpieceTxt = symTxt (Proxy :: Proxy rpiece)
          pRoute :: ParsedRoute '(m, paths)
          pRoute = snocParsedRoute (snocParsedRoute parsedRoute $ SPiece (Proxy :: Proxy lpiece)) $ SPiece (Proxy :: Proxy rpiece)
          pathPar = fromParsedRoute pRoute
          wbReq = DomRequest undefined
          getResponse = do
            toDomResponse request <$> fromDomRequest request pathPar (\(req :: Request m route) -> toIO serv wbReq $ apiHandler (Proxy :: Proxy s) (pure req))
-}

instance ( KnownSymbol rpiece
         , paths ~ (pp :++ '[DynamicPiece lpiece, StaticPiece rpiece])
         , paths ~ ((pp :++ '[DynamicPiece lpiece]) :++ '[StaticPiece rpiece])
         , route ~ (FromPieces paths)
         , UIHandler w t s m route
         , PathParam m route ~ HListToTuple (FilterDynP paths)
         , Encodings (ContentTypes m route) (ApiErr m route)
         , Encodings (ContentTypes m route) (ApiOut m route)
         , ToHeader (HeaderOut m route)
         , ToParam 'Cookie (CookieOut m route)
         , DecodeParam lpiece
         , ParamErrToApiErr (ApiErr m route)
--         , FromWaiRequestCtx m route
         , Typeable m
         , Typeable route
         , WebUIServer s
         , Monad w
         , SingMethod m
         ) => Router w t s ((lpiece :: *) :/ (rpiece :: Symbol)) '(m, pp) where
  route _ serv parsedRoute request respond = undefined
{-  
    case pathInfo request of
      (lpth : rpth : []) | rpieceTxt == rpth -> case (decodeParam (encodeUtf8 lpth) :: Maybe lpiece) of
        Just dynVal -> respond . Matched =<< getResponse dynVal
        Nothing     -> respond NotMatched
      _ -> respond NotMatched
    where rpieceTxt = symTxt (Proxy :: Proxy rpiece)
          getResponse dynVal = do
            let pRoute :: ParsedRoute '(m, paths)
                pRoute = snocParsedRoute (snocParsedRoute parsedRoute $ DPiece dynVal) $ SPiece (Proxy :: Proxy rpiece)
                wbReq = DomRequest undefined
            apiResp' <- fromDomRequest request (fromParsedRoute pRoute) (\(req :: Request m route) -> toIO serv wbReq $ apiHandler req)
            response <- case apiResp' of
              Validation (Right apiResp) -> return apiResp
              Validation (Left errs) -> return $ Failure $ Left $ ApiError badRequest400 (toApiErr errs) Nothing Nothing
            return $ toDomResponse request response
-}

instance ( route ~ (FromPieces (pp :++ '[DynamicPiece t]))
         , UIHandler w t s m route
         , PathParam m route ~ HListToTuple (FilterDynP (pp :++ '[DynamicPiece t]))
         , Encodings (ContentTypes m route) (ApiErr m route)
         , Encodings (ContentTypes m route) (ApiOut m route)
         , ToHeader (HeaderOut m route)
         , ToParam 'Cookie (CookieOut m route)
         , DecodeParam t
         , ParamErrToApiErr (ApiErr m route)
--         , FromWaiRequestCtx m route
         , Typeable m
         , Typeable route
         , WebUIServer s
         , Monad w
         , SingMethod m
         ) => Router w t s (DynamicPiece t) '(m, pp) where
  route _ serv parsedRoute request respond = undefined {-
    case pathInfo request of
      (lpth : []) -> case (decodeParam (encodeUtf8 lpth) :: Maybe t) of
        Just dynVal -> respond . Matched =<< getResponse dynVal
        Nothing     -> respond NotMatched
      _           -> respond NotMatched
    where getResponse dynVal = do
            let pRoute :: ParsedRoute '(m, (pp :++ '[DynamicPiece t]))
                pRoute = snocParsedRoute parsedRoute $ DPiece dynVal
                wbReq = DomRequest undefined
            apiResp' <- fromDomRequest request (fromParsedRoute pRoute) (\(req :: Request m route) -> toIO serv wbReq $ apiHandler req)
            response <- case apiResp' of
              Validation (Right apiResp) -> return apiResp
              Validation (Left errs) -> return $ Failure $ Left $ ApiError badRequest400 (toApiErr errs) Nothing Nothing
            return $ toDomResponse request response
-}

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
--         , FromWaiRequestCtx m (ns :// piece)
         , Encodings (ContentTypes m (ns :// piece)) (ApiErr m (ns :// piece))
         , Encodings (ContentTypes m (ns :// piece)) (ApiOut m (ns :// piece))
         , Monad w
         ) => Router w t s ((ns :: *) :// (piece :: Symbol)) '(m, pp) where
  route _ serv _ request respond = undefined
{-  
    case pathInfo request of
      (pth : []) | symTxt (Proxy :: Proxy piece) == pth -> respond . Matched =<< getResponse
      [] | T.null $ symTxt (Proxy :: Proxy piece) -> respond . Matched =<< getResponse
      _ -> respond $ NotMatched
    where getResponse = do
            let wbReq = DomRequest undefined
            apiResp' <- fromDomRequest request () (\(req :: Request m (ns :// piece)) -> toIO serv wbReq $ apiHandler req)
            response <- case apiResp' of
              Validation (Right apiResp) -> return apiResp 
              Validation (Left errs) -> return $ Failure $ Left $ ApiError badRequest400 (toApiErr errs) Nothing Nothing
            return $ toDomResponse request response
-}

router ::
  forall apis server m t a.
  ( Router m t server apis '(CUSTOM "", '[])
  , MonadWidget t m
  ) => Proxy apis -> server -> ReflexDomApplication t m a
router _ s = route (Proxy :: Proxy '(apis, t)) s emptyParsedRoutes

fromDomRequest :: forall w t m r.
  ( Functor w
  , SingMethod m
  , MonadWidget t w
  ) =>
  Dynamic t DomRequest ->
  (Dynamic t (Request m r) -> Dynamic t (w (Response m r))) ->
  Dynamic t (w (Validation [ParamErr] (Response m r)))
  -- w (Event t (Validation [ParamErr] (Response m r)))
fromDomRequest domReq handlerFn =
  handlerFn (go <$> domReq)

  where
    go = undefined
{-
            apiResp' <- fromDomRequest request pathPar (\(req :: Request m route) -> toIO serv wbReq $ apiHandler (Proxy :: Proxy s) (pure req))
            response <- case apiResp' of
              Validation (Right apiResp) -> return apiResp
              Validation (Left errs) -> return $ Failure $ Left $ ApiError badRequest400 (toApiErr errs)
-}

toIO :: s -> DomRequest -> eff (Response m r) -> eff (Response m r)
toIO _ _ = id

-- apiHandler :: Proxy s -> Dynamic t (Request m route) -> Dynamic t (HandlerM s (Response m r))
apiHandler ::
  ( UIHandler w t s m r
  ) => Proxy s -> Dynamic t (Request m r) -> Dynamic t (w (Response m r))
apiHandler = handler

toDomResponse ::
  ( ToHeader (HeaderOut m r)
  , ToParam 'Cookie (CookieOut m r)
  , Encodings (ContentTypes m r) (ApiOut m r)
  , Encodings (ContentTypes m r) (ApiErr m r)
  , Reflex t
  ) => Dynamic t DomRequest -> Event t (Validation [ParamErr] (Response m r)) -> Event t DomResponse
toDomResponse _domReq res =
  fmap go res

  where
    go = undefined

toUIApplication ::
  ( DomBuilder t m
  , Monad m
  , Reflex t
  , MonadRouted t m
  , MonadFix m
  , MonadHold t m
  -- , EventWriter t (RouteResult DomResponse) m
  ) => ReflexDomApplication t m (Event t (RouteResult DomResponse)) -> m (Event t (RouteResult DomResponse))
toUIApplication app = withPathSegment $ \dPathInfo -> do
  app (go <$> dPathInfo) pure

  where
    go (pSegMay, qps) =
      DT.trace ("pSegMay: " <> show pSegMay) (DomRequest [])

uiApp :: forall (t :: *) server m.
  ( Router (RouteT t m) t server (Apis (UIInterface server)) '(CUSTOM "", '[])
  , MonadWidget t m
  -- , EventWriter t (RouteResult DomResponse) m
  ) => server -> m (Event t (RouteResult DomResponse))
uiApp server =
  routeApp "" $ toUIApplication $ router (Proxy :: Proxy (Apis (UIInterface server))) server


-- Shared: Server Router

symTxt :: KnownSymbol sym => proxy sym -> Text
symTxt sym = T.pack (symbolVal sym)

emptyParsedRoutes :: ParsedRoute '(CUSTOM "", '[])
emptyParsedRoutes = Nil Proxy


data PieceType :: * -> * where
  SPiece  :: Proxy (p :: Symbol) -> PieceType (StaticPiece p)
  NSPiece :: Proxy (ns :: *) -> PieceType (Namespace ns)
  DPiece  :: !val -> PieceType (DynamicPiece val)

data ParsedRoute :: (*, [*]) -> * where
  Nil              :: Proxy method -> ParsedRoute '(method, '[])
  ConsStaticPiece  :: Proxy (p :: Symbol) -> ParsedRoute '(method, ps) -> ParsedRoute '(method, ((StaticPiece p) ': ps))
  ConsNSPiece      :: Proxy (ns :: *) -> ParsedRoute '(method, ps) -> ParsedRoute '(method, ((Namespace ns) ': ps))
  ConsDynamicPiece :: !t -> ParsedRoute '(method, ps) -> ParsedRoute '(method, ((DynamicPiece t) ': ps))

data RouteResult a =
  NotMatched | Matched a
  deriving (Show, Eq)

type family MarkDyn (pp :: *) :: * where
  MarkDyn (p1 :/ t)  = (p1 :/ t)
  MarkDyn (p :// t)  = (p :// t)
  MarkDyn (t :: *)   = DynamicPiece t
  
snocParsedRoute :: ParsedRoute '(method, ps) -> PieceType pt -> ParsedRoute '(method, ps :++ '[pt])
snocParsedRoute nil@Nil{} (SPiece sym)   = sym `ConsStaticPiece` nil
snocParsedRoute nil@Nil{} (NSPiece prov) = prov `ConsNSPiece` nil
snocParsedRoute nil@Nil{} (DPiece val)   = val `ConsDynamicPiece` nil
snocParsedRoute (ConsStaticPiece sym routes) pt  = (ConsStaticPiece sym $ snocParsedRoute routes pt)
snocParsedRoute (ConsNSPiece prov routes) pt     = (ConsNSPiece prov $ snocParsedRoute routes pt)
snocParsedRoute (ConsDynamicPiece sym routes) pt = (ConsDynamicPiece sym $ snocParsedRoute routes pt)

fromParsedRoute :: (PathParam m (FromPieces pths) ~ HListToTuple (FilterDynP pths))
                  => ParsedRoute '(m, pths) -> PathParam m (FromPieces pths)
fromParsedRoute proutes = case dropStaticPiece proutes of
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

dropStaticPiece :: ParsedRoute '(m, pths) -> HList (FilterDynP pths)
dropStaticPiece (Nil _)                 = HNil
dropStaticPiece (ConsStaticPiece _ ps)  = dropStaticPiece ps
dropStaticPiece (ConsNSPiece _ ps)      = dropStaticPiece ps
dropStaticPiece (ConsDynamicPiece p ps) = p :* dropStaticPiece ps

data HList :: [*] -> * where
  HNil :: HList '[]
  (:*) :: !a -> HList as -> HList (a ': as)
infixr 5 :*


-- Compact server
{-
data CompactUIServer t (api :: *) (server :: (* -> *) -> *) (eff :: * -> *) = CompactUIServer (forall a.() -> eff a -> IO a) (server eff)

instance (WebApi api{-, MonadCatch eff, MonadIO eff-}) => WebUIServer (CompactUIServer t api s eff) where
  type HandlerM (CompactUIServer t api s eff) = eff
  -- type ApiInterface (CompactUIServer t api s eff) = api
  -- toIO (CompactUIServer toIO' _) = toIO'

instance ( ApiContract api m r
         , opname ~ GetOpIdName api (OperationId m r)
         , HasField (GetOpIdName api (OperationId m r)) (server eff) handler
         , UnifyHandler (handler == (Request m r -> eff (Response m r))) server opname handler (Dynamic t (Request m r) -> Dynamic t (eff (Response m r)))
         ) => UIHandler (CompactUIServer t api server eff) m r where
  handler _ = unifyHandler @((handler == (Request m r -> eff (Response m r)))) @server @opname $ getField @(GetOpIdName api (OperationId m r)) (undefined :: server eff)

class UnifyHandler (isEq :: Bool) (server :: (* -> *) -> *) (fn :: Symbol) handlerAct handlerExp where
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

-}
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

