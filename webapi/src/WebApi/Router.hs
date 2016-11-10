{-|

Module      : WebApi.Contract
License     : BSD3
Stability   : experimental
-}

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
module WebApi.Router
       ( -- * Route types
         Static
       , Root
       , (:/)
       , (://)
         
       -- * Default routing implementation
       , Route
       , Router (..)
       , router
       , ToPieces
       , FromPieces

       -- * Custom routing
       , PathSegment (..)
       , MkPathFormatString (..)
       , apiHandler

       -- * Internal
       , FilterDynP
       , StaticPiece
       , DynamicPiece
       , ParsedRoute (..)
       , PieceType (..)
       , fromParsedRoute
       , snocParsedRoute
       , symTxt
       ) where

import Control.Exception (SomeException (..))
import Control.Monad.Catch (catches, Handler (..), MonadCatch)
import Data.Proxy
import Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Data.Typeable (Typeable)
import GHC.TypeLits
import Network.HTTP.Types hiding (Query)
import Network.Wai (pathInfo, requestMethod)
import WebApi.ContentTypes
import WebApi.Contract
import WebApi.Internal
import WebApi.Param
import WebApi.Util

-- | Datatype representing a endpoint.
data Route (ms :: [*]) (r :: *)

data StaticPiece (s :: Symbol)

data DynamicPiece (t :: *)

data Namespace (ns :: *)

-- | Datatype associating a provider with a route.
data (://) (ns :: *) (ps :: k)
infixr 5 ://

-- | Datatype representing a route.
data (:/) (p1 :: k) (p2 :: k1)
infixr 5 :/

type instance PathParam' m (Static s) = ()
type instance PathParam' m (p1 :/ p2) = HListToTuple (FilterDynP (ToPieces (p1 :/ p2)))
type instance PathParam' m (p :// (ps :: *)) = HListToTuple (FilterDynP (ToPieces ps))
type instance PathParam' m (p :// (ps :: Symbol)) = ()

-- | Datatype representing a static path piece.
data Static (s :: Symbol)

type Root = Static ""


data PieceType :: * -> * where
  SPiece  :: Proxy (p :: Symbol) -> PieceType (StaticPiece p)
  NSPiece :: Proxy (ns :: *) -> PieceType (Namespace ns)
  DPiece  :: !val -> PieceType (DynamicPiece val)

data ParsedRoute :: (*, [*]) -> * where
  Nil              :: Proxy method -> ParsedRoute '(method, '[])
  ConsStaticPiece  :: Proxy (p :: Symbol) -> ParsedRoute '(method, ps) -> ParsedRoute '(method, ((StaticPiece p) ': ps))
  ConsNSPiece      :: Proxy (ns :: *) -> ParsedRoute '(method, ps) -> ParsedRoute '(method, ((Namespace ns) ': ps))
  ConsDynamicPiece :: !t -> ParsedRoute '(method, ps) -> ParsedRoute '(method, ((DynamicPiece t) ': ps))

data HList :: [*] -> * where
  HNil :: HList '[]
  (:*) :: !a -> HList as -> HList (a ': as)
infixr 5 :*

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

-- | Convert the path into a flat hierarchy.
type family ToPieces (r :: k) :: [*] where
  ToPieces (ns :// (ps :: *))      = Namespace ns ': ToPieces' ps
  ToPieces (ns :// (ps :: Symbol)) = Namespace ns ': ToPieces' (Static ps)
  ToPieces p                       = ToPieces' p

type family ToPieces' (r :: k) :: [*] where
  ToPieces' (Static s)                         = '[StaticPiece s]
  ToPieces' ((p1 :: Symbol) :/ (p2 :: Symbol)) = '[StaticPiece p1, StaticPiece p2]
  ToPieces' ((p1 :: *) :/ (p2 :: Symbol))      = '[DynamicPiece p1, StaticPiece p2]
  ToPieces' ((p1 :: Symbol) :/ (p2 :/ p3))     = StaticPiece p1 ': ToPieces' (p2 :/ p3)
  ToPieces' ((p1 :: *) :/ (p2 :/ p3))          = DynamicPiece p1 ': ToPieces' (p2 :/ p3)
  ToPieces' ((p1 :: *) :/ (p2 :: *))           = '[DynamicPiece p1, DynamicPiece p2]
  ToPieces' ((p1 :: Symbol) :/ (p2 :: *))      = '[StaticPiece p1, DynamicPiece p2]

type family FromPieces (pps :: [*]) :: * where
  FromPieces (Namespace ns ': ps) = ns :// FromPieces' ps
  FromPieces ps                   = FromPieces' ps

type family FromPieces' (pps :: [*]) :: * where
  FromPieces' '[StaticPiece s]                    = Static s
  FromPieces' '[StaticPiece p1, StaticPiece p2]   = p1 :/ p2
  FromPieces' '[DynamicPiece p1, DynamicPiece p2] = p1 :/ p2
  FromPieces' '[StaticPiece p1, DynamicPiece p2]  = p1 :/ p2
  FromPieces' '[DynamicPiece p1, StaticPiece p2]  = p1 :/ p2

  FromPieces' (DynamicPiece p ': ps)              = p :/ FromPieces' ps
  FromPieces' (StaticPiece p ': ps)               = p :/ FromPieces' ps

{-
  FromPieces '[StaticPiece p1, StaticPiece p2]   = p1 :/ p2
  FromPieces '[DynamicPiece p1, DynamicPiece p2] = p1 :/ p2
  FromPieces '[StaticPiece p1, DynamicPiece p2]  = p1 :/ p2
  FromPieces '[DynamicPiece p1, StaticPiece p2]  = p1 :/ p2
  FromPieces ((StaticPiece p1) ': ((StaticPiece p2) ': pps)) = p1 :/ (FromPieces ((StaticPiece p2) ': pps))
  FromPieces ((DynamicPiece p1) ': ((DynamicPiece p2) ': pps)) = p1 :/ (FromPieces ((DynamicPiece p2) ': pps))
  FromPieces ((StaticPiece p1) ': ((DynamicPiece p2) ': pps)) = p1 :/ (FromPieces ((DynamicPiece p2) ': pps))
  FromPieces ((DynamicPiece p1) ': ((StaticPiece p2) ': pps)) = p1 :/ (FromPieces ((StaticPiece p2) ': pps))
-}

type family FilterDynP (ps :: [*]) :: [*] where
  FilterDynP (DynamicPiece p1 ': p2) = p1 ': FilterDynP p2
  FilterDynP (p1 ': p2)              = FilterDynP p2
  FilterDynP '[]                     = '[]

-- | Class to do the default routing.
class Router (server :: *) (r :: k) (pr :: (*, [*])) where
  route :: Proxy r -> server -> ParsedRoute pr -> RoutingApplication

type family MarkDyn (pp :: *) :: * where
  MarkDyn (p1 :/ t)  = (p1 :/ t)
  MarkDyn (p :// t)  = (p :// t)
  MarkDyn (t :: *)   = DynamicPiece t

instance ( SingMethod m
         , Router s r '(m, '[])
         , Router s (Route ms r) pr
         ) => Router s (Route (m ': ms) r) pr where
  route _ _s parsedRoute request respond =
    case requestMethod request == meth of
      True  -> route (Proxy :: Proxy r) _s (Nil (Proxy :: Proxy m)) request respond
      False -> route (Proxy :: Proxy (Route ms r)) _s parsedRoute request respond
    where meth = singMethod (Proxy :: Proxy m)

instance Router s (Route '[] r) pr where
  route _ _s _ _request respond = respond NotMatched

instance (Router s route pr, Router s routes pr) => Router s ((route :: *) ': routes) pr where
  route _ _s parsedRoute request respond =
    route (Proxy :: Proxy route) _s parsedRoute request $ \case
      Matched a -> respond $ Matched a
      NotMatched -> route (Proxy :: Proxy routes) _s parsedRoute request respond

instance Router s '[] pr where
  route _ _s _ _ respond = respond NotMatched

instance (Router s rest '(m, pp :++ '[Namespace ns])) => Router s ((ns :: *) :// (rest :: *)) '(m, pp) where
  route _ _s parsedRoute request respond =
    route (Proxy :: Proxy rest) _s (snocParsedRoute parsedRoute $ NSPiece (Proxy :: Proxy ns)) request respond

instance (Router s (MarkDyn rest) '(m, (pp :++ '[DynamicPiece piece])), DecodeParam piece)
                      => Router s ((piece :: *) :/ (rest :: *)) '(m, pp) where
  route _ _s parsedRoute request respond =
    case pathInfo request of
      (lpth : rpths)  -> case (decodeParam (encodeUtf8 lpth) :: Maybe piece) of
        Just dynPiece -> route (Proxy :: Proxy (MarkDyn rest)) _s (snocParsedRoute parsedRoute $ DPiece dynPiece) request {pathInfo = rpths} respond
        Nothing -> respond NotMatched
      _ -> respond $ NotMatched

instance (Router s (MarkDyn rest) '(m, (pp :++ '[StaticPiece piece])), KnownSymbol piece)
  => Router s ((piece :: Symbol) :/ (rest :: *)) '(m, pp) where
  route _ _s parsedRoute request respond =
    case pathInfo request of
      (lpth : rpths) | lpieceTxt == lpth -> route (Proxy :: Proxy (MarkDyn rest)) _s (snocParsedRoute parsedRoute $ SPiece (Proxy :: Proxy piece)) request {pathInfo = rpths} respond
      _ -> respond $ NotMatched
    where lpieceTxt = symTxt (Proxy :: Proxy piece)


-- Base Cases
instance ( KnownSymbol piece, ApiHandler s m (Static piece)
         , ToHeader (HeaderOut m (Static piece))
         , ToParam 'Cookie (CookieOut m (Static piece))
         , FromParam 'QueryParam (QueryParam m (Static piece))
         , FromParam 'FormParam (FormParam m (Static piece))
         , FromParam 'FileParam (FileParam m (Static piece))
         , FromHeader (HeaderIn m (Static piece))
         , FromParam 'Cookie (CookieIn m (Static piece))
         , Encodings (ContentTypes m (Static piece)) (ApiOut m (Static piece))
         , Encodings (ContentTypes m (Static piece)) (ApiErr m (Static piece))
         , PathParam m (Static piece) ~ ()
         , ParamErrToApiErr (ApiErr m (Static piece))
         , ToHListRecTuple (StripContents (RequestBody m (Static piece)))
         , PartDecodings (RequestBody m (Static piece))
         , Typeable m
         , Typeable (Static piece)
         , WebApiServer s
         ) => Router s (Static piece) '(m, pp) where
  route _ serv _ request respond =
    case pathInfo request of
      (pth : []) | symTxt (Proxy :: Proxy piece) == pth -> respond . Matched =<< getResponse
      [] | T.null $ symTxt (Proxy :: Proxy piece) -> respond . Matched =<< getResponse
      _ -> respond $ NotMatched
    where getResponse = do
            apiResp' <- fromWaiRequest request () (\(req :: Request m (Static piece)) -> toIO serv $ apiHandler (toTagged (Proxy :: Proxy '[]) serv) req)
            response <- case apiResp' of
              Validation (Right apiResp) -> return apiResp 
              Validation (Left errs) -> return $ Failure $ Left $ ApiError badRequest400 (toApiErr errs) Nothing Nothing
            return $ toWaiResponse request response

instance ( KnownSymbol lpiece
         , KnownSymbol rpiece
         , paths ~ (pp :++ '[StaticPiece lpiece, StaticPiece rpiece])
         , paths ~ ((pp :++ '[StaticPiece lpiece]) :++ '[StaticPiece rpiece])
         , route ~ (FromPieces paths)
         , ApiHandler s m route
         , PathParam m route ~ HListToTuple (FilterDynP paths)
         , FromParam 'QueryParam (QueryParam m route)
         , FromParam 'FormParam (FormParam m route)
         , FromParam 'FileParam (FileParam m route)
         , FromParam 'Cookie (CookieIn m route)
         , FromHeader (HeaderIn m route)
         , Encodings (ContentTypes m route) (ApiErr m route)
         , Encodings (ContentTypes m route) (ApiOut m route)
         , ToHeader (HeaderOut m route)
         , ToParam 'Cookie (CookieOut m route)
         , ParamErrToApiErr (ApiErr m route)
         , ToHListRecTuple (StripContents (RequestBody m route))
         , PartDecodings (RequestBody m route)
         , Typeable m
         , Typeable route
         , WebApiServer s
         ) => Router s ((lpiece :: Symbol) :/ (rpiece :: Symbol)) '(m, pp) where
  route _ serv parsedRoute request respond =
    case pathInfo request of
      (lpth : rpth : []) | lpieceTxt == lpth && rpieceTxt == rpth -> respond . Matched =<< getResponse
      _ -> respond NotMatched
    where lpieceTxt = symTxt (Proxy :: Proxy lpiece)
          rpieceTxt = symTxt (Proxy :: Proxy rpiece)
          pRoute :: ParsedRoute '(m, paths)
          pRoute = snocParsedRoute (snocParsedRoute parsedRoute $ SPiece (Proxy :: Proxy lpiece)) $ SPiece (Proxy :: Proxy rpiece)
          pathPar = fromParsedRoute pRoute
          getResponse = do
            apiResp' <- fromWaiRequest request pathPar (\(req :: Request m route) -> toIO serv $ apiHandler (toTagged (Proxy :: Proxy '[]) serv) req)
            response <- case apiResp' of
              Validation (Right apiResp) -> return apiResp
              Validation (Left errs) -> return $ Failure $ Left $ ApiError badRequest400 (toApiErr errs) Nothing Nothing
            return $ toWaiResponse request response

instance ( KnownSymbol rpiece
         , paths ~ (pp :++ '[DynamicPiece lpiece, StaticPiece rpiece])
         , paths ~ ((pp :++ '[DynamicPiece lpiece]) :++ '[StaticPiece rpiece])
         , route ~ (FromPieces paths)
         , ApiHandler s m route
         , PathParam m route ~ HListToTuple (FilterDynP paths)
         , FromParam 'QueryParam (QueryParam m route)
         , FromParam 'FormParam (FormParam m route)
         , FromParam 'FileParam (FileParam m route)
         , FromParam 'Cookie (CookieIn m route)
         , FromHeader (HeaderIn m route)
         , Encodings (ContentTypes m route) (ApiErr m route)
         , Encodings (ContentTypes m route) (ApiOut m route)
         , ToHeader (HeaderOut m route)
         , ToParam 'Cookie (CookieOut m route)
         , DecodeParam lpiece
         , ParamErrToApiErr (ApiErr m route)
         , ToHListRecTuple (StripContents (RequestBody m route))
         , PartDecodings (RequestBody m route)
         , Typeable m
         , Typeable route
         , WebApiServer s
         ) => Router s ((lpiece :: *) :/ (rpiece :: Symbol)) '(m, pp) where
  route _ serv parsedRoute request respond =
    case pathInfo request of
      (lpth : rpth : []) | rpieceTxt == rpth -> case (decodeParam (encodeUtf8 lpth) :: Maybe lpiece) of
        Just dynVal -> respond . Matched =<< getResponse dynVal
        Nothing     -> respond NotMatched
      _ -> respond NotMatched
    where rpieceTxt = symTxt (Proxy :: Proxy rpiece)
          getResponse dynVal = do
            let pRoute :: ParsedRoute '(m, paths)
                pRoute = snocParsedRoute (snocParsedRoute parsedRoute $ DPiece dynVal) $ SPiece (Proxy :: Proxy rpiece)
            apiResp' <- fromWaiRequest request (fromParsedRoute pRoute) (\(req :: Request m route) -> toIO serv $ apiHandler (toTagged (Proxy :: Proxy '[]) serv) req)
            response <- case apiResp' of
              Validation (Right apiResp) -> return apiResp
              Validation (Left errs) -> return $ Failure $ Left $ ApiError badRequest400 (toApiErr errs) Nothing Nothing
            return $ toWaiResponse request response

instance ( route ~ (FromPieces (pp :++ '[DynamicPiece t]))
         , ApiHandler s m route
         , PathParam m route ~ HListToTuple (FilterDynP (pp :++ '[DynamicPiece t]))
         , FromParam 'QueryParam (QueryParam m route)
         , FromParam 'FormParam (FormParam m route)
         , FromParam 'FileParam (FileParam m route)
         , FromParam 'Cookie (CookieIn m route)
         , FromHeader (HeaderIn m route)
         , Encodings (ContentTypes m route) (ApiErr m route)
         , Encodings (ContentTypes m route) (ApiOut m route)
         , ToHeader (HeaderOut m route)
         , ToParam 'Cookie (CookieOut m route)
         , DecodeParam t
         , ParamErrToApiErr (ApiErr m route)
         , ToHListRecTuple (StripContents (RequestBody m route))
         , PartDecodings (RequestBody m route)
         , Typeable m
         , Typeable route
         , WebApiServer s
         ) => Router s (DynamicPiece t) '(m, pp) where
  route _ serv parsedRoute request respond =
    case pathInfo request of
      (lpth : []) -> case (decodeParam (encodeUtf8 lpth) :: Maybe t) of
        Just dynVal -> respond . Matched =<< getResponse dynVal
        Nothing     -> respond NotMatched
      _           -> respond NotMatched
    where getResponse dynVal = do
            let pRoute :: ParsedRoute '(m, (pp :++ '[DynamicPiece t]))
                pRoute = snocParsedRoute parsedRoute $ DPiece dynVal
            apiResp' <- fromWaiRequest request (fromParsedRoute pRoute) (\(req :: Request m route) -> toIO serv $ apiHandler (toTagged (Proxy :: Proxy '[]) serv) req)
            response <- case apiResp' of
              Validation (Right apiResp) -> return apiResp
              Validation (Left errs) -> return $ Failure $ Left $ ApiError badRequest400 (toApiErr errs) Nothing Nothing
            return $ toWaiResponse request response

instance ( PathParam m (ns :// piece) ~ ()
         , ParamErrToApiErr (ApiErr m (ns :// piece))
         , KnownSymbol piece
         , FromHeader (HeaderIn m (ns :// piece))
         , FromParam 'QueryParam (QueryParam m (ns :// piece))
         , FromParam 'FormParam (FormParam m (ns :// piece))
         , FromParam 'FileParam (FileParam m (ns :// piece))
         , FromParam 'Cookie (CookieIn m (ns :// piece))
         , ToHListRecTuple (StripContents (RequestBody m (ns :// piece)))
         , SingMethod m
         , PartDecodings (RequestBody m (ns :// piece))
         , WebApiServer s
         , Typeable ns
         , Typeable m
         , Typeable piece
         , ApiHandler s m (ns :// piece)
         , ToHeader (HeaderOut m (ns :// piece))
         , ToParam 'Cookie (CookieOut m (ns :// piece))
         , Encodings (ContentTypes m (ns :// piece)) (ApiErr m (ns :// piece))
         , Encodings (ContentTypes m (ns :// piece)) (ApiOut m (ns :// piece))
         ) => Router s ((ns :: *) :// (piece :: Symbol)) '(m, pp) where
  route _ serv _ request respond =
    case pathInfo request of
      (pth : []) | symTxt (Proxy :: Proxy piece) == pth -> respond . Matched =<< getResponse
      [] | T.null $ symTxt (Proxy :: Proxy piece) -> respond . Matched =<< getResponse
      _ -> respond $ NotMatched
    where getResponse = do
            apiResp' <- fromWaiRequest request () (\(req :: Request m (ns :// piece)) -> toIO serv $ apiHandler (toTagged (Proxy :: Proxy '[]) serv) req)
            response <- case apiResp' of
              Validation (Right apiResp) -> return apiResp 
              Validation (Left errs) -> return $ Failure $ Left $ ApiError badRequest400 (toApiErr errs) Nothing Nothing
            return $ toWaiResponse request response

router :: ( Router server apis '(CUSTOM "", '[])
          ) => Proxy apis -> server -> RoutingApplication
router apis s = route apis s emptyParsedRoutes

-- Helpers

symTxt :: KnownSymbol sym => proxy sym -> Text
symTxt sym = pack (symbolVal sym)

emptyParsedRoutes :: ParsedRoute '(CUSTOM "", '[])
emptyParsedRoutes = Nil Proxy

snocParsedRoute :: ParsedRoute '(method, ps) -> PieceType pt -> ParsedRoute '(method, ps :++ '[pt])
snocParsedRoute nil@Nil{} (SPiece sym)   = sym `ConsStaticPiece` nil
snocParsedRoute nil@Nil{} (NSPiece prov) = prov `ConsNSPiece` nil
snocParsedRoute nil@Nil{} (DPiece val)   = val `ConsDynamicPiece` nil
snocParsedRoute (ConsStaticPiece sym routes) pt  = (ConsStaticPiece sym $ snocParsedRoute routes pt)
snocParsedRoute (ConsNSPiece prov routes) pt     = (ConsNSPiece prov $ snocParsedRoute routes pt)
snocParsedRoute (ConsDynamicPiece sym routes) pt = (ConsDynamicPiece sym $ snocParsedRoute routes pt)

instance (MkFormatStr (ToPieces (a :/ b))) => MkPathFormatString (a :/ b) where
  mkPathFormatString _ = mkFormatStr (Proxy :: Proxy (ToPieces (a :/ b)))

instance (MkPathFormatString b) => MkPathFormatString (a :// b) where
  mkPathFormatString _ = mkPathFormatString (Proxy :: Proxy b)

instance (KnownSymbol s) => MkPathFormatString (Static s) where
  mkPathFormatString _ = mkFormatStr (Proxy :: Proxy (ToPieces (Static s)))

class MkFormatStr (xs :: [*]) where
  mkFormatStr :: Proxy xs -> [PathSegment]

instance (KnownSymbol s, MkFormatStr xs) => MkFormatStr (StaticPiece s ': xs) where
  mkFormatStr _ = StaticSegment (T.pack (symbolVal (Proxy :: Proxy s))) : mkFormatStr (Proxy :: Proxy xs)

instance (MkFormatStr xs) => MkFormatStr (DynamicPiece s ': xs) where
  mkFormatStr _ = Hole : mkFormatStr (Proxy :: Proxy xs)

instance MkFormatStr '[] where
  mkFormatStr _ = []

-- | This function is used to call local handler without incurring the cost of network round trip and se/deserialisation of Request and Response.
apiHandler :: forall query p m r.
             ( query ~ '[]
             , MonadCatch (HandlerM p)
             , ApiHandler p m r
             , Typeable m
             , Typeable r) => Tagged query p -> Request m r -> HandlerM p (Query (Response m r) query)
apiHandler serv req =  (handler serv req) `catches` excepHandlers
  where excepHandlers :: [Handler (HandlerM p) (Query (Response m r) query)]
        excepHandlers = [ Handler (\ (ex :: ApiException m r) -> handleApiException (unTagged serv) ex)
                        , Handler (\ (ex :: SomeException) -> handleSomeException (unTagged serv) ex) ]
