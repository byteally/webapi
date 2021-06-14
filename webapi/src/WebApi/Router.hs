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
       , NamespaceOf

       -- * Custom routing
       , PathSegment (..)
       , MkPathFormatString (..)
       , apiHandler

       -- * Internal
       , FromPieces'
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
--import qualified Network.Wai as Wai
import WebApi.ContentTypes
import WebApi.Contract
import WebApi.Internal
import WebApi.Param
import WebApi.Util


-- | Get the namespace of a route
type family NamespaceOf (r :: *) where
  NamespaceOf (ns :// r) = ns

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
instance ( KnownSymbol piece, ApiHandler s m route
         , ToHeader (HeaderOut m route)
         , ToParam 'Cookie (CookieOut m route)
         , FromWaiRequestCtx m route
         , Encodings (ContentTypes m route) (ApiOut m route)
         , Encodings (ContentTypes m route) (ApiErr m route)
         , PathParam m route ~ ()
         , ParamErrToApiErr (ApiErr m route)
         , Typeable m
         , Typeable route
         , WebApiServer s
         , paths ~ (pp :++ '[StaticPiece piece])
         , route ~ (FromPieces paths)
         ) => Router s (Static piece) '(m, pp) where
  route _ serv _ request respond =
    case pathInfo request of
      (pth : []) | symTxt (Proxy :: Proxy piece) == pth -> respond . Matched =<< getResponse
      [] | T.null $ symTxt (Proxy :: Proxy piece) -> respond . Matched =<< getResponse
      _ -> respond $ NotMatched
    where getResponse = do
            let wbReq = WebApiRequest { rawRequest = request }
            apiResp' <- fromWaiRequest request () (\(req :: Request m route) -> toIO serv wbReq $ apiHandler (toTagged (Proxy :: Proxy '[]) serv) req)
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
         , Encodings (ContentTypes m route) (ApiErr m route)
         , Encodings (ContentTypes m route) (ApiOut m route)
         , ToHeader (HeaderOut m route)
         , ToParam 'Cookie (CookieOut m route)
         , ParamErrToApiErr (ApiErr m route)
         , FromWaiRequestCtx m route
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
          wbReq = WebApiRequest { rawRequest = request }
          getResponse = do
            apiResp' <- fromWaiRequest request pathPar (\(req :: Request m route) -> toIO serv wbReq $ apiHandler (toTagged (Proxy :: Proxy '[]) serv) req)
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
         , Encodings (ContentTypes m route) (ApiErr m route)
         , Encodings (ContentTypes m route) (ApiOut m route)
         , ToHeader (HeaderOut m route)
         , ToParam 'Cookie (CookieOut m route)
         , DecodeParam lpiece
         , ParamErrToApiErr (ApiErr m route)
         , FromWaiRequestCtx m route
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
                wbReq = WebApiRequest { rawRequest = request }
            apiResp' <- fromWaiRequest request (fromParsedRoute pRoute) (\(req :: Request m route) -> toIO serv wbReq $ apiHandler (toTagged (Proxy :: Proxy '[]) serv) req)
            response <- case apiResp' of
              Validation (Right apiResp) -> return apiResp
              Validation (Left errs) -> return $ Failure $ Left $ ApiError badRequest400 (toApiErr errs) Nothing Nothing
            return $ toWaiResponse request response

instance ( route ~ (FromPieces (pp :++ '[DynamicPiece t]))
         , ApiHandler s m route
         , PathParam m route ~ HListToTuple (FilterDynP (pp :++ '[DynamicPiece t]))
         , Encodings (ContentTypes m route) (ApiErr m route)
         , Encodings (ContentTypes m route) (ApiOut m route)
         , ToHeader (HeaderOut m route)
         , ToParam 'Cookie (CookieOut m route)
         , DecodeParam t
         , ParamErrToApiErr (ApiErr m route)
         , FromWaiRequestCtx m route
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
                wbReq = WebApiRequest { rawRequest = request }
            apiResp' <- fromWaiRequest request (fromParsedRoute pRoute) (\(req :: Request m route) -> toIO serv wbReq $ apiHandler (toTagged (Proxy :: Proxy '[]) serv) req)
            response <- case apiResp' of
              Validation (Right apiResp) -> return apiResp
              Validation (Left errs) -> return $ Failure $ Left $ ApiError badRequest400 (toApiErr errs) Nothing Nothing
            return $ toWaiResponse request response

instance ( PathParam m (ns :// piece) ~ ()
         , ParamErrToApiErr (ApiErr m (ns :// piece))
         , KnownSymbol piece 
         , SingMethod m
         , WebApiServer s
         , Typeable ns
         , Typeable m
         , Typeable piece
         , ApiHandler s m (ns :// piece)
         , ApiContract (ApiInterface s) m (ns :// piece)
         , ToHeader (HeaderOut m (ns :// piece))
         , ToParam 'Cookie (CookieOut m (ns :// piece))
         , FromWaiRequestCtx m (ns :// piece)
         , Encodings (ContentTypes m (ns :// piece)) (ApiErr m (ns :// piece))
         , Encodings (ContentTypes m (ns :// piece)) (ApiOut m (ns :// piece))
         ) => Router s ((ns :: *) :// (piece :: Symbol)) '(m, pp) where
  route _ serv _ request respond =
    case pathInfo request of
      (pth : []) | symTxt (Proxy :: Proxy piece) == pth -> respond . Matched =<< getResponse
      [] | T.null $ symTxt (Proxy :: Proxy piece) -> respond . Matched =<< getResponse
      _ -> respond $ NotMatched
    where getResponse = do
            let wbReq = WebApiRequest { rawRequest = request }
            apiResp' <- fromWaiRequest request () (\(req :: Request m (ns :// piece)) -> toIO serv wbReq $ apiHandler (toTagged (Proxy :: Proxy '[]) serv) req)
            response <- case apiResp' of
              Validation (Right apiResp) -> return apiResp 
              Validation (Left errs) -> return $ Failure $ Left $ ApiError badRequest400 (toApiErr errs) Nothing Nothing
            return $ toWaiResponse request response

instance ( KnownSymbol pth
         , Router (NestedApplication apps) nest acc
         ) => Router (NestedApplication ( '(pth, c) ': apps)) nest acc where
  route pNest nesApp pr request respond =
    let (app, apps) = unconsNesApp nesApp
    in case pathInfo request of
         (pth : pths) | symTxt (Proxy :: Proxy pth) == pth
                     -> app (request {pathInfo = pths}) (respond . Matched)
         _ -> route pNest apps pr request respond

instance Router (NestedApplication '[]) nest acc where
  route _ _ _ _ respond = respond $ NotMatched
                   

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

