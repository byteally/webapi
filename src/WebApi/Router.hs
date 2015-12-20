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
       ( Static
       , Root
       , (:/)
       , Route
       , Router (..)
       , router

       , MkFormatStr (..)
       , ToPieces
       , PathSegment (..)
       ) where

import Control.Exception (catches, Handler (..), SomeException (..))
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

data Route (m :: *) (r :: *)

data StaticPiece (s :: Symbol)

data DynamicPiece (t :: *)

data (:/) (p1 :: k) (p2 :: k1)
infixr 5 :/

type instance PathParam' m (Static s) = ()
type instance PathParam' m (p1 :/ p2) = HListToTuple (FilterDynP (ToPieces (p1 :/ p2)))

data Static (s :: Symbol)

type Root = Static ""


data PieceType :: * -> * where
  SPiece :: Proxy (p :: Symbol) -> PieceType (StaticPiece p)
  DPiece :: !val -> PieceType (DynamicPiece val)

data ParsedRoute :: (*, [*]) -> * where
  Nil              :: Proxy method -> ParsedRoute '(method, '[])
  ConsStaticPiece  :: Proxy (p :: Symbol) -> ParsedRoute '(method, ps) -> ParsedRoute '(method, ((StaticPiece p) ': ps))
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
dropStaticPiece (ConsDynamicPiece p ps) = p :* dropStaticPiece ps

type family ToPieces (r :: k) :: [*] where
  ToPieces (Static s)                       = '[StaticPiece s]
  ToPieces ((p1 :: Symbol) :/ (p2 :: Symbol)) = '[StaticPiece p1, StaticPiece p2]
  ToPieces ((p1 :: *) :/ (p2 :: Symbol))      = '[DynamicPiece p1, StaticPiece p2]
  ToPieces ((p1 :: Symbol) :/ (p2 :/ p3))    = StaticPiece p1 ': ToPieces (p2 :/ p3)
  ToPieces ((p1 :: *) :/ (p2 :/ p3))         = DynamicPiece p1 ': ToPieces (p2 :/ p3)
  ToPieces ((p1 :: *) :/ (p2 :: *))           = '[DynamicPiece p1, DynamicPiece p2]
  ToPieces ((p1 :: Symbol) :/ (p2 :: *))      = '[StaticPiece p1, DynamicPiece p2]

type family FromPieces (pps :: [*]) :: * where
  FromPieces '[StaticPiece s]                    = Static s
  FromPieces '[StaticPiece p1, StaticPiece p2]   = p1 :/ p2
  FromPieces '[DynamicPiece p1, DynamicPiece p2] = p1 :/ p2
  FromPieces '[StaticPiece p1, DynamicPiece p2]  = p1 :/ p2
  FromPieces '[DynamicPiece p1, StaticPiece p2]  = p1 :/ p2
  FromPieces ((StaticPiece p1) ': ((StaticPiece p2) ': pps)) = p1 :/ (FromPieces ((StaticPiece p2) ': pps))
  FromPieces ((DynamicPiece p1) ': ((DynamicPiece p2) ': pps)) = p1 :/ (FromPieces ((DynamicPiece p2) ': pps))
  FromPieces ((StaticPiece p1) ': ((DynamicPiece p2) ': pps)) = p1 :/ (FromPieces ((DynamicPiece p2) ': pps))
  FromPieces ((DynamicPiece p1) ': ((StaticPiece p2) ': pps)) = p1 :/ (FromPieces ((StaticPiece p2) ': pps))

type family FilterDynP (ps :: [*]) :: [*] where
  FilterDynP (DynamicPiece p1 ': p2) = p1 ': FilterDynP p2
  FilterDynP (p1 ': p2)              = FilterDynP p2
  FilterDynP '[]                     = '[]

type family HListToTuple (xs :: [*]) :: * where
  HListToTuple '[]   = ()
  HListToTuple '[p1] = p1
  HListToTuple '[p1, p2] = (p1, p2)
  HListToTuple '[p1, p2, p3] = (p1, p2, p3)
  HListToTuple '[p1, p2, p3, p4] = (p1, p2, p3, p4)
  HListToTuple '[p1, p2, p3, p4, p5] = (p1, p2, p3, p4, p5)
  HListToTuple '[p1, p2, p3, p4, p5, p6] = (p1, p2, p3, p4, p5, p6)
  HListToTuple '[p1, p2, p3, p4, p5, p6, p7] = (p1, p2, p3, p4, p5, p6, p7)
  HListToTuple '[p1, p2, p3, p4, p5, p6, p7, p8] = (p1, p2, p3, p4, p5, p6, p7, p8)
  HListToTuple '[p1, p2, p3, p4, p5, p6, p7, p8, p9] = (p1, p2, p3, p4, p5, p6, p7, p8, p9)

infixr 5 :++
type family (:++) (as :: [k]) (bs :: [k]) :: [k] where
  '[] :++ bs       = bs
  (a ': as) :++ bs = a ': (as :++ bs)

class Router (server :: *) (r :: k) (pr :: (*, [*])) where
  route :: ( iface ~ (ApiInterface server)
            , HandlerM iface ~ IO
          ) => Proxy r -> server -> ParsedRoute pr -> RoutingApplication

type family MarkDyn (pp :: *) :: * where
  MarkDyn (p1 :/ t) = (p1 :/ t)
  MarkDyn (t :: *)   = DynamicPiece t

instance (Router s r '(m, '[]), SingMethod m) => Router s (Route m r) pr where
  route _ _s _ request respond =
    case requestMethod request == meth of
      True  -> route (Proxy :: Proxy r) _s (Nil (Proxy :: Proxy m)) request respond
      False -> respond NotMatched
    where meth = singMethod (Proxy :: Proxy m)

instance (Router s route pr, Router s routes pr) => Router s ((route :: *) ': routes) pr where
  route _ _s parsedRoute request respond =
    route (Proxy :: Proxy route) _s parsedRoute request $ \case
      Matched a -> respond $ Matched a
      NotMatched -> route (Proxy :: Proxy routes) _s parsedRoute request respond

instance Router s '[] pr where
  route _ _s _ _ respond = respond NotMatched

instance (Router s (MarkDyn rest) '(m, (pp :++ '[DynamicPiece piece])), HttpParam piece)
                      => Router s ((piece :: *) :/ (rest :: *)) '(m, pp) where
  route _ _s parsedRoute request respond =
    case pathInfo request of
      (lpth : rpths)  -> case (fromHttpParam (encodeUtf8 lpth) :: Maybe piece) of
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
instance ( KnownSymbol piece, Server s m (Static piece)
         , ToHeader (HeaderOut m (Static piece))
         , ToParam (CookieOut m (Static piece)) 'Cookie
         , FromParam (QueryParam m (Static piece)) 'QueryParam
         , FromParam (FormParam m (Static piece)) 'FormParam
         , FromParam (FileParam m (Static piece)) 'FileParam
         , FromHeader (HeaderIn m (Static piece))
         , FromParam (CookieIn m (Static piece)) 'Cookie
         , Encodings (ContentTypes m (Static piece)) (ApiOut m (Static piece))
         , Encodings (ContentTypes m (Static piece)) (ApiErr m (Static piece))
         , PathParam m (Static piece) ~ ()
         , ParamErrToApiErr (ApiErr m (Static piece))
         , Typeable m
         , Typeable (Static piece)  
         ) => Router s (Static piece) '(m, pp) where
  route _ serv _ request respond =
    case pathInfo request of
      (pth : []) | symTxt (Proxy :: Proxy piece) == pth -> respond . Matched =<< getResponse
      [] | T.null $ symTxt (Proxy :: Proxy piece) -> respond . Matched =<< getResponse
      _ -> respond $ NotMatched
    where getResponse = do
            apiReq' <- fromWaiRequest request ()
            response <- case apiReq' of
              Validation (Right apiReq) -> serve serv (Proxy :: Proxy '[]) (apiReq :: Request m (Static piece))
              Validation (Left errs) -> return $ Failure $ Left $ ApiError badRequest400 (toApiErr errs) Nothing Nothing
            return $ toWaiResponse request response

instance ( KnownSymbol lpiece
         , KnownSymbol rpiece
         , paths ~ (pp :++ '[StaticPiece lpiece, StaticPiece rpiece])
         , paths ~ ((pp :++ '[StaticPiece lpiece]) :++ '[StaticPiece rpiece])
         , route ~ (FromPieces paths)
         , Server s m route
         , PathParam m route ~ HListToTuple (FilterDynP paths)
         , FromParam (QueryParam m route) 'QueryParam
         , FromParam (FormParam m route) 'FormParam
         , FromParam (FileParam m route) 'FileParam
         , FromParam (CookieIn m route) 'Cookie
         , FromHeader (HeaderIn m route)
         , Encodings (ContentTypes m route) (ApiErr m route)
         , Encodings (ContentTypes m route) (ApiOut m route)
         , ToHeader (HeaderOut m route)
         , ToParam (CookieOut m route) 'Cookie
         , ParamErrToApiErr (ApiErr m route)
         , Typeable m
         , Typeable route  
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
            apiReq' <- fromWaiRequest request pathPar
            response <- case apiReq' of
              Validation (Right apiReq) -> serve serv (Proxy :: Proxy '[]) (apiReq :: Request m route)
              Validation (Left errs) -> return $ Failure $ Left $ ApiError badRequest400 (toApiErr errs) Nothing Nothing
            return $ toWaiResponse request response

instance ( KnownSymbol rpiece
         , paths ~ (pp :++ '[DynamicPiece lpiece, StaticPiece rpiece])
         , paths ~ ((pp :++ '[DynamicPiece lpiece]) :++ '[StaticPiece rpiece])
         , route ~ (FromPieces paths)
         , Server s m route
         , PathParam m route ~ HListToTuple (FilterDynP paths)
         , FromParam (QueryParam m route) 'QueryParam
         , FromParam (FormParam m route) 'FormParam
         , FromParam (FileParam m route) 'FileParam
         , FromParam (CookieIn m route) 'Cookie
         , FromHeader (HeaderIn m route)
         , Encodings (ContentTypes m route) (ApiErr m route)
         , Encodings (ContentTypes m route) (ApiOut m route)
         , ToHeader (HeaderOut m route)
         , ToParam (CookieOut m route) 'Cookie
         , HttpParam lpiece
         , ParamErrToApiErr (ApiErr m route)
         , Typeable m
         , Typeable route  
         ) => Router s ((lpiece :: *) :/ (rpiece :: Symbol)) '(m, pp) where
  route _ serv parsedRoute request respond =
    case pathInfo request of
      (lpth : rpth : []) | rpieceTxt == rpth -> case (fromHttpParam (encodeUtf8 lpth) :: Maybe lpiece) of
        Just dynVal -> respond . Matched =<< getResponse dynVal
        Nothing     -> respond NotMatched
      _ -> respond NotMatched
    where rpieceTxt = symTxt (Proxy :: Proxy rpiece)
          getResponse dynVal = do
            let pRoute :: ParsedRoute '(m, paths)
                pRoute = snocParsedRoute (snocParsedRoute parsedRoute $ DPiece dynVal) $ SPiece (Proxy :: Proxy rpiece)
            apiReq' <- fromWaiRequest request (fromParsedRoute pRoute)
            response <- case apiReq' of
              Validation (Right apiReq) -> serve serv (Proxy :: Proxy '[]) (apiReq :: Request m route)
              Validation (Left errs) -> return $ Failure $ Left $ ApiError badRequest400 (toApiErr errs) Nothing Nothing
            return $ toWaiResponse request response


instance ( route ~ (FromPieces (pp :++ '[DynamicPiece t]))
         , Server s m route
         , PathParam m route ~ HListToTuple (FilterDynP (pp :++ '[DynamicPiece t]))
         , FromParam (QueryParam m route) 'QueryParam
         , FromParam (FormParam m route) 'FormParam
         , FromParam (FileParam m route) 'FileParam
         , FromParam (CookieIn m route) 'Cookie
         , FromHeader (HeaderIn m route)
         , Encodings (ContentTypes m route) (ApiErr m route)
         , Encodings (ContentTypes m route) (ApiOut m route)
         , ToHeader (HeaderOut m route)
         , ToParam (CookieOut m route) 'Cookie
         , HttpParam t
         , ParamErrToApiErr (ApiErr m route)
         , Typeable m
         , Typeable route
         ) => Router s (DynamicPiece t) '(m, pp) where
  route _ serv parsedRoute request respond =
    case pathInfo request of
      (lpth : []) -> case (fromHttpParam (encodeUtf8 lpth) :: Maybe t) of
        Just dynVal -> respond . Matched =<< getResponse dynVal
        Nothing     -> respond NotMatched
      _           -> respond NotMatched
    where getResponse dynVal = do
            let pRoute :: ParsedRoute '(m, (pp :++ '[DynamicPiece t]))
                pRoute = snocParsedRoute parsedRoute $ DPiece dynVal
            apiReq' <- fromWaiRequest request (fromParsedRoute pRoute)
            response <- case apiReq' of
              Validation (Right apiReq) -> serve serv (Proxy :: Proxy '[]) (apiReq :: Request m route)
              Validation (Left errs) -> return $ Failure $ Left $ ApiError badRequest400 (toApiErr errs) Nothing Nothing
            return $ toWaiResponse request response


router :: ( iface ~ (ApiInterface server)
          , HandlerM iface ~ IO
          , Router server apis '(CUSTOM "", '[])
          ) => Proxy apis -> server -> RoutingApplication
router apis s = route apis s emptyParsedRoutes

-- Helpers

symTxt :: KnownSymbol sym => proxy sym -> Text
symTxt sym = pack (symbolVal sym)

emptyParsedRoutes :: ParsedRoute '(CUSTOM "", '[])
emptyParsedRoutes = Nil Proxy

snocParsedRoute :: ParsedRoute '(method, ps) -> PieceType pt -> ParsedRoute '(method, ps :++ '[pt])
snocParsedRoute nil@Nil{} (SPiece sym) = sym `ConsStaticPiece` nil
snocParsedRoute nil@Nil{} (DPiece val) = val `ConsDynamicPiece` nil
snocParsedRoute (ConsStaticPiece sym routes) symOrVal = (ConsStaticPiece sym $ snocParsedRoute routes symOrVal)
snocParsedRoute (ConsDynamicPiece sym routes) symOrVal = (ConsDynamicPiece sym $ snocParsedRoute routes symOrVal)

instance (MkFormatStr (ToPieces (a :/ b))) => MkPathFormatString (a :/ b) where
  mkPathFormatString _ = mkFormatStr (Proxy :: Proxy (ToPieces (a :/ b)))

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

serve :: forall query p m r.
       ( query ~ '[]
       , HandlerM (ApiInterface p) ~ IO
       , Server p m r
       , Typeable m
       , Typeable r) => p -> Proxy query -> Request m r -> (HandlerM (ApiInterface p)) (Query (Response m r) query)
serve serv q req = catches (handler serv q req) dub
  where dub :: [Handler (Query (Response m r) query)]
        dub = [ Handler (\ (ex :: ApiException m r) -> handleApiException serv ex)
              , Handler (\ (ex :: SomeException) -> handleSomeException serv ex) ]

