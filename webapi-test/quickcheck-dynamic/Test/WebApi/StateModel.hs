{-# LANGUAGE UndecidableInstances #-}
module Test.WebApi.StateModel
  ( WebApiAction (..)
  , ApiState (..)
  , WebApiActionCxt
  , ApiSuccess (..)
  , ErrorState (..)
  , ResultError (..)
  , ModifyClientCookies (..)
  , SuccessApiModel
  , FailureApiModel
  , HasApiState (..)
  , DSum (..)
  , Val (..)
  , ClientRequestVal (..)
  , WebApiGlobalStateModel (..)
  , successCall
  , successCallWith
  , mkWebApiAction
  , getOpIdFromRequest
  , getSuccessOut
  , getSuccessCode
  , getSuccessHeaders
  , getSuccessCookies
  , defSuccessApiModel
  , defFailureApiModel
  , initApiState
  , modifyApiState
  ) where

import Test.WebApi
import Test.QuickCheck.StateModel
import Test.QuickCheck.DynamicLogic (DynLogicModel (..))
-- import Test.QuickCheck.StateModel.Variables (Any (..))
import qualified Test.QuickCheck as QC
import WebApi.Contract
import WebApi.Param
import WebApi.ContentTypes
import WebApi.Util
import Web.Cookie
import Control.Exception (SomeException)
import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Kind
import Data.Typeable
import Data.Coerce
import GHC.TypeLits
import qualified Network.HTTP.Types as H
import Data.Functor.Identity
import qualified Data.Text as T
import qualified Data.Map.Strict as M
import qualified Data.Set as Set
import Data.Dependent.Sum (DSum (..))
import System.IO.Unsafe
import qualified Unsafe.Coerce as Unsafe
import qualified GHC.Base as Unsafe (Any)
import qualified Record
import Data.Reflection

type WebApiActionCxt (apps :: [Type]) (meth :: Type) (app :: Type) (r :: k) =
  ( ToParam 'PathParam (PathParam meth (app://r))
  , ToParam 'QueryParam (QueryParam meth (app://r))
  , ToParam 'FormParam (FormParam meth (app :// r))
  , ToParam 'FileParam (FileParam meth (app :// r))
  , ToHeader (HeaderIn meth (app://r))
  , ToHeader (HeaderOut meth (app://r))
  , FromHeader (HeaderOut meth (app :// r))
  , FromParam Cookie (CookieOut meth (app :// r))
  , Decodings (ContentTypes meth (app :// r)) (ApiOut meth (app :// r))
  , Decodings (ContentTypes meth (app :// r)) (ApiErr meth (app :// r))
  , PartEncodings (RequestBody meth (app :// r))
  , ToHListRecTuple (StripContents (RequestBody meth (app :// r)))
  , MkPathFormatString (app://r)
  , SingMethod meth
  , WebApi app
  , Typeable app
  , Typeable (ApiOut meth (app :// r))
  , Typeable (ApiErr meth (app :// r))
  , Typeable r
  , Typeable meth
  , Typeable k
  , AppIsElem app apps
  , KnownSymbol (GetOpIdName (OperationId meth (app :// r)))
  , Record.FromHK (PathParam meth (app://r))
  , Record.FromHK (QueryParam meth (app://r))
  , Record.FromHK (FormParam meth (app://r))
  , Record.FromHK (HeaderIn meth (app://r))
  )

data ApiSuccess (m :: Type) (r :: Type) = ApiSuccess
  { code      :: H.Status
  , out       :: ApiOut m r
  , headerOut :: HeaderOut m r
  , cookieOut :: CookieOut m r
  }

getSuccessOut :: ApiSuccess m r -> ApiOut m r
getSuccessOut (ApiSuccess {out}) = out

getSuccessCode :: ApiSuccess m r -> H.Status
getSuccessCode (ApiSuccess {code}) = code

getSuccessHeaders :: ApiSuccess m r -> HeaderOut m r
getSuccessHeaders (ApiSuccess {headerOut}) = headerOut

getSuccessCookies :: ApiSuccess m r -> CookieOut m r
getSuccessCookies (ApiSuccess {cookieOut}) = cookieOut

data Val a where
  Const :: a -> Val a
  Var :: Typeable x => (x -> a) -> Var x -> Val a
  HKVal :: (Typeable x, Record.FromHK x) => (x -> a) -> Record.HK Val x -> Val a -- TODO: Avoid FromHK dep
  Pair :: (Typeable x1, Typeable x2) => ((x1, x2) -> a) -> (Val x1, Val x2) -> Val a
  Opt :: Val a

deriving instance Functor Val

instance Applicative Val where
  pure = Const
  f' <*> a' = case f' of
    Const f -> case a' of
      Const a -> Const (f a)
      Var fn v -> Var (f . fn) v
      HKVal fn hkv -> HKVal (f . fn) hkv
      Pair fn vs -> Pair (f . fn) vs
      Opt -> error "TODO"
    Var fn v -> case a' of
      Const a -> Var (flip fn a) v
      Var fn1 v1 -> Pair (\(x, x1) -> fn x (fn1 x1)) (Var id v, Var id v1)
      HKVal fn1 hkv1 -> Pair (\(x, x1) -> fn x (fn1 x1)) (Var id v, HKVal id hkv1)
      Pair fn1 vs -> Pair (\(x, x1) -> fn x (fn1 x1)) (Var id v, Pair id vs)
      Opt -> error "TODO"
    HKVal fn v -> case a' of
      Const a -> HKVal (flip fn a) v
      Var fn1 v1 -> Pair (\(x, x1) -> fn x (fn1 x1)) (HKVal id v, Var id v1)
      HKVal fn1 hkv1 -> Pair (\(x, x1) -> fn x (fn1 x1)) (HKVal id v, HKVal id hkv1)
      Pair fn1 vs -> Pair (\(x, x1) -> fn x (fn1 x1)) (HKVal id v, Pair id vs)
      Opt -> error "TODO"
    Pair fn vs -> case a' of
      Const a -> Pair ((\f -> f a) . fn) vs
      Var fn1 v -> Pair (\(x1x2, x) -> fn x1x2 (fn1 x)) (Pair id vs, Var id v)
      HKVal fn1 hkv1 -> Pair (\(x1x2, x) -> fn x1x2 (fn1 x)) (Pair id vs, HKVal id hkv1)
      Pair fn1 vs1 -> Pair (\(x1x2, x4x5) -> fn x1x2 (fn1 x4x5)) (Pair id vs, Pair id vs1)
      Opt -> error "TODO"
    Opt -> Opt

resolveVal :: LookUp -> Val a -> a
resolveVal lkp = \case
  Const v -> v
  Var f var -> f (lkp var)
  HKVal f hk -> f $ runIdentity $ Record.fromHK $ Record.hoistHK (Identity . resolveVal lkp) hk
  Pair f (v1, v2) -> f (resolveVal lkp v1, resolveVal lkp v2)
  Opt -> error "TODO"

instance HasVariables (Val a) where
  getAllVariables = \case
    Const {} -> Set.empty
    Var _ var -> getAllVariables var
    HKVal _ hk -> Set.unions $ Record.hkToListWith getAllVariables hk
    Pair _ (v1, v2) -> getAllVariables v1 <> getAllVariables v2
    Opt -> error "TODO"

data ClientRequestVal meth r = ClientRequestVal
  { query :: Val (QueryParam meth r)
  , form :: Val (FormParam meth r)
  , header :: Val (HeaderIn meth r)
  , path :: Val (PathParam meth r)
  , file :: Val (FileParam meth r)
  , body :: Val (HListToTuple (StripContents (RequestBody meth r)))
  }

instance HasVariables (ClientRequestVal meth r) where
  getAllVariables ClientRequestVal {query, form, header, path, file, body} =
    getAllVariables query
    <> getAllVariables form
    <> getAllVariables header
    <> getAllVariables path
    <> getAllVariables file
    <> getAllVariables body

resolveRequest ::
  ( Record.FromHK (QueryParam meth r)
  , Record.FromHK (FormParam meth r)
  , Record.FromHK (HeaderIn meth r)
  , Record.FromHK (PathParam meth r)
  , SingMethod meth
  ) => LookUp
  -> ClientRequestVal meth r
  -> Maybe (ClientRequest meth r)
resolveRequest lkp ClientRequestVal {query, form, header, path, file, body} =
  do
    query' <- Just $ resolveVal lkp query
    form' <- Just $ resolveVal lkp form
    header' <- Just $ resolveVal lkp header
    path' <- Just $ resolveVal lkp path
    file' <- Just $ resolveVal lkp file
    body' <- Just $ resolveVal lkp body
    Just $ ClientRequest {query = query', form = form', header = header', path = path', file = file', body = body'}

data WebApiAction s (apps :: [Type]) (a :: Type) where
  SuccessCall :: WebApiActionCxt apps meth app r
    => ClientRequestVal meth (app :// r)
    -> SuccessApiModel s apps meth (app :// r) res
    -> Maybe (ApiSuccess meth (app :// r) -> ModifyClientCookies app)
    -> (ApiSuccess meth (app :// r) -> Either ResultError res)
    -> WebApiAction s apps res
  ErrorCall :: WebApiActionCxt apps meth app r
    => ClientRequest meth (app :// r)
    -> WebApiAction s apps (ApiErr meth (app :// r))
  SomeExceptionCall :: WebApiActionCxt apps meth app r
    => ClientRequest meth (app :// r)
    -> WebApiAction s apps (SomeException)

instance Show (WebApiAction s apps a) where
  show = \case
    SuccessCall creq _ _ _ -> show . getOpIdFromRequest $ creq
    ErrorCall creq -> show . unsafePerformIO . toWaiRequest . fromClientRequest $ creq
    SomeExceptionCall creq -> show . unsafePerformIO . toWaiRequest . fromClientRequest $ creq

-- TODO: Revisit
instance Eq (WebApiAction s apps a) where
  (==) (SuccessCall creq1 _ _ _) = \case
    SuccessCall creq2 _ _ _ -> (getOpIdFromRequest $ creq1) == (getOpIdFromRequest $ creq2)
    _ -> False
  (==) (ErrorCall creq1) = \case
    ErrorCall creq2 -> (show . unsafePerformIO . toWaiRequest . fromClientRequest $ creq1) == (show . unsafePerformIO . toWaiRequest . fromClientRequest $ creq2)
    _ -> False
  (==) (SomeExceptionCall creq1) = \case
    SomeExceptionCall creq2 -> (show . unsafePerformIO . toWaiRequest . fromClientRequest $ creq1) == (show . unsafePerformIO . toWaiRequest . fromClientRequest $ creq2)
    _ -> False

instance HasVariables (WebApiAction s apps a) where
  getAllVariables = \case
    SuccessCall creq _ _ _ -> getAllVariables creq
    ErrorCall _creq -> error "TODO:"
    SomeExceptionCall _creq -> error "TODO:"

newtype RefinementId = RefinementId Text
  deriving newtype (Show, Eq, Ord, Read)

data NamedEntity k = NamedEntity
  { namedEntity :: M.Map k [Any Val]
  , entityRefinement :: M.Map RefinementId (Any Val -> Bool)
  , entityToRefinements :: M.Map k (Set.Set RefinementId)
  }

instance Show k => Show (NamedEntity k) where
  show NamedEntity {namedEntity = ne, entityRefinement = er, entityToRefinements = e2r} =
    "Entities: " ++ show (M.keys ne) ++ ", Refinements: " ++ (show $ M.keys er) ++ ", EntityRefinements: " ++ show e2r

instance Eq k => Eq (NamedEntity k) where
  NamedEntity {namedEntity = ne1, entityRefinement = er1, entityToRefinements = e2r1} ==
    NamedEntity {namedEntity = ne2, entityRefinement = er2, entityToRefinements = e2r2} =
    ne1 == ne2 && (M.keys er1) == (M.keys er2) && e2r1 == e2r2
    
instance Ord k => Semigroup (NamedEntity k) where
  NamedEntity {namedEntity = ne1, entityRefinement = er1, entityToRefinements = e2r1} <>
    NamedEntity {namedEntity = ne2, entityRefinement = er2, entityToRefinements = e2r2} =
    NamedEntity { namedEntity = M.unionWith (<>) ne1 ne2
                , entityRefinement = M.unionWith (\p1 p2 -> \v -> p1 v && p2 v) er1 er2
                , entityToRefinements = M.unionWith (<>) e2r1 e2r2
                }

instance Ord k => Monoid (NamedEntity k) where
  mempty = NamedEntity
    { namedEntity = mempty
    , entityRefinement = mempty
    , entityToRefinements = mempty
    }

newtype NamedEntityTyped = NamedEntityTyped (NamedEntity TypeRep)
  deriving newtype (Semigroup, Monoid, Show, Eq)

newtype KeyedEntityId = KeyedEntityId RefinementId
  deriving newtype (Show, Eq, Ord, Read)

data NamedEntityKeyed = NamedEntityKeyed
  { keyToName :: M.Map Trail KeyedEntityId
  , typeOfEntities :: M.Map KeyedEntityId TypeRep
  , namedEntityKeyed :: NamedEntity KeyedEntityId
  } deriving (Show, Eq)

instance Semigroup NamedEntityKeyed where
  NamedEntityKeyed {keyToName = k2n1, typeOfEntities = tys1, namedEntityKeyed = ne1} <>
    NamedEntityKeyed {keyToName = k2n2, typeOfEntities = tys2, namedEntityKeyed = ne2} =
    NamedEntityKeyed { keyToName = M.unionWithKey (\k l r -> if l == r then l else error $ "Encountered duplicate trail: " ++ show k ++ "! Pointing to conflicting entities: " ++ show (l, r)) k2n1 k2n2 
                     , typeOfEntities = M.unionWithKey (\k l r -> if l == r then l else error $ "Encountered duplicate entity: " ++ show k ++ "! with conflicting type: " ++ show (l, r)) tys1 tys2
                     , namedEntityKeyed = ne1 <> ne2
                     }

instance Monoid NamedEntityKeyed where
  mempty = NamedEntityKeyed { keyToName = mempty
                            , typeOfEntities = mempty
                            , namedEntityKeyed = mempty
                            }
  
newtype Trail = Trail [Text]
  deriving newtype (Show, Eq, Ord, Read)

data ApiState (s :: Type) (apps :: [Type]) = ApiState
  { apiState :: M.Map TypeRep Unsafe.Any
  , namedEntityTyped :: NamedEntityTyped
  , namedEntityKeyed :: NamedEntityKeyed
  }

instance Show (ApiState s apps) where
  show (ApiState {apiState, namedEntityTyped, namedEntityKeyed}) =
    "ApiState: " ++ show (M.keys apiState) ++ ", Entites (Typed): " ++ show namedEntityTyped ++ ", Entites (Keyed): "++ show namedEntityKeyed

instance Eq (ApiState s apps) where
  ApiState {namedEntityTyped = net1, namedEntityKeyed = nek1} ==
    ApiState {namedEntityTyped = net2, namedEntityKeyed = nek2} = net1 == net2 && nek1 == nek2

modifyApiState :: forall app apps stTag s. (Typeable app, AppIsElem app apps) => DSum (stTag apps app) Proxy -> (DSum (stTag apps app) Identity -> DSum (stTag apps app) Identity) -> ApiState s apps -> ApiState s apps
modifyApiState ctor@(tag :=> _) f (ApiState {apiState = stMap, namedEntityTyped, namedEntityKeyed}) = case M.lookup (typeRep (getAppProxy' ctor)) stMap of
  Nothing -> undefined
  Just anyv -> case f (tag :=> (Identity $ castToTagVal tag anyv)) of
    _ :=> (Identity newval) -> ApiState
      { apiState = M.insert (typeRep (getAppProxy' ctor)) (Unsafe.unsafeCoerce newval :: Unsafe.Any) stMap
      , namedEntityTyped
      , namedEntityKeyed
      }
  where
    castToTagVal :: forall tag x.tag x -> Unsafe.Any -> x
    castToTagVal _ anyv = Unsafe.unsafeCoerce anyv :: x

class HasApiState (apps1 :: [Type]) stTag (apps :: [Type]) where
  apiStateUniv :: Proxy apps1 -> (forall app. Typeable app => DSum (stTag apps app) Proxy -> r) -> [r]

initApiState :: forall apps stTag s. HasApiState apps stTag apps => (forall app. Typeable app => DSum (stTag apps app) Proxy -> DSum (stTag apps app) Identity) -> ApiState s apps
initApiState f = ApiState { apiState = M.fromList $ apiStateUniv (Proxy @apps) $ \ctor -> case f ctor of
                              _ :=> (Identity v) -> (typeRep (getAppProxy' ctor), Unsafe.unsafeCoerce v :: Unsafe.Any)
                          , namedEntityTyped = mempty
                          , namedEntityKeyed = mempty
                          }


getAppProxy' :: forall stTag apps app f. Typeable app => DSum (stTag apps app) f -> Proxy app
getAppProxy' _ = Proxy


instance HasVariables (ApiState s apps) where
  getAllVariables = mempty

data SuccessApiModel s apps meth r a = SuccessApiModel
  { nextState :: Maybe (Var a -> ApiState s apps -> ApiState s apps)
  , failureNextState :: Maybe (ApiState s apps -> ApiState s apps)
  , precondition :: Maybe (ApiState s apps -> Bool)
  , validFailingAction :: Maybe (ApiState s apps -> Bool)
  , shrinkAction :: Maybe (VarContext -> ApiState s apps -> [Any (Action (ApiState s apps))])
  , postCondition :: (ApiState s apps, ApiState s apps) -> LookUp -> a -> Bool
  , postconditionOnFailure :: (ApiState s apps, ApiState s apps) -> LookUp -> Either ErrorState a -> Bool
  }

defSuccessApiModel :: SuccessApiModel s apps meth r a
defSuccessApiModel = SuccessApiModel
  { nextState = Nothing
  , failureNextState = Nothing
  , precondition = Nothing
  , validFailingAction = Nothing
  , shrinkAction = Nothing
  , postCondition = \_ _ _ -> True
  , postconditionOnFailure = \_ _ _ -> True
  }

data FailureApiModel s apps meth r a = FailureApiModel
  { failureNextState :: Maybe (ApiState s apps -> ApiState s apps)
  , precondition :: Maybe (ApiState s apps -> Bool)
  , validFailingAction :: Maybe (ApiState s apps -> Bool)
  , shrinkAction :: Maybe (VarContext -> ApiState s apps -> [Any (Action (ApiState s apps))])
  , postconditionOnFailure :: (ApiState s apps, ApiState s apps) -> LookUp -> Either ErrorState a -> Bool
  }

defFailureApiModel :: FailureApiModel s apps meth r a
defFailureApiModel = FailureApiModel
  { failureNextState = Nothing
  , precondition = Nothing
  , validFailingAction = Nothing
  , shrinkAction = Nothing
  , postconditionOnFailure = \_ _ _ -> True
  }

mkWebApiAction :: WebApiAction s apps a -> Action (ApiState s apps) a
mkWebApiAction = coerce

newtype ApiInitState apps = ApiInitState (M.Map TypeRep Unsafe.Any)

data WebApiGlobalStateModel apps = WebApiGlobalStateModel
  { appAribitaryAction :: forall (s :: Type). VarContext -> ApiState s apps -> Any (Action (ApiState s apps))
  , appInitState :: ApiInitState apps
  , namedEntityTyped :: NamedEntityTyped
  , namedEntityKeyed :: NamedEntityKeyed
  }

instance (Reifies s (WebApiGlobalStateModel apps)) => StateModel (ApiState s apps) where
  newtype Action (ApiState s apps) a = MkWebApiAction (WebApiAction s apps a)
    deriving newtype (Show, Eq, HasVariables)

  actionName = \case
    MkWebApiAction (SuccessCall creq _ _ _) -> getOpIdFromRequest creq
    MkWebApiAction (ErrorCall creq) -> getOpIdFromRequest creq
    MkWebApiAction (SomeExceptionCall creq) -> getOpIdFromRequest creq

  arbitraryAction varCxt s = case reflect (Proxy @s) of
    WebApiGlobalStateModel {appAribitaryAction} -> pure $ appAribitaryAction @s varCxt s

  initialState =
    let WebApiGlobalStateModel {appInitState, namedEntityTyped, namedEntityKeyed} = reflect (Proxy @s)
    in ApiState {apiState = coerce appInitState, namedEntityTyped, namedEntityKeyed}

  nextState s (MkWebApiAction act) var = case act of
    SuccessCall _creq SuccessApiModel {nextState=nsMay} _ _ -> maybe s (\ns -> ns var s) nsMay
    ErrorCall {} -> error "TODO:"
    SomeExceptionCall {} -> error "TODO:"

  failureNextState s (MkWebApiAction act) = case act of
    SuccessCall _creq SuccessApiModel {failureNextState=nsMay} _ _ -> maybe s (\ns -> ns s) nsMay
    ErrorCall {} -> error "TODO:"
    SomeExceptionCall {} -> error "TODO:"

  precondition s (MkWebApiAction act) = case act of
    SuccessCall _creq SuccessApiModel {precondition=pcMay} _ _ -> maybe True (\pc -> pc s) pcMay
    ErrorCall {} -> error "TODO:"
    SomeExceptionCall {} -> error "TODO:"

  validFailingAction s (MkWebApiAction act) = case act of
    SuccessCall _creq SuccessApiModel {validFailingAction=vfaMay} _ _ -> maybe False (\vfa -> vfa s) vfaMay
    ErrorCall {} -> error "TODO:"
    SomeExceptionCall {} -> error "TODO:"

  shrinkAction varCxt s (MkWebApiAction act) = case act of
    SuccessCall _creq SuccessApiModel {shrinkAction=saMay} _ _ -> maybe [] (\sa -> sa varCxt s) saMay
    ErrorCall {} -> error "TODO:"
    SomeExceptionCall {} -> error "TODO:"


data ErrorState =
  UnExpectedApiError
  { status :: H.Status
  , headerOut :: [H.Header]
--  , cookieOut :: [(ByteString, H.Cookie)]
  }
  | UnExpectedApiCrash
  { status :: H.Status
  , headerOut :: [H.Header]
  , someError :: OtherError
--  , cookieOut :: [(ByteString, H.Cookie)]
  }
  | UnExpectedApiSuccess
  { status :: H.Status
  , headerOut :: [H.Header]
--  , cookieOut :: [(ByteString, H.Cookie)]
  }
  | InputNotSetError -- TODO: Add missing field details
  | ResultError ResultError
  deriving (Show)

data ResultError = MkResultError
  { err :: T.Text
  } deriving (Show)

data ModifyClientCookies app
  = SetClientCookies [SetCookie]
  | ModifyClientCookies (ClientCookies -> ClientCookies)
  | DeleteClientCookies [ByteString]
--  deriving (Show)


instance (Reifies s (WebApiGlobalStateModel apps)) => RunModel (ApiState s apps) (WebApiSessions apps) where
  type Error (ApiState s apps) (WebApiSessions apps) = ErrorState
  perform _ act lkp = case act of
    MkWebApiAction (SuccessCall creq' _model cookModMay f) -> do
      case resolveRequest lkp creq' of
        Just creq -> testClients creq >>= \case
          Success code out headerOut cookieOut -> do
            let apiSucc = ApiSuccess {code, out, headerOut, cookieOut}
            case cookModMay of
              Nothing -> pure ()
              Just cookMod -> case cookMod apiSucc of
                modCk@(SetClientCookies setcooks) -> mapM_ (setClientCookie modCk) setcooks
                modCk@(ModifyClientCookies modcooks) -> modifyClientCookies modCk modcooks
                modCk@(DeleteClientCookies delcooks) -> mapM_ (deleteClientCookie modCk) delcooks
            pure $ either (Left . ResultError) Right $ f apiSucc
          Failure (Right oerr) -> pure $ Left UnExpectedApiCrash
                                  { status = H.status500 -- TODO: Fix this
                                  , headerOut = [] -- TODO: Fix this
                                  , someError = oerr
                                  }
          Failure (Left (ApiError code _err hd _)) -> pure $ Left UnExpectedApiError
                                                     { status = code
                                                     , headerOut = maybe [] toHeader hd
                                                     }
        Nothing -> pure $ Left InputNotSetError
    MkWebApiAction (ErrorCall creq) -> do
      testClients creq >>= \case
        Failure (Left (ApiError _ err _ _)) -> pure $ Right err
        Failure (Right oerr) -> pure $ Left UnExpectedApiCrash
                                { status = H.status500 -- TODO: Fix this
                                , headerOut = [] -- TODO: Fix this
                                , someError = oerr
                                }
        Success code _out headerOut _cookieOut -> pure $ Left $ UnExpectedApiSuccess
                                                { status = code
                                                , headerOut = toHeader headerOut
                                                }
    MkWebApiAction (SomeExceptionCall creq) -> do
      testClients creq >>= \case
        Failure (Right (OtherError e)) -> pure $ Right e
        Failure (Left (ApiError code _err hd _)) -> pure $ Left UnExpectedApiError
                                                   { status = code
                                                   , headerOut = maybe [] toHeader hd
                                                   }
        Success code _out headerOut _cookieOut -> pure $ Left $ UnExpectedApiSuccess
                                                { status = code
                                                , headerOut = toHeader headerOut
                                                }

  postcondition _ _act _lkp _a = pure True
  postconditionOnFailure _ _act _lkp _a = pure True

  monitoring (_s, s') act _lkp _res =
     QC.counterexample ("show res" ++ " <- " ++ actionName act ++ "\n  -- State: " ++ show s')
      . QC.tabulate "Registry size" ["Val1", "Val2"]

  monitoringFailure s' act _lkp err =
    QC.counterexample (show err ++ " <- " ++ actionName act ++ "\n  -- State: " ++ show s')
      . QC.tabulate "Registry size" ["Val1", "Val2"]

instance (Reifies s (WebApiGlobalStateModel apps)) => DynLogicModel (ApiState s apps) where
  restricted _ = False

successCall :: forall meth r app apps s. WebApiActionCxt apps meth app r =>
  ClientRequestVal meth (app :// r)
  -> Action (ApiState s apps) (ApiOut meth (app :// r))
successCall creq = mkWebApiAction $ SuccessCall creq defSuccessApiModel Nothing (Right . getSuccessOut)

successCallWith :: forall meth r app res apps s. (Typeable res, WebApiActionCxt apps meth app r) =>
  ClientRequestVal meth (app :// r)
  -> Maybe (ApiSuccess meth (app :// r) -> ModifyClientCookies app)
  -> (ApiSuccess meth (app :// r) -> Either ResultError res)
  -> Action (ApiState s apps) res
successCallWith creq cookModMay f = mkWebApiAction (SuccessCall creq defSuccessApiModel cookModMay f)

-- data ShowDict a where
--   ShowDict :: Show a => ShowDict a

-- showDictAction :: forall apps a. Action (ApiState s apps) a -> ShowDict a
-- showDictAction = \case
--   MkWebApiAction (SuccessCall {}) -> ShowDict

getOpIdFromRequest :: forall meth app r req. (KnownSymbol (GetOpIdName (OperationId meth (app://r))), Typeable app, Typeable r) => req meth (app://r) -> String
getOpIdFromRequest _ =
  let
    routeName = symbolVal (Proxy @(GetOpIdName (OperationId meth (app://r))))
    appName = show $ typeRep (Proxy @app)
  in appName ++ "/" ++ routeName

type family GetOpIdName (oid :: OpId) :: Symbol where
  GetOpIdName ('OpId _ n) = n
  GetOpIdName ('UndefinedOpId m r) = TypeError ('Text "OperationId is not set for " ':<>: 'ShowType m ':<>: 'Text " " ':<>: 'ShowType r)

-- instance ArgDict Show Identity where
