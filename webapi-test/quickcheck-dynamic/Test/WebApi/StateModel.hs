{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RecordWildCards #-}
module Test.WebApi.StateModel
  ( WebApiAction (..)
  , ApiState (..)
  , ApiAction (..)
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
  , NER (..)
  , HasApiSession (..)
  , successCall
  , successCallWith
  , mkApiAction
  , mkWebApiAction
  , getOpIdFromRequest
  , getSuccessOut
  , getSuccessCode
  , getSuccessHeaders
  , getSuccessCookies
  , defSuccessApiModel
  , defFailureApiModel
  , setNextState
  , setFailureNextState
  , setPrecondition
  , setValidFailingAction
  , setShrinkAction
  , setPostcondition
  , setPostconditionOnFailure
  , initApiState
  , modifyApiState
  , apiGenAction
  , addTypedEntity
  , inClass
  , notInClass
  , inClassKeyed
  , notInClassKeyed
  , fromVar
  , recToVal
  , hkToVal
  , resolveNamedEntities
  , startSession
  , endSession
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
import Data.Maybe
import Data.Hashable

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
  -- , Record.FromHK (PathParam meth (app://r))
  -- , Record.FromHK (QueryParam meth (app://r))
  -- , Record.FromHK (FormParam meth (app://r))
  -- , Record.FromHK (HeaderIn meth (app://r))
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
      Opt -> Opt
    Var fn v -> case a' of
      Const a -> Var (flip fn a) v
      Var fn1 v1 -> Pair (\(x, x1) -> fn x (fn1 x1)) (Var id v, Var id v1)
      HKVal fn1 hkv1 -> Pair (\(x, x1) -> fn x (fn1 x1)) (Var id v, HKVal id hkv1)
      Pair fn1 vs -> Pair (\(x, x1) -> fn x (fn1 x1)) (Var id v, Pair id vs)
      Opt -> Opt
    HKVal fn v -> case a' of
      Const a -> HKVal (flip fn a) v
      Var fn1 v1 -> Pair (\(x, x1) -> fn x (fn1 x1)) (HKVal id v, Var id v1)
      HKVal fn1 hkv1 -> Pair (\(x, x1) -> fn x (fn1 x1)) (HKVal id v, HKVal id hkv1)
      Pair fn1 vs -> Pair (\(x, x1) -> fn x (fn1 x1)) (HKVal id v, Pair id vs)
      Opt -> Opt
    Pair fn vs -> case a' of
      Const a -> Pair ((\f -> f a) . fn) vs
      Var fn1 v -> Pair (\(x1x2, x) -> fn x1x2 (fn1 x)) (Pair id vs, Var id v)
      HKVal fn1 hkv1 -> Pair (\(x1x2, x) -> fn x1x2 (fn1 x)) (Pair id vs, HKVal id hkv1)
      Pair fn1 vs1 -> Pair (\(x1x2, x4x5) -> fn x1x2 (fn1 x4x5)) (Pair id vs, Pair id vs1)
      Opt -> Opt
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
    Opt -> mempty

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
  ( SingMethod meth
  -- , Record.FromHK (QueryParam meth r)
  -- , Record.FromHK (FormParam meth r)
  -- , Record.FromHK (HeaderIn meth r)
  -- , Record.FromHK (PathParam meth r)
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

data AnyVal where
  SomeVal :: Val x -> AnyVal

-- inClassAny :: ApiState s apps -> RefinementId -> AnyVal -> Bool
-- inClassAny ApiState { namedEntityTyped = NamedEntityTyped NamedEntity
--                                       { namedEntity
--                                       , entityRefinement
--                                       , entityToRefinements
--                                       }
--                  } rid = \val -> undefined

-- notInClassAny :: ApiState s apps -> RefinementId -> AnyVal -> Bool
-- notInClassAny st rid = \somev -> not $ inClassAny st rid somev

inClass :: forall t apps s. Typeable t => RefinementId -> ApiState s apps -> Val t -> Bool
inClass rid ApiState { namedEntityTyped = NamedEntityTyped ne} = inClass' (typeRep (Proxy @t)) ne rid

notInClass :: forall t apps s. Typeable t => RefinementId -> ApiState s apps -> Val t -> Bool
notInClass rid st = \somev -> not $ inClass rid st somev

inClassKeyed :: forall t apps s. Typeable t => KeyedEntityId -> RefinementId -> ApiState s apps -> Val t -> Bool
inClassKeyed eid rid ApiState
  { namedEntityKeyed = NamedEntityKeyed {typeOfEntities, namedEntityKeyed}
  } = case M.lookup eid typeOfEntities of
        Nothing -> const False
        Just trep
          | typeRep (Proxy @t) == trep -> inClass' eid namedEntityKeyed rid
          | otherwise -> const False

notInClassKeyed :: forall t apps s. Typeable t => KeyedEntityId -> RefinementId -> ApiState s apps -> Val t -> Bool
notInClassKeyed eid rid st = \somev -> not $ inClassKeyed eid rid st somev

inClass' :: forall t k. (Typeable t, Ord k) => k -> NamedEntity k -> RefinementId -> Val t -> Bool
inClass' k (NamedEntity {entityRefinement, entityToRefinements}) rid =
  \val -> case M.lookup k entityToRefinements of
            Nothing -> False
            Just rids -> if Set.member rid rids
                         then fromMaybe (error $ "Panic: Refinement predicate lookup failed for: "
                                          ++ (show rid)
                                        ) (M.lookup rid entityRefinement) $ SomeVal val
                         else False

data NamedEntity k = NamedEntity
  { namedEntity :: M.Map k [Any NamedVal]
  , entityRefinement :: M.Map RefinementId (AnyVal -> Bool)
  , entityToRefinements :: M.Map k (Set.Set RefinementId)
  }

instance Show k => Show (NamedEntity k) where
  show NamedEntity {namedEntity = ne, entityRefinement = er, entityToRefinements = e2r} =
    "Entities: " ++ show ((fmap . fmap) showAnyNamedVal ne) ++ ", Refinements: " ++ (show $ M.keys er) ++ ", EntityRefinements: " ++ show e2r

instance Eq k => Eq (NamedEntity k) where
  NamedEntity {namedEntity = ne1, entityRefinement = er1, entityToRefinements = e2r1} ==
    NamedEntity {namedEntity = ne2, entityRefinement = er2, entityToRefinements = e2r2} =
    M.keys ne1 == M.keys ne2 && (M.keys er1) == (M.keys er2) && e2r1 == e2r2

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

resolveNamedEntities :: Env -> ApiState s apps -> ApiState s apps
resolveNamedEntities env ApiState {namedEntityTyped, namedEntityKeyed = NamedEntityKeyed {namedEntityKeyed, ..}, ..} =
  ApiState { namedEntityTyped = coerce $ resolveNamedEntity (coerce namedEntityTyped)
           , namedEntityKeyed = NamedEntityKeyed {namedEntityKeyed = resolveNamedEntity namedEntityKeyed, ..}
           , ..}
  where
    resolveNamedEntity :: NamedEntity k -> NamedEntity k 
    resolveNamedEntity NamedEntity {namedEntity, ..} =
      NamedEntity {namedEntity = M.map (fmap (\(Some (NamedVal {val, ..})) -> Some (NamedVal {val = Const $ resolveVal (lookUpVar env) val, ..}))) namedEntity, ..}

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

newtype SessionKey k = SessionKey k
  deriving newtype (Show, Eq, Hashable)

class HasApiSession (wa :: [Type] -> Type) where
  onStartSession :: wa apps -> m ()
  onEndSession :: wa apps -> m ()

startSession :: HasApiSession wa => wa apps -> k -> m (SessionKey k)
startSession = error "TODO"

endSession :: HasApiSession wa => wa apps -> SessionKey k -> m (SessionKey k)
endSession = error "TODO"

{-
-- name :: a -> (forall name. (a ~~ name) -> t) -> t
do
  globalKey <- startSession "global" $ do

  customerKey <- startSession "customer@example.com"
  endSession customerKey

  customerKey <- startSession "customer@example.com"
  endSession customerKey

  endSession globalKey

-}

  
data ApiState (s :: Type) (apps :: [Type]) = ApiState
  { apiState :: M.Map TypeRep Unsafe.Any
  , namedEntityTyped :: NamedEntityTyped
  , namedEntityKeyed :: NamedEntityKeyed
--  , sessionState :: M.Map SessionKey NamedEntityTyped
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
  { apiNextState :: Maybe (Var a -> ApiState s apps -> ApiState s apps)
  , apiFailureNextState :: Maybe (ApiState s apps -> ApiState s apps)
  , apiPrecondition :: Maybe (ApiState s apps -> Bool)
  , apiValidFailingAction :: Maybe (ApiState s apps -> Bool)
  , apiShrinkAction :: Maybe (VarContext -> ApiState s apps -> [Any (Action (ApiState s apps))])
  , apiPostcondition :: (ApiState s apps, ApiState s apps) -> LookUp -> a -> Bool
  , apiPostconditionOnFailure :: (ApiState s apps, ApiState s apps) -> LookUp -> Either ErrorState a -> Bool
  }

defSuccessApiModel :: SuccessApiModel s apps meth r a
defSuccessApiModel = SuccessApiModel
  { apiNextState = Nothing
  , apiFailureNextState = Nothing
  , apiPrecondition = Nothing
  , apiValidFailingAction = Nothing
  , apiShrinkAction = Nothing
  , apiPostcondition = \_ _ _ -> True
  , apiPostconditionOnFailure = \_ _ _ -> True
  }

setNextState :: (Var a -> ApiState s apps -> ApiState s apps) -> SuccessApiModel s apps meth r a -> SuccessApiModel s apps meth r a
setNextState f SuccessApiModel {..} = SuccessApiModel {apiNextState = Just f, ..}

setFailureNextState :: (ApiState s apps -> ApiState s apps) -> SuccessApiModel s apps meth r a -> SuccessApiModel s apps meth r a
setFailureNextState f SuccessApiModel {..} = SuccessApiModel {apiFailureNextState = Just f, ..}

setPrecondition :: (ApiState s apps -> Bool) -> SuccessApiModel s apps meth r a -> SuccessApiModel s apps meth r a
setPrecondition f SuccessApiModel {..} = SuccessApiModel {apiPrecondition = Just f, ..}

setValidFailingAction :: (ApiState s apps -> Bool) -> SuccessApiModel s apps meth r a -> SuccessApiModel s apps meth r a
setValidFailingAction f SuccessApiModel {..} = SuccessApiModel {apiValidFailingAction = Just f, ..}

setShrinkAction :: (VarContext -> ApiState s apps -> [Any (Action (ApiState s apps))]) -> SuccessApiModel s apps meth r a -> SuccessApiModel s apps meth r a
setShrinkAction f SuccessApiModel {..} = SuccessApiModel {apiShrinkAction = Just f, ..}

setPostcondition :: ((ApiState s apps, ApiState s apps) -> LookUp -> a -> Bool) -> SuccessApiModel s apps meth r a -> SuccessApiModel s apps meth r a
setPostcondition f SuccessApiModel {..} = SuccessApiModel {apiPostcondition = f, ..}

setPostconditionOnFailure :: ((ApiState s apps, ApiState s apps) -> LookUp -> Either ErrorState a -> Bool) -> SuccessApiModel s apps meth r a -> SuccessApiModel s apps meth r a
setPostconditionOnFailure f SuccessApiModel {..} = SuccessApiModel {apiPostconditionOnFailure = f, ..}

data FailureApiModel s apps meth r a = FailureApiModel
  { apiFailureNextState :: Maybe (ApiState s apps -> ApiState s apps)
  , apiFailurePrecondition :: Maybe (ApiState s apps -> Bool)
  , apiValidFailingAction :: Maybe (ApiState s apps -> Bool)
  , apiFailureShrinkAction :: Maybe (VarContext -> ApiState s apps -> [Any (Action (ApiState s apps))])
  , apiPostconditionOnFailure :: (ApiState s apps, ApiState s apps) -> LookUp -> Either ErrorState a -> Bool
  }

defFailureApiModel :: FailureApiModel s apps meth r a
defFailureApiModel = FailureApiModel
  { apiFailureNextState = Nothing
  , apiFailurePrecondition = Nothing
  , apiValidFailingAction = Nothing
  , apiFailureShrinkAction = Nothing
  , apiPostconditionOnFailure = \_ _ _ -> True
  }

mkWebApiAction :: WebApiAction s apps a -> Action (ApiState s apps) a
mkWebApiAction = coerce

-- newtype ApiInitState apps = ApiInitState (M.Map TypeRep Unsafe.Any)

data WebApiGlobalStateModel apps = WebApiGlobalStateModel
  { appAribitaryAction :: forall (s :: Type). VarContext -> ApiState s apps -> Any (Action (ApiState s apps))
  , appInitState :: forall (s :: Type). ApiState s apps
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
    let WebApiGlobalStateModel {appInitState} = reflect (Proxy @s)
    in appInitState

  nextState s (MkWebApiAction act) var = case act of
    SuccessCall _creq SuccessApiModel {apiNextState=nsMay} _ _ -> maybe s (\ns -> ns var s) nsMay
    ErrorCall {} -> error "TODO:"
    SomeExceptionCall {} -> error "TODO:"

  failureNextState s (MkWebApiAction act) = case act of
    SuccessCall _creq SuccessApiModel {apiFailureNextState=nsMay} _ _ -> maybe s (\ns -> ns s) nsMay
    ErrorCall {} -> error "TODO:"
    SomeExceptionCall {} -> error "TODO:"

  precondition s (MkWebApiAction act) = case act of
    SuccessCall _creq SuccessApiModel {apiPrecondition=pcMay} _ _ -> maybe True (\pc -> pc s) pcMay
    ErrorCall {} -> error "TODO:"
    SomeExceptionCall {} -> error "TODO:"

  validFailingAction s (MkWebApiAction act) = case act of
    SuccessCall _creq SuccessApiModel {apiValidFailingAction=vfaMay} _ _ -> maybe False (\vfa -> vfa s) vfaMay
    ErrorCall {} -> error "TODO:"
    SomeExceptionCall {} -> error "TODO:"

  shrinkAction varCxt s (MkWebApiAction act) = case act of
    SuccessCall _creq SuccessApiModel {apiShrinkAction=saMay} _ _ -> maybe [] (\sa -> sa varCxt s) saMay
    ErrorCall {} -> error "TODO:"
    SomeExceptionCall {} -> error "TODO:"


-- instance Functor (Action (ApiState s apps)) where
--   fmap f = \case
--     MkWebApiAction (SuccessCall creq apiModel cookMod resF) -> MkWebApiAction (SuccessCall creq apiModel cookMod (f . resF))
--     _ -> error "TODO:"

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
  -> SuccessApiModel s apps meth (app :// r) res
  -> Maybe (ApiSuccess meth (app :// r) -> ModifyClientCookies app)
  -> (ApiSuccess meth (app :// r) -> Either ResultError res)
  -> Action (ApiState s apps) res
successCallWith creq apiModel cookModMay f = mkWebApiAction (SuccessCall creq apiModel cookModMay f)

newtype ApiAction apps a = ApiAction (forall s. ApiState s apps -> Action (ApiState s apps) a)

mkApiAction :: (forall s. ApiState s apps -> Action (ApiState s apps) a) -> ApiAction apps a
mkApiAction act = ApiAction act
  
-- data ShowDict a where
--   ShowDict :: Show a => ShowDict a

-- showDictAction :: forall apps a. Action (ApiState s apps) a -> ShowDict a
-- showDictAction = \case
--   MkWebApiAction (SuccessCall {}) -> ShowDict

type EntityName = Text
data NER pred desc = NER
  { refinementId :: RefinementId
  , entityName :: EntityName
  , refinement :: Maybe pred
  , desc :: Maybe desc
  }

{-

{ id = 123
, name = "PartnerId"
, desc = "Unique reference to the active Trading Partners"
}

{ id = 124
, name = "DeletedPartnerId"
, desc = "Unique reference to the deleted Trading Partners"
, refinement = \(id : Integer) -> True
}

{ id = 125
, name = "TenantPartnerId"
, desc = "Unique reference to the Trading Partners who is the current tenant"
, refinement = \(partner : {isTenant : Bool}) -> isTenant
}

-}

data NamedVal t where
  NamedVal :: (Show t, Eq t) => {name :: RefinementId, val :: Val t} -> NamedVal t

instance Show (NamedVal t) where
  show NamedVal {name, val} = show name ++ case val of
    Const v -> show v
    Var _ v -> show v
    HKVal _ _ -> error "TODO:"
    Pair _ _ -> error "TODO:"
    Opt -> "NULL"

showAnyNamedVal :: Any NamedVal -> String
showAnyNamedVal (Some nv) = show nv

fromVar :: Typeable a => Var a -> Val a
fromVar = Var id

hkToVal :: forall t. (Typeable t, Record.FromHK t) => Record.HK Val t -> Val t
hkToVal = HKVal id

recToVal :: forall t xs. (Typeable t, Record.FromHK t, Record.ValidateRecToType xs t) => Record.HRec Val xs -> Val t
recToVal = HKVal id . Record.fromHRec

-- hkToDict :: Record.HK f x -> Record.HK (Dict c) x
-- hkToDict = undefined

-- data Dict (c :: Type -> Constraint) (t :: Type) where
--   Dict :: c t => Dict c t

instance Eq (NamedVal t) where
  NamedVal {name = n1, val = v1} == NamedVal {name = n2, val = v2}
    | n1 == n2 = case (v1, v2) of
        (Const c1, Const c2) -> c1 == c2
        (Var _ var1, Var _ var2') -> maybe False (var1 ==) $ gcast var2'
        (HKVal _ _hk1, HKVal _ _hk2') -> error "TODO:" -- maybe False (hk1 ==) $ gcast hk2'
        (Pair _ _, Pair _ _) -> error "TODO:"
        (Opt, Opt) -> True
        _ -> False
    | otherwise = False

addTypedEntity :: forall t apps s.
  ( Typeable t
  , Show t
  , Eq t
  ) => Val t
  -> NER (Val t -> Bool) Text
  -> ApiState s apps
  -> ApiState s apps
addTypedEntity val NER {}{-refinementId, entityName, refinement-} ApiState {namedEntityTyped, ..} =
  let
    ty = typeRep (Proxy @t)
    nval = NamedVal {name = undefined, val = val}
    newNET = NamedEntityTyped NamedEntity { namedEntity = M.singleton ty [Some $ nval]
                                          , entityRefinement = undefined
                                          , entityToRefinements = undefined
                                          }
  in ApiState {namedEntityTyped = namedEntityTyped <> newNET, ..}

getOpIdFromRequest :: forall meth app r req. (KnownSymbol (GetOpIdName (OperationId meth (app://r))), Typeable app, Typeable r) => req meth (app://r) -> String
getOpIdFromRequest _ =
  let
    routeName = symbolVal (Proxy @(GetOpIdName (OperationId meth (app://r))))
    appName = show $ typeRep (Proxy @app)
  in appName ++ "/" ++ routeName

apiGenAction :: forall (state :: Type) (api :: Type) (app :: Type -> Type).
  ( WebApi api
  , HasGenAction app state (Apis api)
  ) => app api
  -> VarContext
  -> state
  -> QC.Gen (Any (Action state))
apiGenAction app = getGenAction (Proxy @(Apis api)) app

class HasGenAction (app :: Type -> Type) (state :: Type) (apis :: [Type]) where
  getGenAction :: Proxy apis -> app api -> VarContext -> state -> QC.Gen (Any (Action state))

type family GetOpIdName (oid :: OpId) :: Symbol where
  GetOpIdName ('OpId _ n) = n
  GetOpIdName ('UndefinedOpId m r) = TypeError ('Text "OperationId is not set for " ':<>: 'ShowType m ':<>: 'Text " " ':<>: 'ShowType r)

-- instance ArgDict Show Identity where
