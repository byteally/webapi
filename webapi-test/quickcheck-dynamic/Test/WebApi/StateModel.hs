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
  , Input
  , ClientRequestF (..)
  , mkWebApiAction
  , getOpIdFromRequest
  , getSuccessOut
  , getSuccessCode
  , getSuccessHeaders
  , getSuccessCookies
  , defSuccessApiModel
  , initApiState
  , modifyApiState
  , getInput
  ) where

import Test.WebApi
import Test.QuickCheck.StateModel
import Test.QuickCheck.DynamicLogic (DynLogicModel (..))
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
import qualified Data.Map as M
import Data.Dependent.Map (DMap)
import Data.Dependent.Sum (DSum (..))
import qualified Data.Dependent.Map as DMap
import System.IO.Unsafe
import qualified Unsafe.Coerce as Unsafe
import qualified GHC.Base as Unsafe (Any)
import qualified Record
-- import Data.Reflection

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
  HKVal :: Typeable x => (x -> a) -> Record.HK Val x -> Val a
  Pair :: ((x1, x2) -> a) -> (Val x1, Val x2) -> Val a
--  ApVar :: (LookUp -> a) -> Val a
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
    Var fn v -> case a' of
      Const a -> Var (flip fn a) v
      Var fn1 v1 -> Pair (\(x, x1) -> fn x (fn1 x1)) (Var id v, Var id v1)
      HKVal fn1 hkv1 -> Pair (\(x, x1) -> fn x (fn1 x1)) (Var id v, HKVal id hkv1)
      Pair fn1 vs -> Pair (\(x, x1) -> fn x (fn1 x1)) (Var id v, Pair id vs)
    HKVal fn v -> case a' of
      Const a -> HKVal (flip fn a) v
      Var fn1 v1 -> Pair (\(x, x1) -> fn x (fn1 x1)) (HKVal id v, Var id v1)
      HKVal fn1 hkv1 -> Pair (\(x, x1) -> fn x (fn1 x1)) (HKVal id v, HKVal id hkv1)
      Pair fn1 vs -> Pair (\(x, x1) -> fn x (fn1 x1)) (HKVal id v, Pair id vs)
    Opt -> Opt

resolveVal :: LookUp -> Val a -> a
resolveVal lkp = \case
  Const v -> v
  Var f var -> f (lkp var)

data ClientRequestF f meth r = ClientRequestF
  { query :: Record.HK f (QueryParam meth r)
  , form :: Record.HK f (FormParam meth r)
  , header :: Record.HK f (HeaderIn meth r)
  , path :: Record.HK f (PathParam meth r)
  }

resolveRequest ::
  ( Record.FromHK (QueryParam meth r)
  , Record.FromHK (FormParam meth r)
  , Record.FromHK (HeaderIn meth r)
  , Record.FromHK (PathParam meth r)
  , SingMethod meth
  ) => LookUp
  -> ClientRequestF Input meth r
  -> Maybe (ClientRequest meth r)
resolveRequest lkp ClientRequestF {query, form, header, path} =
  do
    query <- Record.fromHK $ Record.hoistHK (fmap (resolveVal lkp) . getInput) query
    form <- Record.fromHK $ Record.hoistHK (fmap (resolveVal lkp) . getInput) form
    header <- Record.fromHK $ Record.hoistHK (fmap (resolveVal lkp) . getInput) header
    path <- Record.fromHK $ Record.hoistHK (fmap (resolveVal lkp) . getInput) path
    Just $ ClientRequest {query, form, header, path, file = undefined, body = undefined}

-- TODO: One of the following,
-- 1. newtype Input a = Input (Maybe (Sum Val (HK Input) a))
-- 2. newtype Input a = Input (Sum (Compose Maybe Val) (HK Input) a)
newtype Input a = Input (Maybe (Val a))

instance Functor Input where
  fmap f (Input i) = Input $ (fmap . fmap) f i

getInput :: Input a -> Maybe (Val a)
getInput (Input i) = i

data WebApiAction (apps :: [Type]) (a :: Type) where
  SuccessCall :: WebApiActionCxt apps meth app r
    => ClientRequestF Input meth (app :// r)
    -> SuccessApiModel apps meth (app :// r) res
    -> ModifyClientCookies
    -> (ApiSuccess meth (app :// r) -> Either ResultError res)
    -> WebApiAction apps res
  ErrorCall :: WebApiActionCxt apps meth app r => ClientRequest meth (app :// r) -> WebApiAction apps (ApiErr meth (app :// r))
  SomeExceptionCall :: WebApiActionCxt apps meth app r => ClientRequest meth (app :// r) -> WebApiAction apps (SomeException)

instance Show (WebApiAction apps a) where
  show = \case
    SuccessCall creq _ _ _ -> show . getOpIdFromRequest $ creq
    ErrorCall creq -> show . unsafePerformIO . toWaiRequest . fromClientRequest $ creq
    SomeExceptionCall creq -> show . unsafePerformIO . toWaiRequest . fromClientRequest $ creq

-- TODO: Revisit
instance Eq (WebApiAction apps a) where
  (==) (SuccessCall creq1 _ _ _) = \case
    SuccessCall creq2 _ _ _ -> (getOpIdFromRequest $ creq1) == (getOpIdFromRequest $ creq2)
    _ -> False
  (==) (ErrorCall creq1) = \case
    ErrorCall creq2 -> (show . unsafePerformIO . toWaiRequest . fromClientRequest $ creq1) == (show . unsafePerformIO . toWaiRequest . fromClientRequest $ creq2)
    _ -> False
  (==) (SomeExceptionCall creq1) = \case
    SomeExceptionCall creq2 -> (show . unsafePerformIO . toWaiRequest . fromClientRequest $ creq1) == (show . unsafePerformIO . toWaiRequest . fromClientRequest $ creq2)
    _ -> False

instance HasVariables (WebApiAction apps a) where
  getAllVariables = mempty

data ApiState (apps :: [Type]) = ApiState (M.Map TypeRep Unsafe.Any) -- (DMap (Tag RealWorld) Identity)

instance Show (ApiState apps) where
  show (ApiState dmap) = "undefined" -- showTaggedPrec p $DMap.dmap

instance Eq (ApiState apps) where
  s1 == s2 = undefined

modifyApiState :: forall app apps stTag. (Typeable app, AppIsElem app apps) => DSum (stTag apps app) Proxy -> (DSum (stTag apps app) Identity -> DSum (stTag apps app) Identity) -> ApiState apps -> ApiState apps
modifyApiState ctor@(tag :=> _) f (ApiState stMap) = case M.lookup (typeRep (getAppProxy' ctor)) stMap of
  Nothing -> undefined
  Just anyv -> case f (tag :=> (Identity $ castToTagVal tag anyv)) of
    _ :=> (Identity newval) -> ApiState $ M.insert (typeRep (getAppProxy' ctor)) (Unsafe.unsafeCoerce newval :: Unsafe.Any) stMap
  where
    castToTagVal :: forall tag x.tag x -> Unsafe.Any -> x
    castToTagVal _ anyv = Unsafe.unsafeCoerce anyv :: x

class HasApiState (apps1 :: [Type]) stTag (apps :: [Type]) where
  apiStateUniv :: Proxy apps1 -> (forall app. Typeable app => DSum (stTag apps app) Proxy -> r) -> [r]

initApiState :: forall apps stTag. HasApiState apps stTag apps => (forall app. Typeable app => DSum (stTag apps app) Proxy -> DSum (stTag apps app) Identity) -> ApiState apps
initApiState f = ApiState $ M.fromList $ apiStateUniv (Proxy @apps) $ \ctor -> case f ctor of
  tag :=> (Identity v) -> (typeRep (getAppProxy' ctor), Unsafe.unsafeCoerce v :: Unsafe.Any)


getAppProxy' :: forall stTag apps app f. Typeable app => DSum (stTag apps app) f -> Proxy app
getAppProxy' _ = Proxy


instance HasVariables (ApiState apps) where
  getAllVariables = mempty

data SuccessApiModel apps meth r a = SuccessApiModel
  { nextState :: Maybe (Var a -> ApiState apps -> ApiState apps)
  , failureNextState :: Maybe (ApiState apps -> ApiState apps)
  , precondition :: Maybe (ApiState apps -> Bool)
  , validFailingAction :: Maybe (ApiState apps -> Bool)
  , shrinkAction :: Maybe (VarContext -> ApiState apps -> [Any (Action (ApiState apps))])
  , postCondition :: (ApiState apps, ApiState apps) -> LookUp -> a -> Bool
  , postconditionOnFailure :: (ApiState apps, ApiState apps) -> LookUp -> Either ErrorState a -> Bool
  }

defSuccessApiModel :: SuccessApiModel apps meth r a
defSuccessApiModel = SuccessApiModel
  { nextState = Nothing
  , failureNextState = Nothing
  , precondition = Nothing
  , validFailingAction = Nothing
  , shrinkAction = Nothing
  }

data FailureApiModel apps meth r a = FailureApiModel
  { failureNextState :: Maybe (ApiState apps -> ApiState apps)
  , precondition :: Maybe (ApiState apps -> Bool)
  , validFailingAction :: Maybe (ApiState apps -> Bool)
  , shrinkAction :: Maybe (VarContext -> ApiState apps -> [Any (Action (ApiState apps))])
  , postconditionOnFailure :: (ApiState apps, ApiState apps) -> LookUp -> Either ErrorState a -> Bool
  }

mkWebApiAction :: WebApiAction apps a -> Action (ApiState apps) a
mkWebApiAction = coerce

newtype ApiInitState apps = ApiInitState (M.Map TypeRep Unsafe.Any)

instance StateModel (ApiState apps) where
  newtype Action (ApiState apps) a = MkWebApiAction (WebApiAction apps a)
    deriving newtype (Show, Eq, HasVariables)

  actionName = \case
    MkWebApiAction (SuccessCall creq _ _ _) -> getOpIdFromRequest creq
    MkWebApiAction (ErrorCall creq) -> getOpIdFromRequest creq
    MkWebApiAction (SomeExceptionCall creq) -> getOpIdFromRequest creq

  arbitraryAction _varCxt _s = pure undefined

  initialState = ApiState mempty

  nextState s (MkWebApiAction act) var = case act of
    SuccessCall creq SuccessApiModel {nextState=nsMay} _ _ -> maybe s (\ns -> ns var s) nsMay

  failureNextState s (MkWebApiAction act) = case act of
    SuccessCall creq SuccessApiModel {failureNextState=nsMay} _ _ -> maybe s (\ns -> ns s) nsMay

  precondition s (MkWebApiAction act) = case act of
    SuccessCall creq SuccessApiModel {precondition=pcMay} _ _ -> maybe True (\pc -> pc s) pcMay

  validFailingAction s (MkWebApiAction act) = case act of
    SuccessCall creq SuccessApiModel {validFailingAction=vfaMay} _ _ -> maybe False (\vfa -> vfa s) vfaMay

  shrinkAction varCxt s (MkWebApiAction act) = case act of
    SuccessCall creq SuccessApiModel {shrinkAction=saMay} _ _ -> maybe [] (\sa -> sa varCxt s) saMay


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

data ModifyClientCookies
  = SetClientCookies [SetCookie]
  | ModifyClientCookies [(ByteString, SetCookieUpdate)]
  | DeleteClientCookies [ByteString]
  | NoCookiesMod
  deriving (Show)

data SetCookieUpdate = SetCookieUpdate
  { setCookieUpdateOpName :: Text
  , setCookieUpdateOp :: SetCookie -> SetCookie
  }

instance Show SetCookieUpdate where
  show SetCookieUpdate {setCookieUpdateOpName} = T.unpack setCookieUpdateOpName


instance RunModel (ApiState apps) (WebApiSessions apps) where
  type Error (ApiState apps) (WebApiSessions apps) = ErrorState
  perform _ act lkp = case act of
    MkWebApiAction (SuccessCall creq' model cookMod f) -> do
      case resolveRequest lkp creq' of
        Just creq -> testClients creq >>= \case
          Success code out headerOut cookieOut -> pure $ either (Left . ResultError) Right $ f $ ApiSuccess {code, out, headerOut, cookieOut}
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

  monitoring (_s, s') act _lkp res =
     QC.counterexample ("show res" ++ " <- " ++ actionName act ++ "\n  -- State: " ++ show s')
      . QC.tabulate "Registry size" ["Val1", "Val2"]

  monitoringFailure s' act _lkp err =
    QC.counterexample (show err ++ " <- " ++ actionName act ++ "\n  -- State: " ++ show s')
      . QC.tabulate "Registry size" ["Val1", "Val2"]

instance DynLogicModel (ApiState apps) where
  restricted _ = False

-- data ShowDict a where
--   ShowDict :: Show a => ShowDict a

-- showDictAction :: forall apps a. Action (ApiState apps) a -> ShowDict a
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
