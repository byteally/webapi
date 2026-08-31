{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE QuantifiedConstraints #-}
module Test.WebApi.StateModel
  ( WebApiAction (..)
  , ApiState (..)
  , WebApiActionCxt
  , ApiSuccess (..)
  , ErrorState (..)
  , XActionError (..)
  , ResultError (..)
  , ModifyClientCookies (..)
  , SuccessApiModel
  , FailureApiModel
  , HasApiState (..)
  , HasApiStateM (..)
  , DSum (..)
  , module Test.WebApi.Val
  , ClientRequestVal (..)
  , WebApiGlobalStateModel (..)
  , NER (..)
  , HasApiSession (..)
  , WebApiSessionsCxt
  , NoXState
  , ContextSwitch (..)
  , ApiAction (..)
  , mkApiAction
  , runApiAction
  , Refinement (..)
  , refinementNames
  , ensuresRefinement
  , refutesRefinement
  , withRefutation
  , classAt
  , classAtPart
  , fillsPart
  , classOf
  , FieldClass (..)
  , ResultClass (..)
  , partName
  , requestUnset
  , andNextState
  , recordClasses
  , eventually
  , recordsAs
  , declares
  , withContract
  , lookupRefinement
  , registerClass
  , registerClassRep
  , generateAmong
  , drawEntity
  , RequestFiller (..)
  , Filler (..)
  , Part (..)
  , fills
  , freshFiller
  , constFiller
  , entityFiller
  , ApiActionWith (..)
  , ActionConfig (..)
  , ActionConfigWith (..)
  , ApiGenM
  , AnyVal (..)
  , GetOpIdName
  , runApiGenM
  , RefinementId (..)
  , initWebApiSessionsCxt
  , successCall
  , successCallWith
  , failingCall
  , failingCallWith
  , expectingFailure
  , ExpectedFailure (..)
  -- , mkApiAction
  , defaultActionConfig
  , mkWebApiAction
  , unWebApiAction
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
  , setLabel
  , andPostcondition
  , addVariables
  , initApiState
  , initApiState_
  , modifyApiState
  , apiAction
  , apiAction_
  , apiGenAction
  , addTypedEntity
  , getTypedEntities
  , getNamedEntities
  , getNamedEntitiesAny
  , removeNamedEntity
  , hasNamedEntity
  , webApiPrecondition
  , addNextState
  , elementsM
  , andPrecondition
  , shrinkRequest
  , modelOnlyDL
  , setGenerateFromDL
  , startSession
  , endSession
  , setContextDL
  , clearContextDL
  -- , genApiActionM
  -- , dlApiActionM
  -- , setContext1
  -- , clearContext1
  -- , inGlobalRange
  -- , inGlobalRange_
  -- , liftApiGen
  -- , liftApiDL
  ) where

import Test.WebApi
import Test.QuickCheck.StateModel
import Test.QuickCheck.DynamicLogic (DynLogicModel (..), DL, getModelStateDL, action, forAllNonVariableQ, withGenQ)
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString as SB
import System.Directory (getTemporaryDirectory)
import System.IO (openBinaryTempFile, hClose)
import Control.Monad (forM)
import qualified Data.ByteString.Lazy.Char8 as LBC
import Data.Char (toLower)
import Data.List (intercalate, isInfixOf)
import qualified Data.CaseInsensitive as CI
import Test.WebApi.Val
-- import Test.QuickCheck.StateModel.Variables (Any (..))
import qualified Test.QuickCheck as QC
import WebApi.Contract
import WebApi.Param
import WebApi.ContentTypes
import WebApi.Util
import Control.Exception (SomeException)
import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Kind
import Data.Typeable
import Data.Coerce
import GHC.TypeLits
import Data.String (fromString)
import qualified Network.HTTP.Types as H
import Data.Functor.Identity
import qualified Data.Text as T
import qualified Data.Map.Strict as M
import qualified Data.Set as Set
import Data.Dependent.Sum (DSum (..))
import System.IO.Unsafe
import qualified Unsafe.Coerce as Unsafe
import qualified GHC.Base as Unsafe (Any)
import Data.Reflection
import Data.Hashable
import Data.Maybe (fromMaybe)
import GHC.Generics (Rep)
import Data.Bifunctor
import Control.Monad.IO.Class
import Control.Concurrent (threadDelay)
import Control.Monad.Reader
import Control.Monad (foldM)

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
  )

-- ApiSuccess and its accessors moved to WebApi.Client.Session (M8a);
-- re-exported here unchanged via the open Test.WebApi import.

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

-- | 'Left' names the request field nobody supplied; the suppliers are the
-- step's ('stepSuppliers').
resolveRequest ::
  ( SingMethod meth
  ) => Suppliers
  -> LookUp
  -> ClientRequestVal meth r
  -> Either Text (ClientRequest meth r)
resolveRequest sup lkp ClientRequestVal {query, form, header, path, file, body} =
  do
    query' <- resolveValWith sup lkp query
    form' <- resolveValWith sup lkp form
    header' <- resolveValWith sup lkp header
    path' <- resolveValWith sup lkp path
    file' <- resolveValWith sup lkp file
    body' <- resolveValWith sup lkp body
    Right $ ClientRequest {query = query', form = form', header = header', path = path', file = file', body = body'}

-- | The labels of the request's fresh leaves.
requestFreshLabels :: ClientRequestVal meth r -> Set.Set Text
requestFreshLabels ClientRequestVal {query, form, header, path, file, body} =
  freshLabels query <> freshLabels form <> freshLabels header <> freshLabels path <> freshLabels file <> freshLabels body

-- | The files the request carries, by name.
requestTempFiles :: ClientRequestVal meth r -> [(Text, Val ByteString)]
requestTempFiles ClientRequestVal {query, form, header, path, file, body} =
  tempFiles query ++ tempFiles form ++ tempFiles header ++ tempFiles path ++ tempFiles file ++ tempFiles body

-- | What nobody supplied in the request (see 'unsetLeaves').
requestUnset :: ClientRequestVal meth r -> [Text]
requestUnset ClientRequestVal {query, form, header, path, file, body} =
  unsetLeaves query ++ unsetLeaves form ++ unsetLeaves header ++ unsetLeaves path ++ unsetLeaves file ++ unsetLeaves body

-- | Start a step: make its fresh tokens, write its files.
stepSuppliers :: LookUp -> ClientRequestVal meth r -> WebApiSessions apps Suppliers
stepSuppliers lkp creq = do
  beginStep
  mapM_ freshToken (Set.toList (requestFreshLabels creq))
  toks <- freshTokens
  let
    -- a file's content may itself use the step's tokens and earlier results
    contentOf n cv = either (\why -> error $ T.unpack $ "file " <> n <> ": " <> why) id $
      resolveValWith Suppliers {freshOf = freshLookup toks, fileOf = \m -> Left ("file " <> m <> " inside a file")} lkp cv
  paths <- liftIO $ forM (requestTempFiles creq) $ \(n, cv) -> do
    dir <- getTemporaryDirectory
    (p, h) <- openBinaryTempFile dir (T.unpack (T.map (\ch -> if ch == '/' then '_' else ch) n))
    SB.hPut h (contentOf n cv)
    hClose h
    pure (n, p)
  pure Suppliers
    { freshOf = freshLookup toks
    , fileOf = \n -> maybe (Left ("runner bug: no file was written for " <> n)) Right (lookup n paths)
    }

freshLookup :: M.Map Text Text -> Text -> Either Text Text
freshLookup toks l = maybe (Left ("runner bug: no token was made for fresh " <> l)) Right (M.lookup l toks)

-- | For a report: a fresh leaf shows as its label, a file as its name.
placeholders :: Suppliers
placeholders = Suppliers
  { freshOf = \l -> Right ("<fresh " <> l <> ">")
  , fileOf = \n -> Right ("<file " <> T.unpack n <> ">")
  }

-- | Every request with one leaf shrunk (see 'shrinkVal').
shrinkRequest :: VarContext -> ClientRequestVal meth r -> [ClientRequestVal meth r]
shrinkRequest vctx ClientRequestVal {..} =
  [ClientRequestVal {query = v, ..} | v <- shrinkVal vctx query]
  ++ [ClientRequestVal {form = v, ..} | v <- shrinkVal vctx form]
  ++ [ClientRequestVal {header = v, ..} | v <- shrinkVal vctx header]
  ++ [ClientRequestVal {path = v, ..} | v <- shrinkVal vctx path]
  ++ [ClientRequestVal {file = v, ..} | v <- shrinkVal vctx file]
  ++ [ClientRequestVal {body = v, ..} | v <- shrinkVal vctx body]

data WebApiAction s (c :: Type) (xstate :: Type) (apps :: [Type]) (a :: Type) where
  SuccessCall :: (WebApiActionCxt apps meth app r, Typeable res, Show res, Eq (Action xstate res))
    => ClientRequestVal meth (app :// r)
    -> SuccessApiModel s c xstate apps meth (app :// r) res
    -> Maybe (ApiSuccess meth (app :// r) -> ModifyClientCookies app)
    -> (ApiSuccess meth (app :// r) -> Either ResultError res)
    -> WebApiAction s c xstate apps res
  -- | A call expected to fail (with a given status): the step succeeds
  -- when it does, its result is what the caller makes of the decoded
  -- error, bound like any other result. Everything else — model, request,
  -- shrinking — is as for 'SuccessCall'; only which branch of the
  -- response is the good one differs.
  ErrorCall :: (WebApiActionCxt apps meth app r, Typeable res, Show res, Eq (Action xstate res))
    => ClientRequestVal meth (app :// r)
    -> ExpectedFailure
    -> SuccessApiModel s c xstate apps meth (app :// r) res
    -> (ApiError meth (app :// r) -> Either ResultError res)
    -> WebApiAction s c xstate apps res
  SomeExceptionCall :: WebApiActionCxt apps meth app r
    => ClientRequest meth (app :// r)
    -> WebApiAction s c xstate apps (SomeException)
  SetContext :: (ContextSwitch c) => c -> WebApiAction s c xstate apps ()
  ClearContext :: (ContextSwitch c) => Proxy c -> WebApiAction s c xstate apps ()
  -- | A named step that only moves the model (nothing is performed):
  -- seeding knowledge, scoping generation ('setGenerateFromDL'), …
  ModelOnly :: String -> (ApiState s c xstate apps -> ApiState s c xstate apps) -> WebApiAction s c xstate apps ()
  -- | A step whose result is a token made when it runs (unique to the
  -- execution): a fresh value the DL binds like any other result, so a
  -- request and a later assertion can share it and a replay makes a new one.
  FreshValue :: Text -> WebApiAction s c xstate apps Text
  -- | A step that performs an effect outside the web API — a foreign
  -- transform, say — its result bound like any other. The variables the
  -- effect's closure references are declared up front, so the DL keeps the
  -- steps that bind them; the effect receives the 'LookUp' that resolves
  -- them. A 'Left' is the effect's own failure, reported at the step.
  EffectCall :: (Typeable res, Show res, Eq res, Eq (Action xstate res))
    => String
    -> Set.Set (Any Var)
    -> (LookUp -> IO (Either T.Text res))
    -> WebApiAction s c xstate apps res
  XAction :: Action xstate a -> WebApiAction s c xstate apps a

-- | What an 'ErrorCall' expects of the response.
data ExpectedFailure
  = AnyFailure
    -- ^ any error status
  | FailureStatus Int
    -- ^ this HTTP status
  deriving (Show, Eq)

matchesFailure :: ExpectedFailure -> H.Status -> Bool
matchesFailure AnyFailure _ = True
matchesFailure (FailureStatus n) st = H.statusCode st == n

expectedFailureText :: ExpectedFailure -> String
expectedFailureText = \case
  AnyFailure -> "expect failure"
  FailureStatus n -> "expect " ++ show n

class (Hashable c, Show c, Eq c, Typeable c) => ContextSwitch c where
  setContext :: Hashed c -> IO ()
  clearContext :: Proxy c -> IO ()

instance ContextSwitch () where
  setContext _ = pure ()
  clearContext _ = pure ()
  
instance StateModel xstate => Show (WebApiAction s c xstate apps a) where
  show = \case
    SuccessCall creq model _ _ -> getOpIdFromRequest creq ++ labelSuffix model
    ErrorCall creq expected model _ -> getOpIdFromRequest creq ++ " (" ++ expectedFailureText expected ++ ")" ++ labelSuffix model
    SomeExceptionCall creq -> show . unsafePerformIO . toWaiRequest . fromClientRequest $ creq
    SetContext c -> "Set-Context: " ++ show c
    ClearContext pc -> "Clear-Context: " ++ show (typeRep pc)
    ModelOnly n _ -> "Model: " ++ n
    FreshValue l -> "fresh " ++ T.unpack l
    EffectCall n _ _ -> n
    XAction xact -> show xact

-- TODO: Revisit
instance Eq (Action xstate a) => Eq (WebApiAction s c xstate apps a) where
  -- a labelled step (a script's) is itself; generated ones are their operation
  (==) (SuccessCall creq1 SuccessApiModel {label = l1} _ _) = \case
    SuccessCall creq2 SuccessApiModel {label = l2} _ _ -> getOpIdFromRequest creq1 == getOpIdFromRequest creq2 && l1 == l2
    _ -> False
  (==) (ErrorCall creq1 expected1 SuccessApiModel {label = l1} _) = \case
    ErrorCall creq2 expected2 SuccessApiModel {label = l2} _ -> getOpIdFromRequest creq1 == getOpIdFromRequest creq2 && expected1 == expected2 && l1 == l2
    _ -> False
  (==) (SomeExceptionCall creq1) = \case
    SomeExceptionCall creq2 -> (show . unsafePerformIO . toWaiRequest . fromClientRequest $ creq1) == (show . unsafePerformIO . toWaiRequest . fromClientRequest $ creq2)
    _ -> False
  (==) (SetContext c1) = \case
    SetContext c2' -> maybe False (== (Identity c1)) $ gcast (Identity c2')
    _ -> False
  (==) ClearContext {} = \case
    ClearContext {} -> True
    _ -> False
  (==) (ModelOnly n1 _) = \case
    ModelOnly n2 _ -> n1 == n2
    _ -> False
  (==) (FreshValue l1) = \case
    FreshValue l2 -> l1 == l2
    _ -> False
  (==) (EffectCall n1 _ _) = \case
    EffectCall n2 _ _ -> n1 == n2
    _ -> False
  (==) (XAction xact1) = \case
    XAction xact2 -> xact1 == xact2
    _ -> False

instance StateModel xstate => HasVariables (WebApiAction s c xstate apps a) where
  getAllVariables = \case
    SuccessCall creq SuccessApiModel {apiVariables} _ _ -> getAllVariables creq <> apiVariables
    ErrorCall creq _ SuccessApiModel {apiVariables} _ -> getAllVariables creq <> apiVariables
    SomeExceptionCall {} -> mempty
    SetContext {} -> mempty
    ClearContext {} -> mempty
    ModelOnly {} -> mempty
    FreshValue {} -> mempty
    EffectCall _ vars _ -> vars
    XAction xact -> getAllVariables xact

newtype RefinementId = RefinementId Text
  deriving newtype (Show, Eq, Ord, Read)

data AnyVal where
  SomeVal :: Typeable x => Val x -> AnyVal

-- | The entities the model knows, by type: each carries the class
-- ('RefinementId') it was recorded under. A class is a *name* the model
-- (or a script) gives a value at a position — never a property of the
-- value: values are symbolic here ('Var'), so nothing can be decided from
-- their content until the step runs (that is a postcondition's job; see
-- the refinements of "Test.WebApi.Dhall").
newtype NamedEntity k = NamedEntity
  { namedEntity :: M.Map k [Any NamedVal]
  }

instance Show k => Show (NamedEntity k) where
  show NamedEntity {namedEntity = ne} = "Entities: " ++ show ((fmap . fmap) showAnyNamedVal ne)

instance Eq k => Eq (NamedEntity k) where
  NamedEntity {namedEntity = ne1} == NamedEntity {namedEntity = ne2} = M.keys ne1 == M.keys ne2

instance Ord k => Semigroup (NamedEntity k) where
  NamedEntity {namedEntity = ne1} <> NamedEntity {namedEntity = ne2} = NamedEntity { namedEntity = M.unionWith (<>) ne1 ne2 }

instance Ord k => Monoid (NamedEntity k) where
  mempty = NamedEntity { namedEntity = mempty }

newtype NamedEntityTyped = NamedEntityTyped (NamedEntity TypeRep)
  deriving newtype (Semigroup, Monoid, Show, Eq)

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
  globalKey <- startSession "global" 

  customerKey <- startSession "customer@example.com"
  endSession customerKey

  customerKey <- startSession "customer@example.com"
  endSession customerKey

  endSession globalKey

  usingContext "user1@example.com" $ fooAct
  usingContext "user1@example.com" $ anyActions
  usingContext "user2@example.com" $ fooAct

-}

  
data ApiState (s :: Type) (c :: Type) (xstate :: Type) (apps :: [Type]) = ApiState
  { apiState :: M.Map TypeRep Unsafe.Any
  , namedEntityTyped :: NamedEntityTyped
  , defaultContext :: Maybe c
  , currentContext :: Maybe c
  , xActionState :: xstate
  , generateFrom :: Maybe (Set.Set Text)
    -- ^ the actions (by the name the generator knows them under) generation
    -- draws from; 'Nothing' = every registered action
  , classTypes :: M.Map RefinementId TypeRep
    -- ^ the type each registered class holds ('registerClass'); recording
    -- another type under it is a model bug
--  , sessionState :: M.Map SessionKey NamedEntityTyped
  }

instance Show (ApiState s c xstate apps) where
  show (ApiState {apiState, namedEntityTyped}) =
    "ApiState: " ++ show (M.keys apiState) ++ ", " ++ show namedEntityTyped

instance Eq (ApiState s c xstate apps) where
  ApiState {namedEntityTyped = net1} == ApiState {namedEntityTyped = net2} = net1 == net2

modifyApiState :: forall app apps stTag c xstate s. (Typeable app, AppIsElem app apps) => DSum (stTag apps app) Proxy -> (DSum (stTag apps app) Identity -> DSum (stTag apps app) Identity) -> ApiState s c xstate apps -> ApiState s c xstate apps
modifyApiState ctor@(tag :=> _) f (ApiState {apiState = stMap, ..}) = case M.lookup (typeRep (getAppProxy' ctor)) stMap of
  Nothing -> undefined
  Just anyv -> case f (tag :=> (Identity $ castToTagVal tag anyv)) of
    _ :=> (Identity newval) -> ApiState
      { apiState = M.insert (typeRep (getAppProxy' ctor)) (Unsafe.unsafeCoerce newval :: Unsafe.Any) stMap
      , ..
      }
  where
    castToTagVal :: forall tag x.tag x -> Unsafe.Any -> x
    castToTagVal _ anyv = Unsafe.unsafeCoerce anyv :: x

class HasApiState (apps1 :: [Type]) stTag (apps :: [Type]) where
  apiStateUniv :: Proxy apps1 -> (forall app. Typeable app => DSum (stTag apps app) Proxy -> r) -> [r]

initApiState :: forall c xstate apps stTag s. HasApiState apps stTag apps =>
  (forall app. Typeable app => DSum (stTag apps app) Proxy -> DSum (stTag apps app) Identity)
  -> xstate
  -> ApiState s c xstate apps
initApiState f xstate = ApiState
  { apiState = M.fromList $ apiStateUniv (Proxy @apps) $ \ctor -> case f ctor of
      _ :=> (Identity v) -> (typeRep (getAppProxy' ctor), Unsafe.unsafeCoerce v :: Unsafe.Any)
  , namedEntityTyped = mempty
  , defaultContext = Nothing
  , currentContext = Nothing
  , xActionState = xstate
  , generateFrom = Nothing
  , classTypes = mempty
  }

-- | Register a class with the type its entities have; a second
-- registration with another type is an error.
registerClass :: forall t c xstate apps s. Typeable t => RefinementId -> ApiState s c xstate apps -> ApiState s c xstate apps
registerClass rid = registerClassRep rid (typeRep (Proxy @t))

registerClassRep :: RefinementId -> TypeRep -> ApiState s c xstate apps -> ApiState s c xstate apps
registerClassRep rid ty ApiState {classTypes, ..} = case M.lookup rid classTypes of
  Just ty' | ty' /= ty -> error $ "class " ++ show rid ++ " is registered with two types: " ++ show ty' ++ " and " ++ show ty
  _ -> ApiState {classTypes = M.insert rid ty classTypes, ..}

initApiState_ :: forall c apps stTag s. HasApiState apps stTag apps =>
  (forall app. Typeable app => DSum (stTag apps app) Proxy -> DSum (stTag apps app) Identity)
  -> ApiState s c NoXState apps
initApiState_ f = initApiState f NoXState

getAppProxy' :: forall stTag apps app f. Typeable app => DSum (stTag apps app) f -> Proxy app
getAppProxy' _ = Proxy


instance HasVariables (ApiState s c xstate apps) where
  getAllVariables = mempty

data SuccessApiModel s c xstate apps meth r a = SuccessApiModel
  { apiNextState :: Maybe (Var a -> ApiState s c xstate apps -> ApiState s c xstate apps)
  , apiFailureNextState :: Maybe (ApiState s c xstate apps -> ApiState s c xstate apps)
  , apiPrecondition :: Maybe (ApiState s c xstate apps -> Bool)
  , apiValidFailingAction :: Maybe (ApiState s c xstate apps -> Bool)
  , apiShrinkAction :: Maybe (VarContext -> ApiState s c xstate apps -> [Any (Action (ApiState s c xstate apps))])
  , apiPostcondition :: (ApiState s c xstate apps, ApiState s c xstate apps) -> LookUp -> ClientRequest meth r -> a -> Either Text ()
    -- ^ over the request as it went out and the result of the call; 'Left'
    -- says why it does not hold (the counterexample carries it)
  , apiPostconditionOnFailure :: (ApiState s c xstate apps, ApiState s c xstate apps) -> LookUp -> Either ErrorState a -> Bool
  , apiVariables :: Set.Set (Any Var)
    -- ^ variables the model's hooks look up besides the request's (a
    -- postcondition over an earlier result): the DL must know them, or
    -- shrinking may drop the step that binds them and keep this one
  , label :: Maybe Text
    -- ^ the step's name where it was written (a script's rule), for the
    -- counterexample; 'actionName' stays the operation id
  , apiEnsures :: Set.Set RefinementId
    -- ^ the refinements the step's value is checked against ('ensuresRefinement')
  , apiRefutes :: Set.Set RefinementId
    -- ^ the refinements the step's value is checked to be outside of ('refutesRefinement')
  , apiRecords :: Set.Set RefinementId
    -- ^ the classes the step records its value under ('recordsAs')
  , apiRetry :: Maybe (Int, Int)
    -- ^ (attempts, milliseconds between): the call is repeated while it
    -- fails or its result is refused, for what the platform does
    -- asynchronously ('eventually')
  }

defSuccessApiModel :: SuccessApiModel s c xstate apps meth r a
defSuccessApiModel = SuccessApiModel
  { apiNextState = Nothing
  , apiFailureNextState = Nothing
  , apiPrecondition = Nothing
  , apiValidFailingAction = Nothing
  , apiShrinkAction = Nothing
  , apiPostcondition = \_ _ _ _ -> Right ()
  , apiPostconditionOnFailure = \_ _ _ -> True
  , label = Nothing
  , apiVariables = mempty
  , apiEnsures = mempty
  , apiRefutes = mempty
  , apiRecords = mempty
  , apiRetry = Nothing
  }

-- | Repeat the call up to the given attempts, that many milliseconds
-- apart, while it fails or its result mapping refuses the response —
-- for what the platform only eventually shows (a file a poller picks up).
eventually :: Int -> Int -> SuccessApiModel s c xstate apps meth r a -> SuccessApiModel s c xstate apps meth r a
eventually attempts ms SuccessApiModel {..} = SuccessApiModel {apiRetry = Just (attempts, ms), ..}

labelSuffix :: SuccessApiModel s c xstate apps meth r a -> String
labelSuffix SuccessApiModel {label} = maybe "" (\l -> " -- " ++ T.unpack l) label

setNextState :: (Var a -> ApiState s c xstate apps -> ApiState s c xstate apps) -> SuccessApiModel s c xstate apps meth r a -> SuccessApiModel s c xstate apps meth r a
setNextState f SuccessApiModel {..} = SuccessApiModel {apiNextState = Just f, ..}

setFailureNextState :: (ApiState s c xstate apps -> ApiState s c xstate apps) -> SuccessApiModel s c xstate apps meth r a -> SuccessApiModel s c xstate apps meth r a
setFailureNextState f SuccessApiModel {..} = SuccessApiModel {apiFailureNextState = Just f, ..}

setPrecondition :: (ApiState s c xstate apps -> Bool) -> SuccessApiModel s c xstate apps meth r a -> SuccessApiModel s c xstate apps meth r a
setPrecondition f SuccessApiModel {..} = SuccessApiModel {apiPrecondition = Just f, ..}

setValidFailingAction :: (ApiState s c xstate apps -> Bool) -> SuccessApiModel s c xstate apps meth r a -> SuccessApiModel s c xstate apps meth r a
setValidFailingAction f SuccessApiModel {..} = SuccessApiModel {apiValidFailingAction = Just f, ..}

setShrinkAction :: (VarContext -> ApiState s c xstate apps -> [Any (Action (ApiState s c xstate apps))]) -> SuccessApiModel s c xstate apps meth r a -> SuccessApiModel s c xstate apps meth r a
setShrinkAction f SuccessApiModel {..} = SuccessApiModel {apiShrinkAction = Just f, ..}

setPostcondition :: ((ApiState s c xstate apps, ApiState s c xstate apps) -> LookUp -> ClientRequest meth r -> a -> Either Text ()) -> SuccessApiModel s c xstate apps meth r a -> SuccessApiModel s c xstate apps meth r a
setPostcondition f SuccessApiModel {..} = SuccessApiModel {apiPostcondition = f, ..}

-- | Require another postcondition besides the model's own (the model's is
-- checked first; the first failure is reported).
andPostcondition :: ((ApiState s c xstate apps, ApiState s c xstate apps) -> LookUp -> ClientRequest meth r -> a -> Either Text ()) -> SuccessApiModel s c xstate apps meth r a -> SuccessApiModel s c xstate apps meth r a
andPostcondition f SuccessApiModel {apiPostcondition, ..} = SuccessApiModel {apiPostcondition = \ss lkp req a -> apiPostcondition ss lkp req a >> f ss lkp req a, ..}

setLabel :: Text -> SuccessApiModel s c xstate apps meth r a -> SuccessApiModel s c xstate apps meth r a
setLabel l SuccessApiModel {..} = SuccessApiModel {label = Just l, ..}

-- | Declare variables the model's hooks look up (see 'apiVariables').
addVariables :: Set.Set (Any Var) -> SuccessApiModel s c xstate apps meth r a -> SuccessApiModel s c xstate apps meth r a
addVariables vs SuccessApiModel {apiVariables, ..} = SuccessApiModel {apiVariables = apiVariables <> vs, ..}

setPostconditionOnFailure :: ((ApiState s c xstate apps, ApiState s c xstate apps) -> LookUp -> Either ErrorState a -> Bool) -> SuccessApiModel s c xstate apps meth r a -> SuccessApiModel s c xstate apps meth r a
setPostconditionOnFailure f SuccessApiModel {..} = SuccessApiModel {apiPostconditionOnFailure = f, ..}

data FailureApiModel s c xstate apps meth r a = FailureApiModel
  { apiFailureNextState :: Maybe (ApiState s c xstate apps -> ApiState s c xstate apps)
  , apiFailurePrecondition :: Maybe (ApiState s c xstate apps -> Bool)
  , apiValidFailingAction :: Maybe (ApiState s c xstate apps -> Bool)
  , apiFailureShrinkAction :: Maybe (VarContext -> ApiState s c xstate apps -> [Any (Action (ApiState s c xstate apps))])
  , apiPostconditionOnFailure :: (ApiState s c xstate apps, ApiState s c xstate apps) -> LookUp -> Either ErrorState a -> Bool
  }

defFailureApiModel :: FailureApiModel s c xstate apps meth r a
defFailureApiModel = FailureApiModel
  { apiFailureNextState = Nothing
  , apiFailurePrecondition = Nothing
  , apiValidFailingAction = Nothing
  , apiFailureShrinkAction = Nothing
  , apiPostconditionOnFailure = \_ _ _ -> True
  }

mkWebApiAction :: WebApiAction s c xstate apps a -> Action (ApiState s c xstate apps) a
mkWebApiAction = coerce

unWebApiAction :: Action (ApiState s c xstate apps) a -> WebApiAction s c xstate apps a
unWebApiAction (MkWebApiAction a) = a

-- newtype ApiInitState apps = ApiInitState (M.Map TypeRep Unsafe.Any)

data WebApiGlobalStateModel c xstate apps = WebApiGlobalStateModel
  { appAribitaryAction :: forall (s :: Type). VarContext -> ApiState s c xstate apps -> QC.Gen (Any (Action (ApiState s c xstate apps)))
  , appInitState :: forall (s :: Type). ApiState s c xstate apps
  }

deriving newtype instance Eq (Action xstate a) => Eq (Action (ApiState s c xstate apps) a)

instance ( Reifies s (WebApiGlobalStateModel c xstate apps)
         , StateModel xstate
         , forall a. HasVariables (Action xstate a)
         ) => StateModel (ApiState s c xstate apps) where
  newtype Action (ApiState s c xstate apps) a = MkWebApiAction (WebApiAction s c xstate apps a)
    deriving newtype (Show, HasVariables)

  actionName = \case
    MkWebApiAction (SuccessCall creq _ _ _) -> getOpIdFromRequest creq
    MkWebApiAction (ErrorCall creq _ _ _) -> getOpIdFromRequest creq
    MkWebApiAction (SomeExceptionCall creq) -> getOpIdFromRequest creq
    MkWebApiAction a@(SetContext {}) -> show a
    MkWebApiAction a@(ClearContext {}) -> show a
    MkWebApiAction a@(ModelOnly {}) -> show a
    MkWebApiAction (FreshValue {}) -> "fresh"
    MkWebApiAction (EffectCall n _ _) -> n
    MkWebApiAction (XAction xact) -> actionName xact

  arbitraryAction varCxt s = case reflect (Proxy @s) of
    WebApiGlobalStateModel {appAribitaryAction} -> appAribitaryAction @s varCxt s

  initialState =
    let WebApiGlobalStateModel {appInitState} = reflect (Proxy @s)
    in appInitState

  nextState s@ApiState{xActionState, ..} (MkWebApiAction act) var = case act of
    SuccessCall _creq SuccessApiModel {apiNextState=nsMay} _ _ -> maybe s (\ns -> ns var s) nsMay
    ErrorCall _creq _ SuccessApiModel {apiNextState=nsMay} _ -> maybe s (\ns -> ns var s) nsMay
    SomeExceptionCall {} -> s
    SetContext c -> ApiState {xActionState, currentContext = Just c, ..}
    ClearContext {} -> ApiState {xActionState, currentContext = Nothing, ..}
    ModelOnly _ f -> f s
    FreshValue {} -> s
    EffectCall {} -> s
    XAction xact -> ApiState {xActionState = nextState xActionState xact var, ..}

  failureNextState s@ApiState{xActionState, ..} (MkWebApiAction act) = case act of
    SuccessCall _creq SuccessApiModel {apiFailureNextState=nsMay} _ _ -> maybe s (\ns -> ns s) nsMay
    ErrorCall _creq _ SuccessApiModel {apiFailureNextState=nsMay} _ -> maybe s (\ns -> ns s) nsMay
    SomeExceptionCall {} -> s
    SetContext {} -> s
    ClearContext {} -> s
    ModelOnly {} -> s
    FreshValue {} -> s
    EffectCall {} -> s
    XAction xact -> ApiState {xActionState = failureNextState xActionState xact, ..}

  precondition s (MkWebApiAction act) = webApiPrecondition s act

  validFailingAction s@ApiState{xActionState} (MkWebApiAction act) = case act of
    SuccessCall _creq SuccessApiModel {apiValidFailingAction=vfaMay} _ _ -> maybe False (\vfa -> vfa s) vfaMay
    ErrorCall _creq _ SuccessApiModel {apiValidFailingAction=vfaMay} _ -> maybe False (\vfa -> vfa s) vfaMay
    SomeExceptionCall {} -> False
    SetContext {} -> False
    ClearContext {} -> False
    ModelOnly {} -> False
    FreshValue {} -> False
    EffectCall {} -> False
    XAction xact -> validFailingAction xActionState xact

  shrinkAction varCxt s@ApiState{xActionState} (MkWebApiAction act) = case act of
    -- by default a request shrinks one leaf at a time (a variable to an
    -- earlier one of its type); the model may say otherwise
    SuccessCall creq model@SuccessApiModel {apiShrinkAction=saMay} cookMod f -> case saMay of
      Just sa -> sa varCxt s
      Nothing -> [Some (MkWebApiAction (SuccessCall creq' model cookMod f)) | creq' <- shrinkRequest varCxt creq]
    ErrorCall creq expected model@SuccessApiModel {apiShrinkAction=saMay} f -> case saMay of
      Just sa -> sa varCxt s
      Nothing -> [Some (MkWebApiAction (ErrorCall creq' expected model f)) | creq' <- shrinkRequest varCxt creq]
    SomeExceptionCall {} -> []
    SetContext {} -> []
    ClearContext {} -> []
    ModelOnly {} -> []
    FreshValue {} -> []
    EffectCall {} -> []
    XAction xact -> fmap (\(Some xact') -> Some $ MkWebApiAction $ XAction xact') $ shrinkAction varCxt xActionState xact


-- instance Functor (Action (ApiState s c xstate apps)) where
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
  | UnexpectedStatus -- ^ the call failed, but not as an 'ErrorCall' expected
  { expected :: ExpectedFailure
  , status :: H.Status
  , headerOut :: [H.Header]
  }
  | InputNotSetError Text -- ^ the request field nobody supplied
  | ResultError ResultError
  | XActionError AnyXActionError
  deriving (Show)

newtype XActionError (e :: Type) = MkXActionError e
  deriving newtype (Show, Eq)

data AnyXActionError where
  AnyXActionError :: (Typeable e, Show e) => XActionError e -> AnyXActionError

instance Show AnyXActionError where
  show (AnyXActionError a) = show a  
  
data ResultError = MkResultError
  { err :: T.Text
  } deriving (Show)

-- ModifyClientCookies moved to WebApi.Client.Session (M8a).

data WebApiSessionsCxt = WebApiSessionsCxt
  { -- defaultClientsState :: ClientsState
  }

initWebApiSessionsCxt :: WebApiSessionsCxt
initWebApiSessionsCxt = WebApiSessionsCxt
  {
  }

instance ( Reifies s (WebApiGlobalStateModel c xstate apps)
         , RunModel xstate IO
         , Typeable e
         , Show e
         , XActionError e ~ Error xstate IO
         , RunModel xstate IO
         , StateModel xstate
         ) => RunModel (ApiState s c xstate apps) (ReaderT WebApiSessionsCxt (WebApiSessions apps)) where
  type Error (ApiState s c xstate apps) (ReaderT WebApiSessionsCxt (WebApiSessions apps)) = ErrorState
  perform (ApiState {xActionState}) act lkp = case act of
    MkWebApiAction (SuccessCall creq' SuccessApiModel {apiRetry} cookModMay f) -> ReaderT $ \_ -> do
      sup <- stepSuppliers lkp creq'
      case resolveRequest sup lkp creq' of
        Right creq -> do
          let
            once = testClients creq >>= \case
              Success code out headerOut cookieOut -> do
                let apiSucc = ApiSuccess {code, out, headerOut, cookieOut, sent = creq}
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
            -- what the platform does asynchronously shows up eventually
            attempt n = once >>= \case
              Left _ | n > 1 -> liftIO (threadDelay (1000 * maybe 0 snd apiRetry)) >> attempt (n - 1)
              r -> pure r
          attempt (maybe 1 fst apiRetry)
        Left field -> pure $ Left (InputNotSetError field)
    MkWebApiAction (ErrorCall creq' expected _model f) -> ReaderT $ \_ -> do
      sup <- stepSuppliers lkp creq'
      case resolveRequest sup lkp creq' of
        Right creq -> testClients creq >>= \case
          Failure (Left apiErr@(ApiError code _err hd _))
            | matchesFailure expected code -> pure $ either (Left . ResultError) Right $ f apiErr
            | otherwise -> pure $ Left UnexpectedStatus
                                  { expected
                                  , status = code
                                  , headerOut = maybe [] toHeader hd
                                  }
          Failure (Right oerr) -> pure $ Left UnExpectedApiCrash
                                  { status = H.status500 -- TODO: Fix this
                                  , headerOut = [] -- TODO: Fix this
                                  , someError = oerr
                                  }
          Success code _out headerOut _cookieOut -> pure $ Left $ UnExpectedApiSuccess
                                                  { status = code
                                                  , headerOut = toHeader headerOut
                                                  }
        Left field -> pure $ Left (InputNotSetError field)
    MkWebApiAction (SomeExceptionCall creq) -> ReaderT $ \_ -> do
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
    MkWebApiAction (SetContext c) -> Right <$> (liftIO $ setContext $ hashed c)
    MkWebApiAction (ClearContext pc) -> Right <$> (liftIO $ clearContext pc)
    MkWebApiAction (ModelOnly {}) -> pure (Right ())
    MkWebApiAction (FreshValue l) -> ReaderT $ \_ -> Right <$> freshToken ("fresh:" <> l)
    MkWebApiAction (EffectCall _ _ eff) -> ReaderT $ \_ ->
      either (Left . ResultError . MkResultError) Right <$> liftIO (eff lkp)
    MkWebApiAction (XAction xact) -> ReaderT $ \_ -> do
      res <- liftIO $ perform xActionState xact lkp
      pure $ first (XActionError . AnyXActionError) res

  postcondition ss (MkWebApiAction act) lkp res = case act of
    SuccessCall creq model _ _ -> check creq model res
    ErrorCall creq _ model _ -> check creq model res
    _ -> pure True
    where
      -- the request as it went out: the step's fresh tokens are still those
      check :: forall meth r x. SingMethod meth => ClientRequestVal meth r -> SuccessApiModel s c xstate apps meth r x -> x -> PostconditionM (ReaderT WebApiSessionsCxt (WebApiSessions apps)) Bool
      check creq SuccessApiModel {apiPostcondition} x = do
        toks <- lift (lift freshTokens)
        -- the files were written for the call; the view names them
        case resolveRequest placeholders {freshOf = freshLookup toks} lkp creq >>= \req -> apiPostcondition ss lkp req x of
          Right () -> pure True
          Left why -> do
            counterexamplePost ("  postcondition failed: " ++ T.unpack why)
            pure False

  postconditionOnFailure ss (MkWebApiAction act) lkp res = pure $ case act of
    SuccessCall _ SuccessApiModel {apiPostconditionOnFailure} _ _ -> apiPostconditionOnFailure ss lkp res
    ErrorCall _ _ SuccessApiModel {apiPostconditionOnFailure} _ -> apiPostconditionOnFailure ss lkp res
    _ -> True

  monitoring _ (MkWebApiAction act) lkp res = QC.counterexample $ case act of
    SuccessCall creq _ _ _ -> stepReport (show act) (requestReport lkp creq) (outcome res)
    ErrorCall creq _ _ _ -> stepReport (show act) (requestReport lkp creq) (outcome res)
    FreshValue {} -> stepReport (show act) [] (outcome res)
    _ -> show act
    where
      outcome :: Show x => Either ErrorState x -> String
      outcome = either (("error: " ++) . show) (("result: " ++) . show)

  monitoringFailure _ (MkWebApiAction act) lkp err = QC.counterexample $ case act of
    SuccessCall creq _ _ _ -> stepReport (show act) (requestReport lkp creq) ("error: " ++ show err)
    ErrorCall creq _ _ _ -> stepReport (show act) (requestReport lkp creq) ("error: " ++ show err)
    _ -> show act ++ "\n  error: " ++ show err

-- | A step of the counterexample: the operation (and the rule it came
-- from), the request as it went out, what came back.
stepReport :: String -> [String] -> String -> String
stepReport heading requestLines result = intercalate "\n" $ heading : map ("  " ++) (requestLines ++ [result])

-- | The request as the client sends it (method, path, query, form, body),
-- once its variables are looked up; or the field nobody supplied.
requestReport :: forall meth r.
  ( ToParam 'PathParam (PathParam meth r)
  , ToParam 'QueryParam (QueryParam meth r)
  , ToParam 'FormParam (FormParam meth r)
  , ToParam 'FileParam (FileParam meth r)
  , ToHeader (HeaderIn meth r)
  , PartEncodings (RequestBody meth r)
  , ToHListRecTuple (StripContents (RequestBody meth r))
  , MkPathFormatString r
  , SingMethod meth
  ) => LookUp -> ClientRequestVal meth r -> [String]
requestReport lkp creq = case resolveRequest placeholders lkp creq of
  Left field -> ["request: <" ++ T.unpack field ++ " is not set>"]
  Right resolved ->
    let
      RequestParts {uriPath, meth, qitms, hdrs, formPar, filePar, bodyPart} = requestParts BC.empty [] (fromClientRequest resolved)
      requestLine = "request: " ++ BC.unpack meth ++ " " ++ BC.unpack uriPath ++ BC.unpack (H.renderQuery True qitms)
      headerLines = [ "header: " ++ BC.unpack (CI.original k) ++ ": " ++ masked (CI.original k) (BC.unpack v) | (k, v) <- hdrs, k /= H.hAccept ]
      formLines = [ "form: " ++ intercalate "&" [ BC.unpack k ++ "=" ++ masked k (BC.unpack v) | (k, v) <- formPar ] | not (null formPar) ]
      fileLines = [ "file " ++ BC.unpack k ++ ": " ++ BC.unpack fileName ++ " (" ++ BC.unpack fileContentType ++ ")"
                  | (k, FileInfo {fileName, fileContentType}) <- filePar ]
      bodyLines = [ "body (" ++ BC.unpack ct ++ "): " ++ clip (LBC.unpack body) | Just (ct, body) <- [bodyPart] ]
    in requestLine : headerLines ++ formLines ++ fileLines ++ bodyLines
  where
    -- credentials do not belong in a test report
    masked k v
      | any (`isInfixOf` map toLower (BC.unpack k)) ["password", "authorization", "cookie", "token"] = "<hidden>"
      | otherwise = v
    clip s = case splitAt 2000 s of
      (pre, []) -> pre
      (pre, _) -> pre ++ "…"

instance ( Reifies s (WebApiGlobalStateModel c xstate apps)
         , DynLogicModel xstate
         ) => DynLogicModel (ApiState s c xstate apps) where
  restricted _ = False

successCall :: forall meth r app c xstate apps s. (WebApiActionCxt apps meth app r, Show (ApiOut meth (app :// r)), Eq (Action xstate (ApiOut meth (app :// r)))) =>
  ClientRequestVal meth (app :// r)
  -> Action (ApiState s c xstate apps) (ApiOut meth (app :// r))
successCall creq = mkWebApiAction $ SuccessCall creq defSuccessApiModel Nothing (Right . getSuccessOut)

successCallWith :: forall meth r app res c xstate apps s. (Typeable res, Show res, Eq (Action xstate res), WebApiActionCxt apps meth app r) =>
  ClientRequestVal meth (app :// r)
  -> SuccessApiModel s c xstate apps meth (app :// r) res
  -> Maybe (ApiSuccess meth (app :// r) -> ModifyClientCookies app)
  -> (ApiSuccess meth (app :// r) -> Either ResultError res)
  -> Action (ApiState s c xstate apps) res
successCallWith creq apiModel cookModMay f = mkWebApiAction (SuccessCall creq apiModel cookModMay f)

-- | A call expected to fail; the result is the decoded error.
failingCall :: forall meth r app c xstate apps s. (WebApiActionCxt apps meth app r, Show (ApiErr meth (app :// r)), Eq (Action xstate (ApiErr meth (app :// r)))) =>
  ClientRequestVal meth (app :// r)
  -> ExpectedFailure
  -> Action (ApiState s c xstate apps) (ApiErr meth (app :// r))
failingCall creq expected = mkWebApiAction $ ErrorCall creq expected defSuccessApiModel (\ApiError {err} -> Right err)

failingCallWith :: forall meth r app res c xstate apps s. (Typeable res, Show res, Eq (Action xstate res), WebApiActionCxt apps meth app r) =>
  ClientRequestVal meth (app :// r)
  -> ExpectedFailure
  -> SuccessApiModel s c xstate apps meth (app :// r) res
  -> (ApiError meth (app :// r) -> Either ResultError res)
  -> Action (ApiState s c xstate apps) res
failingCallWith creq expected apiModel f = mkWebApiAction (ErrorCall creq expected apiModel f)

-- | A successful call as a negative scenario: the same request, expected
-- to fail. The model's precondition does not apply (a negative scenario
-- is by nature outside it) — the given one does; the step's result is
-- unit; its next state is the model's 'apiFailureNextState' (identity by
-- default); the label stays. 'Nothing' for anything but a 'SuccessCall'.
expectingFailure :: Eq (Action xstate ()) =>
  ExpectedFailure
  -> (ApiState s c xstate apps -> Bool)
  -> Action (ApiState s c xstate apps) a
  -> Maybe (Action (ApiState s c xstate apps) ())
expectingFailure expected pre (MkWebApiAction act) = case act of
  SuccessCall creq SuccessApiModel {apiFailureNextState, label, apiVariables} _ _ -> Just $ mkWebApiAction $ ErrorCall creq expected
    SuccessApiModel
      { apiNextState = (\ns _ st -> ns st) <$> apiFailureNextState
      , apiFailureNextState
      , apiPrecondition = Just pre
      , apiValidFailingAction = Nothing
      , apiShrinkAction = Nothing
      , apiPostcondition = \_ _ _ _ -> Right ()
      , apiPostconditionOnFailure = \_ _ _ -> True
      , label
      , apiVariables
      , apiEnsures = mempty
      , apiRefutes = mempty
      , apiRecords = mempty
      , apiRetry = Nothing
      }
    (const (Right ()))
  _ -> Nothing


-- | What whoever runs an action adds to it: request overrides (a script's),
-- model additions (what to record, what to require, what to assert).
data ActionConfig m s c xstate apps meth route a = ActionConfig
  { requestMod :: ClientRequestVal meth route -> m (ClientRequestVal meth route)
  , modelMod :: SuccessApiModel s c xstate apps meth route a -> SuccessApiModel s c xstate apps meth route a
  }

defaultActionConfig :: Applicative m => ActionConfig m s c xstate apps meth route a
defaultActionConfig = ActionConfig
  { requestMod = pure
  , modelMod = id
  }

-- | An action a model exports: how it builds its step from a
-- configuration; the fillers for the request fields it leaves 'Unset'
-- ('fills') — applied after the configuration's overrides, so a script's
-- value wins and a generated step gets the filler's; the refinements of
-- its values it declares ('declares'), and those every run must satisfy
-- ('withContract').
data ApiAction c xstate apps meth route a = ApiAction
  { buildAction :: forall s m. HasApiStateM m s c xstate apps => ActionConfig m s c xstate apps meth route a -> m (Action (ApiState s c xstate apps) a)
  , requestFillers :: [RequestFiller c xstate apps meth route]
  , actionRefinements :: [Refinement meth route a]
  , contracts :: [RefinementId]
  , refutations :: [RefinementId]
    -- ^ declared refinements every run's value must be outside of
  , requestClasses :: [FieldClass]
    -- ^ request fields that name an entity of a class (see 'classAt')
  , resultClasses :: [ResultClass c xstate apps a]
    -- ^ result fields recorded under a class on every success (see 'classOf')
  }

-- | A request field that names an entity of a class.
data FieldClass = FieldClass
  { fcPart :: Text
  , fcField :: Text
  , fcClass :: RefinementId
  }

-- | A result field recorded under a class, with how (the typed access is
-- captured where the field is named).
data ResultClass c xstate apps a = ResultClass
  { rcField :: Text
  , rcClass :: RefinementId
  , rcRecord :: forall s. Var a -> ApiState s c xstate apps -> ApiState s c xstate apps
  }

partName :: Part meth route t -> Text
partName = \case
  QueryP -> "query"
  FormP -> "form"
  HeaderP -> "header"
  PathP -> "path"
  BodyP -> "body"
  FileP -> "file"

-- | The request field names an entity of the class: when nobody supplies
-- it, one recorded under the class is drawn (by position), and the step
-- is enabled only while there is one; what a script supplies is checked
-- against the class where the script is checked.
classAt :: forall name x t c xstate apps meth route a. (KnownSymbol name, Typeable x, Generic t, GValRep (Rep t), GGetField name x (Rep t), GSetField name x (Rep t))
  => Part meth route t -> RefinementId -> ApiAction c xstate apps meth route a -> ApiAction c xstate apps meth route a
classAt part rid act = case fills @name part (entityFiller rid) act of
  ApiAction {requestClasses, ..} -> ApiAction {requestClasses = FieldClass (partName part) (T.pack (symbolVal (Proxy @name))) rid : requestClasses, ..}

-- | A whole part of the request (a bare path parameter, say) names an
-- entity of the class; as 'classAt' otherwise.
classAtPart :: forall t c xstate apps meth route a. Typeable t
  => Part meth route t -> RefinementId -> ApiAction c xstate apps meth route a -> ApiAction c xstate apps meth route a
classAtPart part rid act = case fillsPart part (entityFiller rid) act of
  ApiAction {requestClasses, ..} -> ApiAction {requestClasses = FieldClass (partName part) "" rid : requestClasses, ..}

-- | Fill a whole part when nobody supplied it ('Unset'); a supplied one stays.
fillsPart :: forall t c xstate apps meth route a. Part meth route t -> Filler c xstate apps t -> ApiAction c xstate apps meth route a -> ApiAction c xstate apps meth route a
fillsPart part (Filler make) ApiAction {requestFillers, ..} = ApiAction
  { requestFillers = requestFillers ++ [RequestFiller (overPart part (\v -> case v of Unset _ -> make; _ -> pure v))]
  , ..
  }

-- | The result field is an entity of the class: every success records it
-- (a declared refinement still has to be ensured for the record to be
-- sound — 'runApiAction' refuses it otherwise).
classOf :: forall name x c xstate apps meth route a. (KnownSymbol name, Typeable x, Show x, Eq x, Typeable a, Generic a, GValRep (Rep a), GGetField name x (Rep a))
  => RefinementId -> ApiAction c xstate apps meth route a -> ApiAction c xstate apps meth route a
classOf rid@(RefinementId klass) ApiAction {resultClasses, ..} = ApiAction
  { resultClasses = ResultClass
      { rcField = T.pack (symbolVal (Proxy @name))
      , rcClass = rid
      , rcRecord = \var -> addTypedEntity (getField @name (fromVar var)) NER {refinementId = rid, entityName = fromString (T.unpack klass), desc = Nothing}
      } : resultClasses
  , ..
  }

newtype RequestFiller c xstate apps meth route = RequestFiller (forall s m. HasApiStateM m s c xstate apps => ClientRequestVal meth route -> m (ClientRequestVal meth route))

mkApiAction :: (forall s m. HasApiStateM m s c xstate apps => ActionConfig m s c xstate apps meth route a -> m (Action (ApiState s c xstate apps) a)) -> ApiAction c xstate apps meth route a
mkApiAction f = ApiAction { buildAction = f, requestFillers = [], actionRefinements = [], contracts = [], refutations = [], requestClasses = [], resultClasses = [] }

-- | A class of an action's values decided by a predicate — a named,
-- stackable postcondition: a value is in it when the predicate holds and
-- it is in every base. Values are symbolic until the step runs, so a
-- refinement is decided *then*: as a check the step ensures
-- ('ensuresRefinement') or the action promises ('withContract'); a value
-- may be recorded under it only when so ensured ('recordsAs'), which is
-- what makes drawing from the class afterwards sound.
data Refinement meth r a = Refinement
  { refinementName :: RefinementId
  , refines :: [Refinement meth r a]
  , holds :: LookUp -> ClientRequest meth r -> a -> Either Text ()
  }

-- | The refinement and its bases, transitively (itself first).
refinementNames :: Refinement meth r a -> [RefinementId]
refinementNames Refinement {refinementName, refines} = refinementName : concatMap refinementNames refines

-- | Check the step's value against the refinement and its bases; a
-- failure names the one that does not hold.
ensuresRefinement :: Refinement meth r a -> SuccessApiModel s c xstate apps meth r a -> SuccessApiModel s c xstate apps meth r a
ensuresRefinement r = noteEnsured . foldr ((.) . check) id (chain r)
  where
    chain x@Refinement {refines} = x : concatMap chain refines
    check Refinement {refinementName = RefinementId n, holds} =
      andPostcondition (\_ lkp req x -> either (\why -> Left ("refinement " <> n <> ": " <> why)) Right (holds lkp req x))
    noteEnsured SuccessApiModel {apiEnsures, ..} = SuccessApiModel {apiEnsures = apiEnsures <> Set.fromList (refinementNames r), ..}

-- | Check the step's value is *not* in the refinement: outside a class
-- means its predicate fails or one of its bases' does (the class is their
-- conjunction). Refuting never makes a class drawable.
refutesRefinement :: forall meth r a s c xstate apps. Refinement meth r a -> SuccessApiModel s c xstate apps meth r a -> SuccessApiModel s c xstate apps meth r a
refutesRefinement r@Refinement {refinementName = rid@(RefinementId n)} = noteRefuted . andPostcondition (\_ lkp req x -> check lkp req x)
  where
    check :: LookUp -> ClientRequest meth r -> a -> Either Text ()
    check lkp req x = case [ () | Refinement {holds} <- chain r, Left _ <- [holds lkp req x] ] of
      [] -> Left ("refinement " <> n <> " holds, which the step refutes")
      _ -> Right ()
    chain :: Refinement meth r a -> [Refinement meth r a]
    chain x@Refinement {refines} = x : concatMap chain refines
    noteRefuted :: SuccessApiModel s c xstate apps meth r a -> SuccessApiModel s c xstate apps meth r a
    noteRefuted SuccessApiModel {apiRefutes, ..} = SuccessApiModel {apiRefutes = Set.insert rid apiRefutes, ..}

-- | Note that the step records its value under the class (the recording
-- itself is the model's next-state); 'runApiAction' refuses a declared
-- refinement the step does not ensure.
recordsAs :: RefinementId -> SuccessApiModel s c xstate apps meth r a -> SuccessApiModel s c xstate apps meth r a
recordsAs rid SuccessApiModel {apiRecords, ..} = SuccessApiModel {apiRecords = Set.insert rid apiRecords, ..}

-- | Declare a refinement of the action's values.
declares :: Refinement meth route a -> ApiAction c xstate apps meth route a -> ApiAction c xstate apps meth route a
declares r ApiAction {actionRefinements, ..} = ApiAction {actionRefinements = actionRefinements ++ [r], ..}

-- | Promise a declared refinement on every run of the action — a script's
-- step or a generated one.
withContract :: RefinementId -> ApiAction c xstate apps meth route a -> ApiAction c xstate apps meth route a
withContract rid act@ApiAction {contracts, ..} = case lookupRefinement rid act of
  Just _ -> ApiAction {contracts = contracts ++ [rid], ..}
  Nothing -> error $ "withContract " ++ show rid ++ ": not a refinement the action declares"

-- | Promise that no run's value is in a declared refinement.
withRefutation :: RefinementId -> ApiAction c xstate apps meth route a -> ApiAction c xstate apps meth route a
withRefutation rid act@ApiAction {refutations, ..} = case lookupRefinement rid act of
  Just _ -> ApiAction {refutations = refutations ++ [rid], ..}
  Nothing -> error $ "withRefutation " ++ show rid ++ ": not a refinement the action declares"

lookupRefinement :: RefinementId -> ApiAction c xstate apps meth route a -> Maybe (Refinement meth route a)
lookupRefinement rid ApiAction {actionRefinements} = case [ r | r@Refinement {refinementName} <- actionRefinements, refinementName == rid ] of
  r : _ -> Just r
  [] -> Nothing

-- | The action's step: the configuration's overrides, then the fillers;
-- the contracts on top of the configuration's model; and the rule that
-- a declared refinement is recorded only when ensured.
runApiAction :: HasApiStateM m s c xstate apps => ApiAction c xstate apps meth route a -> ActionConfig m s c xstate apps meth route a -> m (Action (ApiState s c xstate apps) a)
runApiAction apiAct@ApiAction {buildAction, requestFillers, actionRefinements, contracts, refutations, resultClasses} ActionConfig {requestMod, modelMod} = do
  act0 <- buildAction ActionConfig
    { requestMod = \creq -> requestMod creq >>= \creq' -> foldM (\r (RequestFiller fill) -> fill r) creq' requestFillers
    , modelMod = \model ->
        foldr refutesRefinement
          (foldr ensuresRefinement (recordClasses resultClasses (modelMod model)) [ r | rid <- contracts, Just r <- [lookupRefinement rid apiAct] ])
          [ r | rid <- refutations, Just r <- [lookupRefinement rid apiAct] ]
    }
  let
    -- a request nobody completed (a class with no entity yet, a field
    -- nothing fills) is not a step the model can take
    act = case unWebApiAction act0 of
      SuccessCall creq model cook f | not (null (requestUnset creq)) -> mkWebApiAction (SuccessCall creq (andPrecondition (const False) model) cook f)
      ErrorCall creq ef model f | not (null (requestUnset creq)) -> mkWebApiAction (ErrorCall creq ef (andPrecondition (const False) model) f)
      _ -> act0
    declared = Set.fromList (map (\Refinement {refinementName} -> refinementName) actionRefinements)
    unsound :: forall s' c' xstate' apps' meth' r' a'. SuccessApiModel s' c' xstate' apps' meth' r' a' -> [RefinementId]
    unsound SuccessApiModel {apiEnsures, apiRecords} = Set.toList ((apiRecords `Set.intersection` declared) `Set.difference` apiEnsures)
  case unWebApiAction act of
    SuccessCall _ model _ _ | rid : _ <- unsound model ->
      error $ "the step records its value as " ++ show rid ++ ", a refinement it does not ensure"
    _ -> pure act

-- | Record the result fields 'classOf' named, on every success.
recordClasses :: [ResultClass c xstate apps a] -> SuccessApiModel s c xstate apps meth r a -> SuccessApiModel s c xstate apps meth r a
recordClasses rcs model = foldr (\ResultClass {rcClass, rcRecord} -> recordsAs rcClass . andNextState rcRecord) model rcs

-- | Move the model further on a success, after what it already does.
andNextState :: (Var a -> ApiState s c xstate apps -> ApiState s c xstate apps) -> SuccessApiModel s c xstate apps meth r a -> SuccessApiModel s c xstate apps meth r a
andNextState f SuccessApiModel {apiNextState, ..} = SuccessApiModel {apiNextState = Just (\var st -> f var (maybe st (\g -> g var st) apiNextState)), ..}

-- | How a field nobody supplied gets its value.
newtype Filler c xstate apps x = Filler (forall s m. HasApiStateM m s c xstate apps => m (Val x))

-- | A fresh token (see 'freshM'), and what to make of it.
freshFiller :: Text -> (Text -> x) -> Filler c xstate apps x
freshFiller l f = Filler (fmap f <$> freshM l)

constFiller :: x -> Filler c xstate apps x
constFiller x = Filler (pure (Const x))

-- | One of the entities recorded under the class, by position (shrinking
-- toward the earliest); 'Unset' when there is none.
entityFiller :: forall x c xstate apps. Typeable x => RefinementId -> Filler c xstate apps x
entityFiller rid@(RefinementId klass) = Filler $ drawEntity rid >>= \case
  Nothing -> pure (Unset ("an entity of class " <> klass))
  Just (SomeVal v) -> pure $ fromMaybe (error $ T.unpack $ "an entity of another type is recorded under class " <> klass) (gcast v)

-- | One of the entities recorded under the class, by position (a
-- quantified choice as a DL step, a draw when generating; shrinking
-- toward the earliest); 'Nothing' when there is none.
drawEntity :: HasApiStateM m s c xstate apps => RefinementId -> m (Maybe AnyVal)
drawEntity rid = do
  st <- getApiStateM
  case getNamedEntitiesAny rid st of
    [] -> pure Nothing
    vs -> Just <$> elementsM vs

-- | Generation among the model's exported actions (by the name
-- 'generateFrom' scopes with): one whose precondition holds in the
-- state, with its model-default request; when none is enabled any scoped
-- one is returned and the DL discards it.
generateAmong :: StateModel xstate
  => [(Text, ApiState s c xstate apps -> QC.Gen (Any (Action (ApiState s c xstate apps))))]
  -> ApiState s c xstate apps
  -> QC.Gen (Any (Action (ApiState s c xstate apps)))
generateAmong actions st@ApiState {generateFrom} = do
  let scoped = [ gen | (name, gen) <- actions, maybe True (Set.member name) generateFrom ]
  candidates <- mapM ($ st) scoped
  let enabled = [ a | a@(Some act) <- candidates, webApiPrecondition st (unWebApiAction act) ]
  case (enabled, candidates) of
    (_ : _, _) -> QC.elements enabled
    ([], _ : _) -> QC.elements candidates
    ([], []) -> error "generateAmong: no action to generate from"

-- | A part of a request, to name the field a filler fills.
data Part meth route t where
  QueryP :: Part meth route (QueryParam meth route)
  FormP :: Part meth route (FormParam meth route)
  HeaderP :: Part meth route (HeaderIn meth route)
  PathP :: Part meth route (PathParam meth route)
  BodyP :: Part meth route (HListToTuple (StripContents (RequestBody meth route)))
  FileP :: Part meth route (FileParam meth route)

overPart :: Functor f => Part meth route t -> (Val t -> f (Val t)) -> ClientRequestVal meth route -> f (ClientRequestVal meth route)
overPart part f ClientRequestVal {..} = case part of
  QueryP -> (\v -> ClientRequestVal {query = v, ..}) <$> f query
  FormP -> (\v -> ClientRequestVal {form = v, ..}) <$> f form
  HeaderP -> (\v -> ClientRequestVal {header = v, ..}) <$> f header
  PathP -> (\v -> ClientRequestVal {path = v, ..}) <$> f path
  BodyP -> (\v -> ClientRequestVal {body = v, ..}) <$> f body
  FileP -> (\v -> ClientRequestVal {file = v, ..}) <$> f file

-- | Declare a filler for a field of a request part: what the field gets
-- when neither the action's default nor a script supplied it.
fills :: forall name x t c xstate apps meth route a. (Generic t, GValRep (Rep t), GGetField name x (Rep t), GSetField name x (Rep t))
  => Part meth route t -> Filler c xstate apps x -> ApiAction c xstate apps meth route a -> ApiAction c xstate apps meth route a
fills part (Filler make) ApiAction {requestFillers, ..} = ApiAction
  { requestFillers = requestFillers ++ [RequestFiller (overPart part (fillField @name make))]
  , ..
  }

data ActionConfigWith outcome s c xstate apps meth route a = ActionConfigWith
  { requestMod :: ClientRequestVal meth route -> ClientRequestVal meth route
  , apiModel :: Maybe (SuccessApiModel s c xstate apps meth route a)
  , resultMod :: outcome meth route -> Either ResultError a
  }

newtype ApiActionWith out c xstate apps meth route a = ApiActionWith (forall s m. (HasApiStateM m s c xstate apps) => ActionConfigWith out s c xstate apps meth route a -> m (Action (ApiState s c xstate apps) a))

-- | The monad determines the state it carries (DL over an ApiState, or
-- ApiGenM), so actions can read the state without naming its indices.
class Monad m => HasApiStateM m s c xstate apps | m -> s c xstate apps where
  getApiStateM :: m (ApiState s c xstate apps)
  -- | A generated value (with its shrinks): a quantified variable of the DL
  -- program, or a draw when the action itself is being generated.
  genValM :: (Typeable a, Show a, Eq a) => QC.Gen a -> (a -> [a]) -> m a
  -- | An index below the count, shrinking toward 0 — and, as a quantified
  -- variable, valid only while it is below the count: a choice among
  -- state-dependent candidates replays as a *position* when the program
  -- around it is shrunk, instead of as a value that may no longer exist.
  genIndexM :: Int -> m Int
  -- | A fresh value: as a DL step, a 'FreshValue' whose result is the token
  -- (a variable — usable in requests and assertions alike, remade on every
  -- execution); when the action itself is being generated, a 'Fresh' leaf
  -- made when the step runs (there is no script to assert on it).
  freshM :: Text -> m (Val Text)

instance (StateModel xstate, Eq (Action xstate Text)) => HasApiStateM (DL (ApiState s c xstate apps)) s c xstate apps where
  getApiStateM = getModelStateDL
  genValM gen shr = forAllNonVariableQ (withGenQ gen (const True) shr)
  genIndexM n = forAllNonVariableQ (withGenQ (QC.choose (0, n - 1)) (\i -> i >= 0 && i < n) (\i -> [0 .. i - 1]))
  freshM l = Var id <$> action (mkWebApiAction (FreshValue l))

instance HasApiStateM (ApiGenM s c xstate apps) s c xstate apps where
  getApiStateM = ask
  genValM gen _ = ApiGenM (lift gen)
  genIndexM n = ApiGenM (lift (QC.choose (0, n - 1)))
  freshM l = pure (fresh l)

-- | One of the candidates — a quantified choice when the action is a DL
-- step, a draw when it is being generated — by position, shrinking toward
-- the first (a model listing its candidates oldest-first shrinks to the
-- oldest; see 'genIndexM').
elementsM :: HasApiStateM m s c xstate apps => [a] -> m a
elementsM [] = error "elementsM: nothing to choose from"
elementsM xs = (xs !!) <$> genIndexM (length xs)

newtype ApiGenM s c xstate apps a = ApiGenM (ReaderT (ApiState s c xstate apps) QC.Gen a)
  deriving newtype (Functor, Applicative, Monad, MonadReader (ApiState s c xstate apps))

runApiGenM :: ApiGenM s c xstate apps a -> ApiState s c xstate apps -> QC.Gen a
runApiGenM (ApiGenM m) st = runReaderT m st

apiAction :: forall s a c xstate meth route apps.
  ( Typeable a
  , StateModel xstate
  , Eq (Action xstate a)
  , Eq (Action xstate Text)
  ) => ActionConfig (DL (ApiState s c xstate apps)) s c xstate apps meth route a
  -> ApiAction c xstate apps meth route a
  -> DL (ApiState s c xstate apps) (Val a)
apiAction cfg apiAct = do
  res <- action =<< runApiAction apiAct cfg
  pure $ Var id res

apiAction_ :: forall s a c xstate meth route apps.
  ( Typeable a
  , StateModel xstate
  , Eq (Action xstate a)
  , Eq (Action xstate Text)
  ) => ApiAction c xstate apps meth route a
  -> DL (ApiState s c xstate apps) (Val a)
apiAction_ act = apiAction defaultActionConfig act

setContextDL :: (StateModel xstate, Eq (Action xstate ()), ContextSwitch c) => c -> DL (ApiState s c xstate apps) ()
setContextDL c = () <$ action (MkWebApiAction $ SetContext c)

clearContextDL :: forall c xstate apps s. (StateModel xstate, Eq (Action xstate ()), ContextSwitch c) => DL (ApiState s c xstate apps) ()
clearContextDL = () <$ action (MkWebApiAction $ ClearContext (Proxy @c))

modelOnlyDL :: (StateModel xstate, Eq (Action xstate ())) => String -> (ApiState s c xstate apps -> ApiState s c xstate apps) -> DL (ApiState s c xstate apps) ()
modelOnlyDL n f = () <$ action (MkWebApiAction $ ModelOnly n f)

-- | Scope generation ('anyAction', 'anyActions') to the named actions;
-- 'Nothing' lifts the scope.
setGenerateFromDL :: (StateModel xstate, Eq (Action xstate ())) => Maybe (Set.Set Text) -> DL (ApiState s c xstate apps) ()
setGenerateFromDL names = modelOnlyDL ("generate from " ++ maybe "*" (show . Set.toList) names) $ \ApiState {..} -> ApiState {generateFrom = names, ..}

-- | The model's precondition of an action (what 'precondition' answers),
-- available without the reflected model.
webApiPrecondition :: StateModel xstate => ApiState s c xstate apps -> WebApiAction s c xstate apps a -> Bool
webApiPrecondition s@ApiState{xActionState} = \case
  SuccessCall _creq SuccessApiModel {apiPrecondition=pcMay} _ _ -> maybe True (\pc -> pc s) pcMay
  ErrorCall _creq _ SuccessApiModel {apiPrecondition=pcMay} _ -> maybe True (\pc -> pc s) pcMay
  SomeExceptionCall {} -> True
  SetContext {} -> True
  ClearContext {} -> True
  ModelOnly {} -> True
  FreshValue {} -> True
  EffectCall {} -> True
  XAction xact -> precondition xActionState xact

-- | Run another next-state transition after the model's own.
addNextState :: (Var a -> ApiState s c xstate apps -> ApiState s c xstate apps) -> SuccessApiModel s c xstate apps meth r a -> SuccessApiModel s c xstate apps meth r a
addNextState f SuccessApiModel {apiNextState, ..} = SuccessApiModel {apiNextState = Just $ \var st -> f var (maybe st (\ns -> ns var st) apiNextState), ..}

-- | Require another condition besides the model's own.
andPrecondition :: (ApiState s c xstate apps -> Bool) -> SuccessApiModel s c xstate apps meth r a -> SuccessApiModel s c xstate apps meth r a
andPrecondition p SuccessApiModel {apiPrecondition, ..} = SuccessApiModel {apiPrecondition = Just $ \st -> p st && maybe True ($ st) apiPrecondition, ..}
  
type EntityName = Text

-- | A class an entity is recorded under: its name, and what it means.
data NER = NER
  { refinementId :: RefinementId
  , entityName :: EntityName
  , desc :: Maybe Text
  }

data NamedVal t where
  NamedVal :: (Show t, Eq t) => {name :: RefinementId, val :: Val t} -> NamedVal t

instance Show (NamedVal t) where
  show NamedVal {name, val} = show name ++ case val of
    Const v -> show v
    Var _ v -> show v
    Pair _ _ -> "<pair>"
    Map _ _ -> "<map>"
    Fields _ -> "<fields>"
    Unset n -> "<unset " ++ T.unpack n ++ ">"
    Fresh l _ -> "<fresh " ++ T.unpack l ++ ">"
    TempFile n _ _ -> "<file " ++ T.unpack n ++ ">"

showAnyNamedVal :: Any NamedVal -> String
showAnyNamedVal (Some nv) = show nv

-- hkToDict :: Record.HK f x -> Record.HK (Dict c) x
-- hkToDict = undefined

-- data Dict (c :: Type -> Constraint) (t :: Type) where
--   Dict :: c t => Dict c t

instance Eq (NamedVal t) where
  NamedVal {name = n1, val = v1} == NamedVal {name = n2, val = v2}
    | n1 == n2 = case (v1, v2) of
        (Const c1, Const c2) -> c1 == c2
        (Var _ var1, Var _ var2') -> maybe False (var1 ==) $ gcast var2'
        (Unset l1, Unset l2) -> l1 == l2
        _ -> False
    | otherwise = False

addTypedEntity :: forall t c xstate apps s.
  ( Typeable t
  , Show t
  , Eq t
  ) => Val t
  -> NER
  -> ApiState s c xstate apps
  -> ApiState s c xstate apps
addTypedEntity val NER {refinementId} ApiState {namedEntityTyped, classTypes, ..} =
  let
    ty = typeRep (Proxy @t)
    newNET = NamedEntityTyped NamedEntity { namedEntity = M.singleton ty [Some NamedVal {name = refinementId, val}] }
  in case M.lookup refinementId classTypes of
    Just ty' | ty' /= ty -> error $ "an entity of type " ++ show ty ++ " recorded under class " ++ show refinementId ++ ", which holds " ++ show ty'
    _ -> ApiState {namedEntityTyped = namedEntityTyped <> newNET, classTypes, ..}

-- | Every entity of type @t@ the model has recorded, newest last.
getTypedEntities :: forall t c xstate apps s. Typeable t => ApiState s c xstate apps -> [Val t]
getTypedEntities ApiState {namedEntityTyped = NamedEntityTyped NamedEntity {namedEntity}} =
  [ v | Some (NamedVal {val = v'}) <- M.findWithDefault [] (typeRep (Proxy @t)) namedEntity, Just v <- [gcast v'] ]

-- | The entities of type @t@ recorded under a given name.
getNamedEntities :: forall t c xstate apps s. Typeable t => RefinementId -> ApiState s c xstate apps -> [Val t]
getNamedEntities rid ApiState {namedEntityTyped = NamedEntityTyped NamedEntity {namedEntity}} =
  [ v | Some (NamedVal {name, val = v'}) <- M.findWithDefault [] (typeRep (Proxy @t)) namedEntity, name == rid, Just v <- [gcast v'] ]

-- | Every entity recorded under the name, whatever its type, oldest first.
getNamedEntitiesAny :: RefinementId -> ApiState s c xstate apps -> [AnyVal]
getNamedEntitiesAny rid ApiState {namedEntityTyped = NamedEntityTyped NamedEntity {namedEntity}} =
  [ SomeVal v | vs <- M.elems namedEntity, Some (NamedVal {name, val = v}) <- vs, name == rid ]

-- | Whether any entity (of any type) is recorded under the name.
hasNamedEntity :: RefinementId -> ApiState s c xstate apps -> Bool
hasNamedEntity rid ApiState {namedEntityTyped = NamedEntityTyped NamedEntity {namedEntity}} =
  or [ name == rid | vs <- M.elems namedEntity, Some (NamedVal {name}) <- vs ]

-- | Forget the entities recorded under the name that equal the value
-- (a variable is equal to itself; constants by 'Eq').
removeNamedEntity :: forall t c xstate apps s. (Typeable t, Show t, Eq t) => RefinementId -> Val t -> ApiState s c xstate apps -> ApiState s c xstate apps
removeNamedEntity rid val ApiState {namedEntityTyped = NamedEntityTyped ne@NamedEntity {namedEntity}, ..} =
  let
    gone = NamedVal {name = rid, val}
    keep (Some nv) = maybe True (/= gone) (gcast nv)
  in ApiState {namedEntityTyped = NamedEntityTyped ne {namedEntity = M.adjust (filter keep) (typeRep (Proxy @t)) namedEntity}, ..}

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

data NoXState = NoXState
  deriving (Show, Eq)

instance HasVariables NoXState where
  getAllVariables _ = mempty

instance Show (Action NoXState a) where
  show NoAction = "NoAction"

instance Eq (Action NoXState a) where
  _ == _ = False

instance HasVariables (Action NoXState a) where
  getAllVariables _ = mempty  
  
instance StateModel NoXState where
  data Action NoXState a where
    NoAction :: Action NoXState ()
  initialState = NoXState
  arbitraryAction _ _ = pure (Some NoAction)

instance RunModel NoXState IO where
  type Error NoXState IO = XActionError ()
  perform _ NoAction _ = pure $ Right ()

instance DynLogicModel NoXState where
  
