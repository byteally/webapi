{-# LANGUAGE UndecidableInstances #-}
module Test.WebApi.StateModel
  ( WebApiAction (..)
  , ApiState (..)
  , WebApiActionCxt
  , mkWebApiAction
  , getOpIdFromRequest
  ) where

import Test.WebApi
import Test.QuickCheck.StateModel
import Test.QuickCheck.DynamicLogic (DynLogicModel (..))
import WebApi.Contract
import WebApi.Param
import WebApi.ContentTypes
import Control.Exception (SomeException)
import Data.Kind
import Data.Typeable
import Data.Coerce
import GHC.TypeLits

type WebApiActionCxt (apps :: [Type]) meth (app :: Type) r =
  ( ToParam 'PathParam (PathParam meth (app://r))
  , ToParam 'QueryParam (QueryParam meth (app://r))
  , FromHeader (HeaderOut meth (app :// r))
  , FromParam Cookie (CookieOut meth (app :// r))
  , Decodings (ContentTypes meth (app :// r)) (ApiOut meth (app :// r))
  , Decodings (ContentTypes meth (app :// r)) (ApiErr meth (app :// r))
  , SingMethod meth
  , WebApi app
  , Typeable app
  , Typeable (ApiOut meth (app :// r))
  , Typeable (ApiErr meth (app :// r))
  , Typeable r
  , AppIsElem app apps
  , KnownSymbol (GetOpIdName (OperationId meth (app :// r)))
  )

data WebApiAction (apps :: [Type]) (a :: Type) where
  SuccessCall :: WebApiActionCxt apps meth app r => ClientRequest meth (app :// r) -> WebApiAction apps (ApiOut meth (app :// r))
  ErrorCall :: WebApiActionCxt apps meth app r => ClientRequest meth (app :// r) -> WebApiAction apps (ApiErr meth (app :// r))
  SomeExceptionCall :: WebApiActionCxt apps meth app r => ClientRequest meth (app :// r) -> WebApiAction apps (SomeException)

instance Show (WebApiAction apps a) where
  show = \case
    SuccessCall creq -> show . toWaiRequest . fromClientRequest $ creq
    ErrorCall creq -> show . toWaiRequest . fromClientRequest $ creq
    SomeExceptionCall creq -> show . toWaiRequest . fromClientRequest $ creq

-- TODO: Revisit
instance Eq (WebApiAction apps a) where
  (==) (SuccessCall creq1) = \case
    SuccessCall creq2 -> (show . toWaiRequest . fromClientRequest $ creq1) == (show . toWaiRequest . fromClientRequest $ creq2)
    _ -> False
  (==) (ErrorCall creq1) = \case
    ErrorCall creq2 -> (show . toWaiRequest . fromClientRequest $ creq1) == (show . toWaiRequest . fromClientRequest $ creq2)
    _ -> False
  (==) (SomeExceptionCall creq1) = \case
    SomeExceptionCall creq2 -> (show . toWaiRequest . fromClientRequest $ creq1) == (show . toWaiRequest . fromClientRequest $ creq2)
    _ -> False    
    
instance HasVariables (WebApiAction apps a) where
  getAllVariables = mempty

data ApiState (apps :: [Type]) = ApiState
  deriving (Show, Eq)

instance HasVariables (ApiState apps) where
  getAllVariables = mempty

mkWebApiAction :: WebApiAction apps a -> Action (ApiState apps) a
mkWebApiAction = coerce

instance StateModel (ApiState apps) where
  newtype Action (ApiState apps) a = MkWebApiAction (WebApiAction apps a)
    deriving newtype (Show, Eq, HasVariables)

  actionName = \case
    MkWebApiAction (SuccessCall creq) -> getOpIdFromRequest creq
    MkWebApiAction (ErrorCall creq) -> getOpIdFromRequest creq
    MkWebApiAction (SomeExceptionCall creq) -> getOpIdFromRequest creq

  arbitraryAction = undefined
  initialState = undefined

instance RunModel (ApiState apps) (WebApiSessions apps) where
  perform _ act _ = case act of
    MkWebApiAction (SuccessCall creq) -> do
      testClients creq >>= \case
        Success _ out _ _ -> pure out
        _ -> error "Fail"
    MkWebApiAction (ErrorCall creq) -> do
      testClients creq >>= \case
        Failure (Left (ApiError _ err _ _)) -> pure err
        _ -> error "Fail"
    MkWebApiAction (SomeExceptionCall creq) -> do
      testClients creq >>= \case
        Failure (Right (OtherError e)) -> pure e
        _ -> error "Fail"

instance DynLogicModel (ApiState apps) where
  restricted _ = False

getOpIdFromRequest :: forall meth app r. (KnownSymbol (GetOpIdName (OperationId meth (app://r))), Typeable app, Typeable r) => ClientRequest meth (app://r) -> String
getOpIdFromRequest _ =
  let
    routeName = symbolVal (Proxy @(GetOpIdName (OperationId meth (app://r))))
    appName = show $ typeRep (Proxy @app)
  in appName ++ "/" ++ routeName

type family GetOpIdName (oid :: OpId) :: Symbol where
  GetOpIdName ('OpId _ n) = n
  GetOpIdName ('UndefinedOpId m r) = TypeError ('Text "OperationId is not set for " ':<>: 'ShowType m ':<>: 'Text " " ':<>: 'ShowType r)
