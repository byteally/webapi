{-# LANGUAGE FlexibleContexts                 #-}
{-# LANGUAGE DeriveGeneric                    #-}

module WebApi.OpenAPI.Utils where

import           Control.Monad.State
import qualified Data.HashMap.Strict.InsOrd as HMI
import           Data.Hashable
import           Data.Maybe
import           Data.OpenApi
import           Data.Text
import           GHC.Generics
import           Lens.Micro.Extras ( view )

getComponent ::
  ( MonadState s m
  , HasComponents s Components
  ) => (Components -> Definitions c) -> Referenced c -> m c
getComponent f r =
  case r of
    Ref ref    -> gets (go ref . f . view components)
    Inline par -> pure par

  where
    go ref =
      fromJust . HMI.lookup (getReference ref)

getParamComponent ::
  ( MonadState s m
  , HasComponents s Components
  ) => Referenced Param -> m Param
getParamComponent = getComponent _componentsParameters

getSchemaComponent ::
  ( MonadState s m
  , HasComponents s Components
  ) => Referenced Schema -> m Schema
getSchemaComponent = getComponent _componentsSchemas

getHeaderComponent ::
  ( MonadState s m
  , HasComponents s Components
  ) => Referenced Header -> m Header
getHeaderComponent = getComponent _componentsHeaders

getRequestBodyComponent ::
  ( MonadState s m
  , HasComponents s Components
  ) => Referenced RequestBody -> m RequestBody
getRequestBodyComponent = getComponent _componentsRequestBodies

getResponsesComponent ::
  ( MonadState s m
  , HasComponents s Components
  ) => Referenced Response -> m Response
getResponsesComponent = getComponent _componentsResponses

data HttpApiInstance =
    JSON
  | PlainText
  | XML
  | Param
  | Header -- Text  -- ^ Header name
  | Other Text
  deriving (Show, Eq, Generic)

instance Hashable HttpApiInstance
