{-# OPTIONS_GHC -Wno-orphans               #-}
{-# LANGUAGE DuplicateRecordFields         #-}
{-# LANGUAGE DerivingStrategies            #-}
{-# LANGUAGE GeneralizedNewtypeDeriving    #-}
{-# LANGUAGE NamedFieldPuns                #-}
{-# LANGUAGE FlexibleContexts              #-}
{-# LANGUAGE StandaloneDeriving            #-}
{-# LANGUAGE DeriveGeneric                 #-}
{-# LANGUAGE MultiParamTypeClasses         #-}

module WebApi.OpenAPI.Analysis where

import           Data.Text ( Text )
import           Control.Monad.State
import           Data.Functor.Identity
import qualified Data.HashMap.Strict as HM
import qualified Data.HashMap.Strict.InsOrd as HMI
import qualified Data.HashSet as HS
import           Data.Hashable
import qualified Data.Map as M
import qualified Data.OpenApi as OA
import           Data.OpenApi hiding ( Schema )
import           Lens.Micro ( Lens', (^.), (&), (%~) )
import           Network.HTTP.Media
import           Network.HTTP.Types
import           WebApi.OpenAPI.Utils ( HttpApiInstance, getHeaderComponent, getSchemaComponent, getParamComponent, getRequestBodyComponent, getResponsesComponent )
import qualified WebApi.OpenAPI.Utils as WO
import qualified Debug.Trace as DT

type RefSchemaMap =
  HM.HashMap Reference (HS.HashSet HttpApiInstance)

deriving newtype instance Hashable Reference

data Schema =
  Schema { schema :: Referenced OA.Schema
         , instances :: HS.HashSet HttpApiInstance
         } deriving (Show, Eq)

data Route =
  Route { route :: FilePath
        , pathParams :: [ OA.Param ]
        -- ^ Contains only those params which have `ParamPath` as the paramLocation.
        --   All overriding that needs to be done is already performed by the time the route is constructed.
        } deriving (Show, Eq)

data Contract =
  Contract { route :: Route
           , method :: StdMethod
           , operation :: OA.Operation
           -- ^ Param overriding is already performed by the time the operation is constructed.
           } deriving (Show, Eq)

data OpenApiAnalysisInfo =
  OpenApiAnalysisInfo { _refSchema :: RefSchemaMap
                      , _contracts :: [ Contract ]
                      } deriving (Show, Eq)

refSchema :: Lens' OpenApiAnalysisInfo RefSchemaMap
refSchema f oai@OpenApiAnalysisInfo { _refSchema } =
  (\refSchs -> oai { _refSchema = refSchs }) <$> f _refSchema

contracts :: Lens' OpenApiAnalysisInfo [ Contract ]
contracts f oai@OpenApiAnalysisInfo { _contracts } =
  (\cts -> oai { _contracts = cts }) <$> f _contracts

defOpenApiAnalysisInfo :: OpenApiAnalysisInfo
defOpenApiAnalysisInfo = OpenApiAnalysisInfo { _refSchema = mempty, _contracts = mempty }

openApiState :: Components -> OpenApiState
openApiState comps = OpenApiState { _openApiAnalysisInfo = defOpenApiAnalysisInfo, _openApiComponentsInfo = comps }

data OpenApiState =
  OpenApiState { _openApiAnalysisInfo :: OpenApiAnalysisInfo
               , _openApiComponentsInfo :: Components
               } deriving (Show, Eq)

openApiAnalysisInfo :: Lens' OpenApiState OpenApiAnalysisInfo
openApiAnalysisInfo f oa@OpenApiState { _openApiAnalysisInfo } =
  (\oai -> oa { _openApiAnalysisInfo = oai }) <$> f _openApiAnalysisInfo

openApiComponentsInfo :: Lens' OpenApiState Components
openApiComponentsInfo f oa@OpenApiState { _openApiComponentsInfo } =
  (\comps -> oa { _openApiComponentsInfo = comps }) <$> f _openApiComponentsInfo

instance HasComponents OpenApiState Components where
  components = openApiComponentsInfo

newtype OpenApiM m a =
  OpenApiM { runOpenApi :: StateT OpenApiState m a }
  deriving newtype (Functor, Applicative, Monad, MonadState OpenApiState)

newContract ::
  ( MonadState OpenApiState m
  ) => Route -> StdMethod -> OA.Operation -> m ()
newContract route method op =
  modify (\s -> s & openApiAnalysisInfo . contracts %~ (contract :))

  where
    contract =
      Contract { route, method, operation = op }

-- | Add a schema and its descendents to the RefSchemaMap with the specified instance.
--   If schema already exists, then the instance set is extended.
newSchema ::
  ( MonadState OpenApiState m
  ) => HttpApiInstance -> Maybe (Referenced OA.Schema) -> m ()
newSchema inst v = do
  -- DT.traceM $ "Ref schema: " <> show v
  maybe (pure ()) (getSchemaDescendentsTagged inst >=> updateRefSchemaMap) v

newParamSchema ::
  ( MonadState OpenApiState m
  ) => Param -> m ()
newParamSchema Param { _paramSchema, _paramIn } =
  newSchema (paramApiInst _paramIn) _paramSchema

  where
    paramApiInst ParamQuery  = WO.Param
    paramApiInst ParamHeader = WO.Header
    paramApiInst ParamPath   = WO.Param
    paramApiInst ParamCookie = WO.Param

newRefRequestBody ::
  ( MonadState OpenApiState m
  ) => Referenced RequestBody -> m ()
newRefRequestBody =
  getRequestBodyComponent >=> newRequestBody

newRequestBody ::
  ( MonadState OpenApiState m
  ) => RequestBody -> m ()
newRequestBody RequestBody { _requestBodyContent } =
  HMI.foldlWithKey' go (pure ()) _requestBodyContent

  where
    go a k MediaTypeObject { _mediaTypeObjectSchema } =
      a >> newSchema (mediaTypeInst k) _mediaTypeObjectSchema

-- TODO: complete.
mediaTypeInst :: MediaType -> HttpApiInstance
mediaTypeInst _ = WO.JSON

newResponses ::
  ( MonadState OpenApiState m
  ) => Responses -> m ()
newResponses Responses { _responsesResponses } =
  HMI.foldl' go (pure ()) _responsesResponses
  where
    go a v =
      a >> (getResponsesComponent v >>= newResponse)

newResponse ::
  ( MonadState OpenApiState m
  ) => Response -> m ()
newResponse Response { _responseContent, _responseHeaders } = do
  HMI.foldlWithKey' respContent (pure ()) _responseContent
  HMI.foldlWithKey' respHeaders (pure ()) _responseHeaders

  where
    respContent a k MediaTypeObject { _mediaTypeObjectSchema } =
      a >> newSchema (mediaTypeInst k) _mediaTypeObjectSchema

    respHeaders a _k v = do
      Header { _headerSchema } <- getHeaderComponent v
      a >> newSchema WO.Header _headerSchema

updateRefSchemaMap ::
  ( MonadState OpenApiState m
  ) => HM.HashMap Reference (HS.HashSet HttpApiInstance) -> m ()
updateRefSchemaMap refSchs =
  modify (\s -> s & openApiAnalysisInfo . refSchema %~ HM.unionWith HS.union refSchs)

getSchemaDescendentsTagged ::
  ( MonadState OpenApiState m
  ) => HttpApiInstance -> Referenced OA.Schema -> m (HM.HashMap Reference (HS.HashSet HttpApiInstance))
getSchemaDescendentsTagged inst =
  fmap (HM.map (const (HS.singleton inst)) . HS.toMap) . getSchemaDescendents

getSchemaDescendents ::
  ( MonadState OpenApiState m
  ) => Referenced OA.Schema -> m (HS.HashSet Reference)
getSchemaDescendents = go mempty
  where
    go hsr (Inline sch) =
      go1 hsr sch
    go hsr (Ref ref)
      | HS.member ref hsr = pure hsr
      | otherwise         = do
          sch <- getSchemaComponent (Ref ref)
          go1 (HS.singleton ref <> hsr) sch

    go1 hsr OA.Schema { _schemaNot
                      , _schemaAllOf
                      , _schemaOneOf
                      , _schemaAnyOf
                      , _schemaProperties
                      , _schemaItems
                      } =
      let childs =
            maybe [] id _schemaAllOf <>
            maybe [] id _schemaOneOf <>
            maybe [] id _schemaAnyOf <>
            maybe [] pure _schemaNot <>
            maybe [] getItems _schemaItems <>
            HMI.elems _schemaProperties

      in foldM go hsr childs

      where
        getItems (OpenApiItemsObject sch) = pure sch
        getItems (OpenApiItemsArray schs) = schs

-- runOpenAPIAnalysis :: (MonadIO m) => OpenApi -> m OpenApiAnalysisInfo
runOpenAPIAnalysis :: OpenApi -> OpenApiAnalysisInfo
runOpenAPIAnalysis oa =
  runIdentity $
  fmap (flip (^.) openApiAnalysisInfo)
       (execStateT (runOpenApi (analyzeOpenAPI oa)) (openApiState (oa ^. components)))

analyzeOpenAPI :: (MonadState OpenApiState m) => OpenApi -> m ()
analyzeOpenAPI OpenApi { _openApiPaths } =
  HMI.foldlWithKey' go (pure ()) _openApiPaths

  where
    -- NOTE: If there exists a path param override at operation level
    --       we name the route as RouteNameMETHOD
    --       otherwise we name the route as RouteName
    go m routeRep PathItem { _pathItemGet, _pathItemPut, _pathItemPost
                           , _pathItemDelete, _pathItemOptions, _pathItemHead
                           , _pathItemPatch, _pathItemTrace, _pathItemParameters
                           } = m >> do
      routeParams <- mapM getParamComponent _pathItemParameters
      let routePathParams =
            filter (\Param { _paramIn } -> _paramIn == ParamPath) routeParams
          route =
            Route { route = routeRep
                  , pathParams = routePathParams
                  }

      mapM_ (uncurry (go0 routeParams route)) [ (GET, _pathItemGet)
                                              , (PUT, _pathItemPut)
                                              , (POST, _pathItemPost)
                                              , (DELETE, _pathItemDelete)
                                              , (OPTIONS, _pathItemOptions)
                                              , (HEAD, _pathItemHead)
                                              , (PATCH, _pathItemPatch)
                                              , (TRACE, _pathItemTrace)
                                              ]

      where
        go0 _ _ _  Nothing   =
          pure ()
        go0 routeParams route method (Just op) =
          go1 routeParams route method op

        go1 routeParams
            route@Route { pathParams = routePathParams }
            method
            op@Operation { _operationParameters, _operationRequestBody, _operationResponses } = do
          opParams <- mapM getParamComponent _operationParameters
          let opPathParams =
                filter (\Param { _paramIn } -> _paramIn == ParamPath) routeParams
              opRoute =
                Route { route = routeRep
                      , pathParams = opPathParams `overrideParams` routePathParams
                      }
              ovOpParams = opParams `overrideParams` routeParams
              opWithOverrides =
                op { _operationParameters = map Inline ovOpParams }

          mapM_ newParamSchema ovOpParams
          maybe (pure ()) newRefRequestBody  _operationRequestBody
          newResponses _operationResponses

          case opPathParams of
            [] -> newContract route method opWithOverrides
            _  -> newContract opRoute method opWithOverrides

overrideParams :: [ Param ] -> [ Param ] -> [ Param ]
overrideParams routeParams opParams =
  M.elems (go routeParams `M.union` go opParams)

  where
    go = M.fromList . foldr (\p@Param { _paramName } a -> ( _paramName, p) : a) []


