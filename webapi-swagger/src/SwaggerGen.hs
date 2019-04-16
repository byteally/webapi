{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE CPP                        #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE BangPatterns               #-}

module SwaggerGen where

import Control.Monad.Reader
import Control.Monad.State
import Data.Swagger hiding (get, paramSchema, Referenced (..))
import qualified Data.Swagger as SW
import GHC.Generics
import qualified Data.Text as T
import qualified Data.HashMap.Strict as HM
import qualified Data.HashMap.Strict.InsOrd as OHM
import qualified Data.List as L
import Data.Hashable
import qualified Data.Aeson as A
import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Char as M
import qualified Data.HashSet as HS
import qualified Data.Char as C
import Network.HTTP.Media.MediaType (MediaType)
import Data.Maybe 
-- import qualified Dhall as D

data Ref  = Inline Primitive
          | Ref    TypeMeta
          deriving (Show, Eq, Generic)

data ErrorMessage = ErrorMessage T.Text
                  deriving (Show, Eq, Generic)

data Method =
      GET
    | POST
    | HEAD
    | PUT
    | DELETE
    | TRACE
    | CONNECT
    | OPTIONS
    | PATCH
    deriving (Show, Eq, Generic, Ord)

data Config = Config { instanceTemplates :: [InstanceTemplate] }
            deriving (Generic)

type UnparsedPiece = T.Text

data Route a = Route { getRoute :: [RoutePiece a] }
           deriving (Show, Eq, Generic, Ord)

data RoutePiece a = Static T.Text | Dynamic a
                deriving (Show, Eq, Generic, Ord)

-- NOTE: "{foo}..{bar}" would be represented as
--       [ PathParam foo, StaticParam "..", PathParam bar]
data PathParamPiece a = DynParamPiece    a
                      | StaticParamPiece T.Text
                    deriving (Show, Eq, Generic)

type PathParamPieceUntyped = PathParamPiece T.Text
type PathParamPieceTyped   = PathParamPiece Ref

data Primitive
  = Date
  | DateTime
  | Password
  | Byte
  | Binary
  | Text
  | Float
  | Double
  | Number
  | Int
  | Int32
  | Int64
  | Bool
  | File
  | Null
  | Default Ref
  | Maybe Ref
  | Array Ref
  | Tuple [Ref]
  | MultiSet Ref
  | DelimitedCollection Delimiter Ref  
  deriving (Show, Eq, Generic)

data SwaggerHaskType
  = Primitive Primitive
  | Object CustomType
  deriving (Show, Eq, Generic)

data CustomTypeWithProv = CustomTypeWithProv { typeProv       :: Provenance
                                             , originalName   :: DefinitionName
                                             , typeDefinition :: TypeDefinition
                                             } deriving (Show, Eq, Generic)

updateCustomType :: (CustomType -> CustomType) -> TypeDefinition -> TypeDefinition
updateCustomType f tyDef =
  tyDef { customHaskType = f (customHaskType tyDef)
        }

updateTypeConstructor :: TypeConstructor -> CustomType -> CustomType
updateTypeConstructor newTyCon (CustomType _ dcons) =
  CustomType newTyCon dcons
updateTypeConstructor newTyCon (CustomNewType _ dcon fld ref) =
  CustomNewType newTyCon dcon fld ref

updateDataConstructorName :: DataConstructorName -> DataConstructorName -> CustomType -> CustomType
updateDataConstructorName oldDcon newDcon (CustomType tyCon dcons) =
  let newDcons = map (\dcon@(DataConstructor n flds) -> case n == oldDcon of
                                                         True -> DataConstructor newDcon flds
                                                         False -> dcon
                     ) dcons
  in  CustomType tyCon newDcons
updateDataConstructorName oldDcon newDcon (CustomNewType tyCon dcon fld ref) =
  let newDcon' = case oldDcon == dcon of
        True -> newDcon
        _    -> dcon
  in  CustomNewType tyCon newDcon' fld ref



getTypeConstructor :: CustomType -> TypeConstructor
getTypeConstructor (CustomType tyCon _)        = tyCon
getTypeConstructor (CustomNewType tyCon _ _ _) = tyCon

dataConstructorNames :: CustomType -> [DataConstructorName]
dataConstructorNames (CustomType _ dcons) =
  map (\(DataConstructor n _) -> n) dcons
dataConstructorNames (CustomNewType _ dcon _ _) = [dcon]

data CustomType = CustomType TypeConstructor    [DataConstructor]
                | CustomNewType TypeConstructor DataConstructorName (Maybe RecordName) Ref
                deriving (Show, Eq, Generic)

data Provenance = Provenance Module [T.Text]
                deriving (Show, Eq, Generic)

data Module = Global
            | RouteLocal (Route UnparsedPiece)
            | Local (Route UnparsedPiece) Method
            deriving (Show, Eq, Generic)

data HaskModule = HaskModule [T.Text] T.Text
                -- ^ (components, qualification)
                deriving (Show, Eq, Generic)

type TypeConstructor     = T.Text
type DataConstructorName = T.Text
type KeyName             = T.Text
type RecordName          = T.Text

data DataConstructor = DataConstructor DataConstructorName [ (Maybe RecordName, Ref) ]
                       deriving (Show, Eq, Generic)

data Delimiter = SlashT | Space | Pipe | Comma
               deriving (Show, Eq, Generic)

data TypeDefinition = TypeDefinition { customHaskType      :: CustomType
                                     , derivingConstraints :: [ConstraintName]
                                     , instances           :: [Instance]
                                     } deriving (Show, Eq, Generic)

defaultTypeDefinition :: CustomType -> TypeDefinition
defaultTypeDefinition cty =
  TypeDefinition { customHaskType      = cty
                 , derivingConstraints = []
                 , instances           = []
                 }

type ConstraintName = T.Text
type InstanceBody   = T.Text

data SwaggerGenState = SwaggerGenState { typeState   :: TypeState
                                       , routeState  :: RouteState
                                       , apiContract :: ContractState
                                       , modules     :: ModuleState
                                       , errors      :: [ErrorMessage]
                                       } deriving (Show, Eq, Generic)

defaultSwaggerState :: SwaggerGenState
defaultSwaggerState = SwaggerGenState { typeState   = HM.empty
                                        , routeState  = HM.empty
                                        , apiContract = HM.empty
                                        , modules     = HM.empty
                                        , errors      = []
                                        }

type TypeState      = HM.HashMap TypeMeta   TypeDefinition

type RouteState     = HM.HashMap (Route UnparsedPiece) (Route Ref)

data ParamType a = QueryParam
                 | FormParam
                 | PathParam
                 | FileParam
                 | HeaderParam
                 | BodyParam [a]
                 deriving (Show, Eq, Generic)

data ResponseType a = ApiOutput [a]
                    | ApiError [a]
                    | HeaderOutput
                  deriving (Show, Eq, Generic)

data TypeMeta = Definition Provenance DefinitionName
              deriving (Show, Eq, Generic)

instance Hashable (RoutePiece T.Text)
instance Hashable TypeMeta
instance Hashable (Route UnparsedPiece)
instance Hashable Method
instance Hashable Provenance
instance Hashable Module

type DefinitionName = T.Text

type ModuleState   = HM.HashMap Module
                                HaskModule 
type ContractState = HM.HashMap ((Route UnparsedPiece), Method)
                                (ContractInfo Ref)

data ContractInfo ty = ContractInfo { contentTypes :: [T.Text]
                                    , requestBody  :: [(T.Text, ty)]
                                    , queryParam   :: Maybe ty
                                    -- , pathParam    :: ty
                                    , formParam    :: Maybe ty
                                    , fileParam    :: Maybe ty
                                    -- , cookieParam  :: Maybe ty
                                    , headerParam  :: Maybe ty
                                    , apiOutput    :: Maybe ty
                                    , apiError     :: Maybe ty
                                    -- , headerOut    :: ty
                                    -- , cookieOut    :: Maybe ty
                                    } deriving (Show, Eq, Generic)

newtype SwaggerGenerator a =
  SwaggerGenerator { getSwaggerGenerator :: ReaderT Config (StateT SwaggerGenState IO) a }
  deriving (MonadReader Config, MonadState SwaggerGenState, Functor, Applicative, Monad, MonadIO)

runSwaggerGenerator :: Config -> SwaggerGenerator () -> IO SwaggerGenState
runSwaggerGenerator cfg (SwaggerGenerator rsio) = 
  snd <$> runStateT (runReaderT rsio cfg) defaultSwaggerState 

moduleComponents :: Module -> [T.Text]
moduleComponents Global             = [ "GlobalDefinitions", "Types"]
moduleComponents (RouteLocal route) = routeComponents route ++ [ "Types" ]
moduleComponents (Local r m)        = routeComponents r ++ [ methodComponent m, "Types" ]

methodComponent :: Method -> T.Text
methodComponent = T.pack . show

routeComponents :: Route UnparsedPiece -> [T.Text]
routeComponents (Route pieces) =
  map (\piece -> case piece of
          Static x -> T.toTitle (replacePuncts x)
          Dynamic x -> T.toTitle (replacePuncts x)
      ) pieces

  where replacePuncts = optionalH . T.map (replacePunct)
        replacePunct x | C.isAlphaNum x = x
                       | otherwise      = '_'
        optionalH x = case T.head x of
          '_' -> "H" <> x
          _   -> x

moduleQualification :: [T.Text] -> T.Text
moduleQualification =
  T.pack . map thead

  where thead x = case T.null x of
          True  -> error "Panic: unexpected empty component of module"
          False -> T.head x

haskModuleComponents :: HaskModule -> [T.Text]
haskModuleComponents (HaskModule cs _) = cs

haskModuleQual :: HaskModule -> T.Text
haskModuleQual (HaskModule _ qual) = qual

mkDefinitionName :: Provenance -> T.Text -> SwaggerGenerator DefinitionName
mkDefinitionName prov n = do
  newDefinitionName prov n <$> gets (keysSet . typeState)
  
newDefinitionName :: Provenance -> T.Text -> HS.HashSet TypeMeta -> DefinitionName
newDefinitionName prov n hs = getDefnName (resolveClashes' defResolver hs (Definition prov n))
  where defResolver i (Definition p defn) = Definition p (defn <> "_" <> T.pack (show i))
        getDefnName (Definition _ defn) = defn

resolveClashes' :: (Eq k, Hashable k) => (Int -> k -> k) -> HS.HashSet k -> k -> k
resolveClashes' f set k = go Nothing
  where go (Just ctr) =
          let resk = f ctr k
          in case HS.member resk set of
               True  -> go (Just (ctr + 1))
               False -> resk
        go Nothing =
          case HS.member k set of
            True  -> go (Just 0)
            False -> k

resolveModuleClashes :: HS.HashSet [T.Text] -> HS.HashSet T.Text -> [T.Text] -> T.Text -> HaskModule
resolveModuleClashes seenImps seenQuals modCmps modQual =
  let newModCmps = resolveModClashes seenImps modCmps
      newModQual = resolveQualClashes seenQuals modQual
  in  HaskModule newModCmps newModQual

resolveModClashes :: HS.HashSet [T.Text] -> [T.Text] -> [T.Text]
resolveModClashes = resolveClashes' resolveModClash
  where resolveModClash ctr ks = case reverse ks of
          (x : xs) -> reverse (x <> "_" <> T.pack (show ctr) : xs)
          [] -> error "Panic: import components cannot be empty"

resolveQualClashes :: HS.HashSet T.Text -> T.Text -> T.Text
resolveQualClashes = resolveClashes' resolveQualClash
  where resolveQualClash ctr k = k <> "_" <> T.pack (show ctr)

mkModuleState :: [((Route UnparsedPiece), Method)] -> ModuleState
mkModuleState rms =
  let (imps, _, _) =
        L.foldl' (\(!mods, !seenImpComps, !seenQuals) (r, m) ->
                       let routeMod      = RouteLocal r
                           rmMod         = Local r m
                           routeModCmps  = moduleComponents routeMod
                           rmModCmps     = moduleComponents rmMod
                           haskRouteMod  = resolveModuleClashes seenImpComps seenQuals routeModCmps (moduleQualification routeModCmps)
                           haskRMMod     = resolveModuleClashes seenImpComps seenQuals rmModCmps (moduleQualification rmModCmps)
                           newMods       = HM.insertWith (flip const) routeMod haskRouteMod $
                                             HM.insert rmMod haskRMMod mods                           
                       in   ( newMods
                            , HS.insert (haskModuleComponents haskRouteMod) $
                                       HS.insert (haskModuleComponents haskRMMod) seenImpComps
                            , HS.insert (haskModuleQual haskRouteMod) $
                                       HS.insert (haskModuleQual haskRMMod)
                                       seenQuals
                            )
               ) ( HM.insert Global gblHaskMod HM.empty
                 , HS.empty
                 , HS.empty
                 ) rms
      
  in imps

  where gblHaskMod = let gblModCmps = (moduleComponents Global)
                         gblModQual = moduleQualification gblModCmps
                     in HaskModule gblModCmps gblModQual

generateSwaggerState :: Swagger -> SwaggerGenerator ()
generateSwaggerState sw = do
  globalSwaggerDefinitions (_swaggerDefinitions sw)
  generateRoutesState (_swaggerParameters sw) (_swaggerResponses sw) (_swaggerPaths sw)
  moduleState

moduleState :: SwaggerGenerator ()
moduleState = do
  rms <- gets (HM.keys . apiContract)
  modify' (\s -> s { modules = mkModuleState rms })

generateRoutesState :: Definitions Param -> Definitions Response -> OHM.InsOrdHashMap FilePath PathItem -> SwaggerGenerator ()
generateRoutesState globalParams globalResps pItems = do
  OHM.foldlWithKey' updatePathItems (pure ()) pItems

  where updatePathItems :: SwaggerGenerator () -> FilePath -> PathItem -> SwaggerGenerator ()
        updatePathItems sws routeStr pItem = do
          let route = parseRoute routeStr
              updState = generateRouteMethodState globalParams globalResps (_pathItemParameters pItem) route
          sws
          ppG   <- updState GET (_pathItemGet pItem)
          ppP   <- updState PUT (_pathItemPut pItem)
          ppPT  <- updState POST (_pathItemPost pItem)
          ppDel <- updState DELETE (_pathItemDelete pItem)
          ppOpt <- updState OPTIONS (_pathItemOptions pItem)
          ppPat <- updState PATCH (_pathItemPatch pItem)
          routeRef <- parseRouteType route (groupByParamName [ppG, ppP, ppPT, ppDel, ppOpt, ppPat])
          insertRoute route routeRef
          pure ()

        groupByParamName :: [(Method, [Param])] -> [(T.Text, [(Method, Param)])]
        groupByParamName methAndPars =
            let methPars = L.groupBy   (\(_, parl) (_, parr) -> _paramName parl == _paramName parr) $
                           L.sortBy    (\(_, parl) (_, parr) -> compare (_paramName parl) (_paramName parr)) $ 
                           concatMap (\(meth, pars) ->
                                        map (\par -> (meth, par)
                                            ) pars) methAndPars
                methParsNamed = map (\x -> case x of
                                        (x' : _) -> (_paramName (snd x'), x)
                                        _        -> error "Panic: group cannot be empty"
                                    ) methPars
            in  methParsNamed
            
generateRouteMethodState :: Definitions Param -> Definitions Response -> [SW.Referenced Param] -> Route UnparsedPiece -> Method -> Maybe Operation -> SwaggerGenerator (Method, [Param])
generateRouteMethodState globalParams globalResps routeRefs route meth mOp =
  case mOp of
    Nothing -> pure (meth, [])
    Just op -> do
      let opParams    = overrideParams routeRefs (_operationParameters op)
                        -- TODO: ResponsesDefault ignored.
          opResponses = _operationResponses op
      let (pp, qp, fp, fip, hp, bp) = groupParamDefinitions globalParams opParams
      qpTy   <- paramDefinitions QueryParam route meth qp
      fpTy   <- paramDefinitions FormParam route meth fp
      fipTy  <- paramDefinitions FileParam route meth fip
      hpTy   <- paramDefinitions HeaderParam route meth hp
      bodyTy <- paramDefinitions (BodyParam []) route meth bp
      let (sucs, fails, _headers) = groupOutputDefinitions globalResps opResponses
      succTy <- outputDefinitions (ApiOutput []) route meth sucs
      errTy <- outputDefinitions (ApiError []) route meth fails
      insertContract route meth qpTy fpTy fipTy hpTy bodyTy succTy errTy
      pure (meth, pp)

insertRoute :: Route UnparsedPiece ->
               Route Ref           ->
               SwaggerGenerator ()
insertRoute k v = do
  modify' (\sw -> sw { routeState = HM.insert k v (routeState sw) })
  mapM_ (updateTypeDefinitionRec (addInputInstance PathParam)) routeRefs

  where routeRefs = case v of
          Route ps -> catMaybes $ map (\x -> case x of
                                         Static{} -> Nothing
                                         Dynamic r -> Just r
                         ) ps
insertContract :: Route UnparsedPiece ->
                   Method ->
                  Maybe Ref ->
                  Maybe Ref ->
                  Maybe Ref ->
                  Maybe Ref ->
                  Maybe Ref ->
                  Maybe Ref ->
                  Maybe Ref ->
                  SwaggerGenerator ()
insertContract r m qp fp fip hp bdy suc err = do
  let cInfo = ContractInfo { queryParam  = qp
                           -- , pathParam   = ""
                           , formParam   = fp
                           , fileParam   = fip
                           -- , cookieParam = ""
                           , headerParam = hp
                           , requestBody = maybe [] (\b -> [("JSON", b)]) bdy
                           , apiOutput   = suc
                           , apiError    = err
                           , contentTypes = []
                           -- , cookieOut   = ""
                           -- , headerOut   = ""
                           }
  modify' (\s -> s { apiContract = HM.insert (r, m) cInfo (apiContract s) })
      
parseRouteType :: Route UnparsedPiece ->
                  [(T.Text, [(Method, Param)])] ->
                  SwaggerGenerator (Route Ref)
parseRouteType rt params = do
  let pps = routePathParamPiece rt
      unDynParamPiece = map (\d -> case d of
                                DynParamPiece x -> x
                                _               -> error "Panic: impossible case @unDynParamPiece"
                            ) . filter isDynParamPiece 
      ppDyns = L.foldl' (\acc x -> case x of
                            Dynamic ds -> unDynParamPiece ds : acc
                            Static _   -> acc
                        ) [] (getRoute pps)
  tys <- go ppDyns
  pure (typedRouteParamPiece pps tys)

  where go ppDyns = mapM go1 ppDyns


        singlePP pp = do
              case L.lookup pp params of
                Just methAndPars -> do
                  pathParamDefinition rt pp methAndPars
                Nothing          -> error ("Panic: route param not found: " ++ show (pp, map fst params))
        go1 [pp] = singlePP pp
        go1 pps  = do
          ppTys <- mapM singlePP pps
          let typeName = T.concat pps
          let ppProdTy = productType typeName
                                     (zip pps ppTys)
          cty <- insertDefinition (routeLocalProv rt) typeName ppProdTy
          pure cty
                
overrideParams :: [SW.Referenced Param] -> [SW.Referenced Param] -> [SW.Referenced Param]
overrideParams routeRefs methRefs =
  L.union methRefs routeRefs

globalSwaggerDefinitions :: Definitions Schema -> SwaggerGenerator ()
globalSwaggerDefinitions defs = do
  _ <- schemaDefinitions (lookupGlobalDefinitionOrGenerate defs) globalProv defs
  pure ()
                                                  
outputDefinitions :: ResponseType MediaType -> Route UnparsedPiece -> Method ->
                      [(HttpStatusCode, Maybe (SW.Referenced Schema))] -> SwaggerGenerator (Maybe Ref)
outputDefinitions _ _ _ []    = pure Nothing
outputDefinitions rType r m rs = Just <$> do
  tyRefs <- mapM (uncurry outputDefinition) rs
  let tyDef = sumType (responseTypeName rType)
                      (map (\(code, refT) ->
                              (responseDataConName rType code, [refT])) tyRefs)
  insertOutputType rType r m tyDef

  where outputDefinition code mrsch = case mrsch of
          Nothing -> pure (code, Inline Null)
          Just (SW.Ref (Reference n)) -> do
            refT <- lookupGlobalDefinition n
            pure (code, refT)
          Just (SW.Inline sc)            -> do
            -- TODO: a name is being made up here
            --       this name should not be clashing with any key
            --       To handle this, verify that all keys under this route/meth
            --       do not have this name
            let n = responseName code
            refT <- schemaDefinition lookupGlobalDefinition (localProv r m) n sc
            pure (code, refT)
        responseName code = "Response" <> T.pack (show code)

groupOutputDefinitions :: Definitions Response -> Responses ->
                           ( [(HttpStatusCode, Maybe (SW.Referenced Schema))] -- ApiSuccess
                           , [(HttpStatusCode, Maybe (SW.Referenced Schema))] -- ApiError
                           , [(HttpStatusCode, HeaderName, Header)]                           -- all headers of
                                                                              -- success & failure
                           )
-- TODO: handling of default responses                           
groupOutputDefinitions globResponses resps =
  let inlineResps =
        map (\(code, rres) -> case rres of
                      SW.Ref (Reference n) -> 
                        case OHM.lookup n globResponses of
                          Nothing -> error "Panic: global response not found"
                          Just p  -> (code, p)
                      SW.Inline p -> (code, p)
                  ) $ OHM.toList (_responsesResponses resps)
      hds         = concatMap (\(code, resp) ->
                                 map (\(hn, hdr) -> (code, hn, hdr)) (OHM.toList (_responseHeaders resp))) inlineResps
      successes   = map (fmap _responseSchema) $ filter (isSuccess . fst) inlineResps
      failures    = map (fmap _responseSchema) $ filter (not . isSuccess . fst) inlineResps
      isSuccess c = c >= 200 && c < 300
  in  (successes, failures, hds)

  

groupParamDefinitions :: Definitions Param ->
                          [SW.Referenced Param] ->
                          ( [Param]  -- path param
                          , [Param]  -- query param
                          , [Param]  -- form param
                          , [Param]  -- file param
                          , [Param]  -- header param
                          , [Param]  -- body param
                          )
groupParamDefinitions globParams params =
  let inlinePars = map inlinePar params
      inlinePar rpar = case rpar of
        SW.Ref (Reference n) ->
          case OHM.lookup n globParams of
            Nothing -> error "Panic: global param not found"
            Just p  -> p
        SW.Inline p          -> p
  in  ( pathParams   inlinePars
      , queryParams  inlinePars
      , formParams   inlinePars
      , fileParams   inlinePars
      , headerParams inlinePars
      , bodyParams   inlinePars
      )

  where queryParams  = filter isQueryParam
        formParams   = filter isFormParam
        pathParams   = filter isPathParam
        fileParams   = filter isFileParam
        headerParams = filter isHeaderParam
        bodyParams   = filter isBodyParam

isQueryParam :: Param -> Bool
isQueryParam par =
  case _paramSchema par of
    ParamOther parOther -> case _paramOtherSchemaIn parOther of
      ParamQuery -> True
      _          -> False
    _ -> False

isPathParam :: Param -> Bool
isPathParam par =
  case _paramSchema par of
    ParamOther parOther -> case _paramOtherSchemaIn parOther of
      ParamPath  -> True
      _          -> False
    _ -> False

isHeaderParam :: Param -> Bool
isHeaderParam par =
  case _paramSchema par of
    ParamOther parOther -> case _paramOtherSchemaIn parOther of
      ParamHeader  -> True
      _          -> False
    _ -> False

isBodyParam :: Param -> Bool
isBodyParam par =
  case _paramSchema par of
    ParamBody _ -> True
    _ -> False

isFileParam :: Param -> Bool
isFileParam = maybe False id . isFileOrFormParam

isFormParam :: Param -> Bool
isFormParam = maybe False not . isFileOrFormParam

isFileOrFormParam :: Param -> Maybe Bool
isFileOrFormParam par =
  case _paramSchema par of
    ParamOther parOther -> case _paramOtherSchemaIn parOther of
      ParamFormData -> case _paramSchemaType (_paramOtherSchemaParamSchema parOther) of
        Just (SwaggerFile {}) -> Just True
        _                     -> Just False        
      _             -> Nothing
    _ -> Nothing

paramDefinitions :: ParamType MediaType -> Route UnparsedPiece -> Method -> [Param] -> SwaggerGenerator (Maybe Ref)
paramDefinitions pType rt method params
  | params == [] = pure Nothing
  | otherwise   = do      
      ptys <- mapM (paramDefinition rt method) params
      let tyDef = productType (paramTypeName pType)
                              (map (\pty -> ( paramName pty
                                           , paramType pty
                                           )
                                   ) ptys
                              )
      Just <$> insertParamType pType rt method tyDef

paramTypeName :: (Show a) => ParamType a -> T.Text
paramTypeName (BodyParam _) = "BodyParam"
paramTypeName pt            = T.pack . show $ pt

responseTypeName :: ResponseType a -> T.Text
responseTypeName (ApiOutput _)     = "ApiOutput"
responseTypeName (ApiError _)      = "ApiError"
responseTypeName HeaderOutput      = "HeaderOutput"

responseDataConName :: ResponseType a -> Int -> T.Text
responseDataConName rt s =
  responseTypeName rt <>
  T.pack (show s)

data ParamTypeInfo = ParamTypeInfo { paramType       :: Ref
                                   , paramName       :: DefinitionName
                                   } deriving (Show, Eq, Generic)

pathParamDefinition :: Route UnparsedPiece -> T.Text -> [(Method, Param)] -> SwaggerGenerator Ref
pathParamDefinition r parN pars = do
  let nubedPars = L.nubBy (\a b -> snd a == snd b) pars
      updParamName p m = p { _paramName = _paramName p <> "PathParam" <> T.pack (show m) }
      pathParamConName m = parN <> "PathParamCon" <> T.pack (show m)
  case nubedPars of
    [(_, p)] -> do
      paramType <$> paramDefinition' (routeLocalProv r) p
    xs       -> do
      tyRefs <- mapM (\(m, p) -> do
                        tyRef <- paramType <$> paramDefinition' (routeLocalProv r) (updParamName p m)
                        pure (m, tyRef)
                    ) xs
      let tyDef = sumType parN
                  (map (\(meth, refT) ->
                           (pathParamConName meth, [refT])) tyRefs)
      
      cty <- insertDefinition (routeLocalProv r) parN tyDef
      pure cty
      
paramDefinition :: Route UnparsedPiece -> Method -> Param -> SwaggerGenerator ParamTypeInfo
paramDefinition r m = paramDefinition' (localProv r m)

paramDefinition' :: Provenance -> Param -> SwaggerGenerator ParamTypeInfo
paramDefinition' prov par = do
  let parName = _paramName par
      req  = case _paramRequired par of
        Nothing -> False
        Just t  -> t
  paramTypeRef <- case _paramSchema par of
                   ParamBody (SW.Inline sch) -> do
                     schemaDefinition lookupGlobalDefinition prov parName sch
                   ParamBody (SW.Ref (Reference n)) ->
                     lookupGlobalDefinition n
                   ParamOther pOth           -> 
                     paramSchemaDefinition prov parName (_paramOtherSchemaParamSchema pOth)
  let reqParamTypeRef = case req of
        True  -> case paramTypeRef of
          Inline (Default r) -> r
          _                  -> paramTypeRef
        False -> case paramTypeRef of
          Inline (Default r) -> Inline (Default r)
          _                  -> Inline (Maybe paramTypeRef)
  pure (ParamTypeInfo reqParamTypeRef parName)

paramSchemaDefinition :: Provenance -> DefinitionName -> ParamSchema t -> SwaggerGenerator Ref
paramSchemaDefinition prov def parSch = hasDef <$> go
  where hasDef ref = case _paramSchemaDefault parSch of
          Just _  -> Inline (Default ref)
          Nothing -> ref
          
        go =
         case (_paramSchemaType parSch) of
           Just SwaggerString -> 
             case _paramSchemaFormat parSch of
               Just "date"      -> inline Date
               Just "date-time" -> inline DateTime
               Just "password"  -> inline Password
               Just "byte"      -> inline Byte
               Just "binary"    -> inline Binary
               Just _           -> inline Text
               Nothing ->
                 case _paramSchemaEnum parSch of
                   Nothing    -> inline Text
                   Just enums -> enumDefinition enums
           Just SwaggerNumber ->
             case _paramSchemaFormat parSch of
               Just "float"  -> inline Float
               Just "double" -> inline Double
               _ -> inline Number
           Just SwaggerInteger -> 
             case _paramSchemaFormat parSch of
               Just "int32" -> inline Int32
               Just "int64" -> inline Int64
               _ -> inline Int
           Just SwaggerBoolean -> inline Bool
           Just SwaggerArray -> arrayDefinition (_paramSchemaItems parSch)
           Just SwaggerNull -> inline Null
           Just SwaggerFile -> inline File
           Just SwaggerObject -> error "Panic: impossible case of object in paramDefinition"
           Nothing -> inline Null
           
        enumDefinition enumVals = do
          let mtyDef = enumType def <$> menums
              menums = traverse (\v -> case v of
                                   A.String t -> Just t
                                   _          -> Nothing) enumVals
          case mtyDef of
            Just tyDef ->
              insertDefinition prov def tyDef
            Nothing    -> error "TODO: non string in enums"

        arrayDefinition = error "TODO: array in param"
        inline = pure . Inline

singleton :: a -> [a]
singleton a = [a]

sumType :: T.Text -> [(T.Text, [Ref])] -> TypeDefinition
sumType rawName ctors =
  let dcons    = map (\(ctorN, args) -> DataConstructor (mkDataConstructorName ctorN) (ctorArgs args)) ctors
      cty      = CustomType (mkTypeConstructorName rawName) dcons
      ctorArgs = map (\ref -> (Nothing, ref))
  in  defaultTypeDefinition cty
  
enumType :: T.Text -> [T.Text] -> TypeDefinition
enumType rawName rawCtors =
  let dcons = map (\ctor -> DataConstructor (mkDataConstructorName ctor) []) rawCtors
      cty   = CustomType (mkTypeConstructorName rawName) dcons
  in  defaultTypeDefinition cty

newType :: T.Text -> Maybe T.Text -> Ref -> TypeDefinition
newType rawName rawFld ref =
  let dconName = mkDataConstructorName rawName
      cty = CustomNewType (mkTypeConstructorName rawName) dconName (mkRecordName <$> rawFld) ref
  in  defaultTypeDefinition cty
  
productType :: T.Text -> [(T.Text, Ref)] -> TypeDefinition
productType rawName rawFlds =
  let dcon = DataConstructor
             (mkDataConstructorName rawName)
             fields
      fields = map (\(fld, ref) -> (Just (mkRecordName fld), ref)) rawFlds
      cty = CustomType (mkTypeConstructorName rawName) [dcon]
      
  in  defaultTypeDefinition cty
  
mkRecordName :: T.Text -> T.Text
mkRecordName = T.toLower

mkDataConstructorName :: T.Text -> T.Text
mkDataConstructorName = T.toTitle

mkTypeConstructorName :: T.Text -> T.Text
mkTypeConstructorName = T.toTitle

schemaDefinitions :: (DefinitionName -> SwaggerGenerator Ref) -> Provenance -> Definitions Schema -> SwaggerGenerator [Ref]
schemaDefinitions defLookup prov defs =
  OHM.foldlWithKey' (\s k sc -> do
                        xs <- s
                        x <- schemaDefinition defLookup prov k sc
                        pure (x : xs)
                    ) (pure []) defs

newtypeIfRoot :: Provenance -> DefinitionName -> Ref -> SwaggerGenerator Ref
newtypeIfRoot prov n ref
  | isGlobalRoot prov = go 
  | otherwise         = pure ref

  where isGlobalRoot (Provenance Global []) = True
        isGlobalRoot _                      = False

        go = do
          let nt = newType n Nothing ref
          insertDefinition prov n nt
          
schemaDefinition :: (DefinitionName -> SwaggerGenerator Ref) -> Provenance -> DefinitionName -> Schema -> SwaggerGenerator Ref
schemaDefinition defLookup prov def sch = do
  liftIO $ putStrLn $ "Schema definition: " ++ show def
  let paramSchema = _schemaParamSchema sch
      additionalProps props = case OHM.null props of
        True -> case _schemaAdditionalProperties sch of
                 Nothing -> props
                 Just (AdditionalPropertiesAllowed _) -> props
                 Just (AdditionalPropertiesSchema _) -> props
        False -> props
  case (_paramSchemaType paramSchema) of
      Just SwaggerObject -> customTypeDefinition (additionalProps (_schemaProperties sch) )
      -- NOTE: Array is handled in both schemaDefinition and paramDefinition
      --       The invariant check is not being handled now (certain arrays are special)
      Just SwaggerArray -> do
        ref <- arrayDefinition (_paramSchemaItems paramSchema)
        newtypeIfRoot prov def ref
      _ -> do
        ref <- paramSchemaDefinition prov def paramSchema
        newtypeIfRoot prov def ref
        
   where reqType k rty =
           case k `L.elem` _schemaRequired sch of
             True -> case rty of
               Inline (Default r) -> r
               _                  -> rty
             False -> case rty of
               Inline (Default r) -> Inline (Default r)
               _                  -> Inline (Maybe rty)
         customTypeDefinition props = do
           --TODO: Additional properties not handled
           fields <- OHM.foldlWithKey' (\s k rsc -> do
                                          xs <- s
                                          x <- schemaField k rsc
                                          let rx = reqType k x
                                          pure ((k, rx) : xs)
                                      ) (pure []) props
           let tyDef = productType def fields
           insertDefinition prov def tyDef
           
         arrayDefinition mitems = case mitems of
           Just (SwaggerItemsPrimitive mfmt ipar) -> primitiveArrayDefinition mfmt ipar
           Just (SwaggerItemsObject rsc) -> objectArrayDefinition rsc
           Just (SwaggerItemsArray rscs) -> tupleDefinition rscs
           Nothing -> error "TODO: default for array"

         primitiveArrayDefinition :: Maybe (CollectionFormat t) -> ParamSchema t -> SwaggerGenerator Ref
         primitiveArrayDefinition mfmt ipar = do
           typeKeys <- getTypeStateKeys           
           let arrCtsDef = newDefinitionName prov (def <> "ArrayContents") typeKeys
               extProv   = extendProvenance def prov
           rcty <- paramSchemaDefinition extProv arrCtsDef ipar
           let unDefault = case rcty of
                             Inline (Default cty) -> cty
                             _                    -> rcty
           let arTy = case mfmt of
                 Nothing              -> Array rcty
                 Just CollectionCSV   -> DelimitedCollection Comma unDefault
                 Just CollectionSSV   -> DelimitedCollection Space unDefault
                 Just CollectionTSV   -> DelimitedCollection SlashT unDefault
                 Just CollectionPipes -> DelimitedCollection Pipe unDefault
                 Just CollectionMulti -> MultiSet unDefault
           pure (Inline arTy)
            
         objectArrayDefinition rsc = do
           case rsc of
             SW.Inline sc -> do
               arrCtsDef <- mkDefinitionName prov
                           (def <> "ArrayContents")
               let extProv   = extendProvenance def prov
               rty <- schemaDefinition defLookup extProv arrCtsDef sc
               pure (Inline (Array rty))
             SW.Ref (Reference n) -> do
               rty <- defLookup n
               pure (Inline (Array rty))

         tupleDefinition rscs = do
           rtys <- mapM (\(i, rsc) -> case rsc of
                           SW.Inline sc -> do
                             tupCtsDef <- mkDefinitionName prov
                                          (def <> "TupleContents" <> T.pack (show i))
                             let extProv = extendProvenance def prov
                             schemaDefinition defLookup
                                          extProv
                                          tupCtsDef sc
                           SW.Ref (Reference n) -> defLookup n
                       )
                       (zip ([0 .. ] :: [Int]) rscs)
           pure (Inline (Tuple rtys))
                  
         schemaField k rsc = case rsc of
           SW.Ref (Reference n) -> do
             defLookup n
           SW.Inline s -> do
             schemaDefinition defLookup (extendProvenance def prov) k s

getTypeStateKeys :: SwaggerGenerator (HS.HashSet TypeMeta)
getTypeStateKeys = gets (HS.fromList . HM.keys . typeState)
         
definitionRef :: Provenance -> DefinitionName -> SwaggerHaskType -> Ref
definitionRef prv defn (Object {}) = Ref (Definition prv defn)
definitionRef _ _ (Primitive ty)   = Inline ty

extendProvenance :: DefinitionName -> Provenance -> Provenance
extendProvenance n (Provenance m ks) = Provenance m (ks ++ [n])

lookupGlobalDefinitionOrGenerate :: Definitions Schema -> DefinitionName -> SwaggerGenerator Ref
lookupGlobalDefinitionOrGenerate schs defn = do
  mcTy <- gets (lookupDefinition defn . typeState)
  case mcTy of
    Nothing  -> do
      case OHM.lookup defn schs of
        Just k -> do
          schemaDefinition (lookupGlobalDefinitionOrGenerate schs) globalProv defn k
        Nothing -> error "Panic: key not found"
    Just cty -> pure (definitionRef globalProv defn (Object cty))

  where lookupDefinition defn' tyDefs =
          customHaskType <$> HM.lookup (Definition globalProv defn') tyDefs


lookupGlobalDefinition :: DefinitionName -> SwaggerGenerator Ref
lookupGlobalDefinition defn = do
  mcTy <- gets (lookupDefinition defn . typeState)
  tyState <- gets typeState
  case mcTy of
    Nothing  -> error $ "Panic: xname not found" ++ (show (defn, show tyState))
    Just cty -> pure (definitionRef globalProv defn (Object cty))

  where lookupDefinition defn' tyDefs =
          customHaskType <$> HM.lookup (Definition globalProv defn') tyDefs

globalProv :: Provenance
globalProv = Provenance Global []

localProv :: Route UnparsedPiece -> Method -> Provenance
localProv r m = Provenance (Local r m) []

routeLocalProv :: Route UnparsedPiece -> Provenance
routeLocalProv r = Provenance (RouteLocal r) []

insertOutputType :: ResponseType MediaType -> Route UnparsedPiece -> Method -> TypeDefinition -> SwaggerGenerator Ref
insertOutputType rt rte meth tyDef = do
  let prov = localProv rte meth
  def <- mkDefinitionName prov (responseTypeName rt)
  val <- insertWithTypeMeta (Definition prov def) tyDef
  let ref = definitionRef prov def (Object (customHaskType val))
  updateTypeDefinitionRec (addResponseInstance rt) ref
  pure ref
  
insertParamType :: ParamType MediaType -> Route UnparsedPiece -> Method -> TypeDefinition -> SwaggerGenerator Ref
insertParamType pt rt meth tyDef = do
  let prov = localProv rt meth
  def <- mkDefinitionName prov (paramTypeName pt)
  val <- insertWithTypeMeta (Definition prov def) tyDef
  let ref = definitionRef prov def (Object (customHaskType val))
  updateTypeDefinitionRec (addInputInstance pt) ref  
  pure ref

insertDefinition :: Provenance -> DefinitionName -> TypeDefinition -> SwaggerGenerator Ref
insertDefinition prov def tyDef = do
  val <- insertWithTypeMeta (Definition prov def) tyDef
  pure (definitionRef prov def (Object (customHaskType val)))

insertWithTypeMeta :: TypeMeta -> TypeDefinition -> SwaggerGenerator TypeDefinition
insertWithTypeMeta tyMeta = go -- 0

  where go {-ct-} tyDef = do
          liftIO $ putStrLn $ "insert for Def: " ++ show tyMeta
          insertDef tyDef
          {-
          tyState <- gets typeState
          let clss = HM.foldlWithKey' (\cs k v ->
                                         case checkNameClash k tyMeta v tyDef of
                                           []  -> cs        
                                           cls -> cls ++ cs
                                      ) [] tyState
          case clss of
            [] -> insertDef tyDef
            _  -> go (ct + 1) (resolveClashes ct clss tyDef)
          -}
        insertDef val = do
          let key = tyMeta
          modify' (\sws -> sws { typeState   = HM.insertWith (flip const)
                                              key val
                                              (typeState sws)
                              , apiContract = apiContract sws
                              , errors      = errors sws
                              }
                  )
          pure val
          
data TypeClash = TypeNameClash
               | ConstructorClash T.Text
               deriving (Show, Eq)

type Counter = Int

resolveTypeDefinitionClashes :: Counter -> [TypeClash] -> TypeDefinition -> TypeDefinition
resolveTypeDefinitionClashes ct cs tyDef =
  L.foldl' (\curTyDef cls -> case cls of
               TypeNameClash -> let tyCon = getTypeConstructor (customHaskType curTyDef)
                               in updateCustomType (updateTypeConstructor (typeNameClashResolution ct tyCon)) curTyDef
               ConstructorClash ctorN -> updateCustomType (updateDataConstructorName ctorN (ctorNameClashResolution ct ctorN)) curTyDef
           ) tyDef cs

  where typeNameClashResolution ctr n = n <> "_Ty"  <> T.pack (show ctr)
        ctorNameClashResolution ctr n = n <> "_Con" <> T.pack (show ctr)
        
checkNameClash :: TypeDefinition -> TypeDefinition -> [TypeClash]
checkNameClash tyl tyr =
  let clashes = typeNameClash (getTypeConstructor l)
                              (getTypeConstructor r) ++
                constructorNameClashes (dataConstructorNames l)
                                       (dataConstructorNames r)
      l = customHaskType tyl
      r = customHaskType tyr
  in clashes

  where typeNameClash t1 t2 = case t1 == t2 of
          True -> [ TypeNameClash ]
          False -> []

        constructorNameClashes xs ys =
          map ConstructorClash (L.intersect xs ys)

-- NOTE: Just checking for {. Can it be escaped?
parseRoute :: FilePath -> Route UnparsedPiece
parseRoute fp =
  let pieces = filter (not . null) (splitOn '/' fp)
  in Route $
     map (\piece -> case elem '{' piece of
             True -> Dynamic (T.pack piece)
             False -> Static (T.pack piece)
         ) pieces

routePathParamPiece :: Route UnparsedPiece -> Route [PathParamPieceUntyped]
routePathParamPiece (Route pieces) =
  Route $
  map (\piece -> case piece of
          Dynamic t -> Dynamic (parsePathParam t)
          Static s  -> Static s
      ) pieces

typedRouteParamPiece :: Route [PathParamPieceUntyped] -> [Ref] -> Route Ref
typedRouteParamPiece (Route rps) ppRefs =
  Route $
  L.reverse $ snd $ 
  L.foldl' (\(refs, ps) a -> case a of
               Static t -> (refs, Static t : ps)
               Dynamic _ -> case refs of
                 (ref : refs') -> (refs', Dynamic ref : ps)
                 []            -> error "Panic: impossible case @typedRouteParamPiece"
           ) (ppRefs, []) rps
  
parsePathParam :: UnparsedPiece -> [PathParamPieceUntyped]
parsePathParam piece = case routeParser piece of
  Just xs -> xs
  Nothing -> error "Panic: error in parsing route"

isDynParamPiece :: PathParamPiece a -> Bool
isDynParamPiece (DynParamPiece _) = True
isDynParamPiece _                 = False

isDynPiece :: RoutePiece a -> Bool
isDynPiece (Dynamic {}) = True
isDynPiece _            = False

splitOn :: Char -> String -> [String]
splitOn _ ""  =  []
splitOn c s   =  cons (case break (== c) s of
                          (l, s') -> (l, case s' of
                                          []      -> []
                                          _:s''   -> splitOn c s''))
  where
    cons ~(h, t) = h : t  

isSameModule :: Module -> Module -> Bool
isSameModule p1 p2 = p1 == p2

getProvenanceModule :: TypeMeta -> Module
getProvenanceModule (Definition prov _)  = go prov
  where go (Provenance m _) = m

provModule :: Provenance -> Module
provModule (Provenance m _) = m

type RouteParser = M.Parsec () T.Text

routeParser :: UnparsedPiece -> Maybe [PathParamPiece T.Text]
routeParser = either (const Nothing) Just . M.parse go "ROUTE"

  where go :: RouteParser [PathParamPiece T.Text]
        go = do
          x <- (Left <$> M.eof) M.<|> (Right <$> M.anySingle)
          case x of
            Right '{' -> do
              piece <- M.some (M.anySingleBut '}')
              _ <- M.char '}'
              ((:) (DynParamPiece (T.pack piece))) <$> go
            Right v -> do
              piece <- M.some (M.anySingleBut '{')
              ((:) (StaticParamPiece (T.pack (v : piece)))) <$> go
            Left _ -> pure []

{-
{
field = \def field v -> "${v} :. ${def}"
ctor  = \def ctor -> "${ctor}"
}
-}

data InstanceTemplate = InstanceTemplate
  { instanceType :: Instance
  , className    :: T.Text
  , methods      :: [MethodTemplate]
  } deriving (Generic)

data MethodTemplate = MethodTemplate
  { fieldTemplate   :: T.Text -> T.Text -> T.Text -> T.Text
                      -- def, field, var 
  , ctorTemplate    :: T.Text -> T.Text -> T.Text
                      -- def, Ctor
  , fieldCombinator :: T.Text
  , body            :: T.Text
  , methodName      :: T.Text
  } deriving (Generic)

data Instance = OutputInstance (ResponseType MediaType)
              | ParamInstance  (ParamType MediaType)
              deriving (Show, Generic, Eq)

{-
instance D.Interpret InstanceTemplate
instance D.Interpret MethodTemplate
instance D.Interpret Template
-}

{-
test :: IO ()
test = do
    x <- D.input D.auto "/tmp/config"
    let ctor = "Foo"
        var = "v"
        fields = [("x", "X"), ("y", "Y")]
        code = body instTmp
                    <> " \\" <> var <> " -> "
                    <> T.intercalate (" " <> fieldCombinator instTmp
                                          <> " ")
                                     ( ctorTemplate instTmp ctor ctor :
                                       map (\(x, y) -> fieldTemplate instTmp x y var) fields
                                     )
        instTmp = x :: InstanceTemplate
    putStrLn (T.unpack code)
-}

keysSet :: HM.HashMap k v -> HS.HashSet k
keysSet m = HS.fromMap (() <$ m)

resolveTypeClashes :: SwaggerGenerator ()
resolveTypeClashes = do
  tyState <- gets typeState
  modify' (\s -> s { typeState = go tyState })

  where go s =
          let modTys = HM.foldlWithKey' (\hses k@(Definition prov _) curTyDef ->
                                HM.insertWith (++) (provModule prov) [(k, curTyDef)] hses) HM.empty s
              newTyState = HM.foldl' (\newTyS tyList ->
                                        resolveTyListClashes 0 newTyS tyList) HM.empty modTys
          in  newTyState
          
        resolveTyListClashes _ !newTyState [] = newTyState
        resolveTyListClashes i !newTyState ((curProv, !curTyDef) : kvs) =
          let clss = L.foldl' (\cs (_, tyDef) ->
                                 case checkNameClash curTyDef tyDef of
                                   []  -> cs
                                   cls -> cls ++ cs
                            ) [] kvs
          in case clss of
               [] -> resolveTyListClashes 0 (HM.insert curProv curTyDef newTyState) kvs
               _  -> resolveTyListClashes (i + 1) newTyState (( curProv
                                                             , resolveTypeDefinitionClashes i clss curTyDef
                                                             ) : kvs
                                                            )

lookupTypeDefinition :: TypeMeta -> TypeState -> TypeDefinition
lookupTypeDefinition r ts =
  case HM.lookup r ts of
    Nothing -> error $ "Panic: type definition not found: " ++ show r
    Just v  -> v

updateTypeDefinition :: (TypeDefinition -> TypeDefinition) -> TypeMeta -> TypeState -> TypeState
updateTypeDefinition f r ts = HM.adjust f r ts

innerTypeRefs :: CustomType -> [Ref]
innerTypeRefs (CustomType _ dcons) = concatMap getRefTy dcons
  where getRefTy (DataConstructor _ xs) = map snd xs
innerTypeRefs (CustomNewType _ _ _ ref) = [ref]

addInputInstance :: ParamType MediaType -> TypeDefinition -> TypeDefinition
addInputInstance pt tyDef =
  tyDef { instances = ParamInstance pt : instances tyDef }

addResponseInstance :: ResponseType MediaType -> TypeDefinition -> TypeDefinition
addResponseInstance pt tyDef =
  tyDef { instances = OutputInstance pt : instances tyDef }

updateTypeDefinitionRec :: (TypeDefinition -> TypeDefinition) -> Ref -> SwaggerGenerator ()
updateTypeDefinitionRec tyFun ref = do
  modify' (\x -> x { typeState = go tyFun ref (typeState x) })

  where go f (Ref r) !ts = 
          let rTyDef  = lookupTypeDefinition r ts
              ityRefs = innerTypeRefs (customHaskType rTyDef)
          in updateTypeDefinition f r (L.foldl' (\ !acc ir -> go f ir acc) ts ityRefs)
        go f (Inline (Default r)) ts = go f r ts
        go f (Inline (Maybe r)) ts   = go f r ts
        go f (Inline (Array r)) ts   = go f r ts
        go f (Inline (Tuple rs)) !ts  =
          L.foldl' (\ !acc r -> go f r acc) ts rs
        go f (Inline (MultiSet r)) ts = go f r ts
        go f (Inline (DelimitedCollection _ r)) ts =
          go f r ts
        go _ _ ts = ts
             


              
                                              
                                              
              
