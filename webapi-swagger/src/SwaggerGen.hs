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

data Ref  = Inline Primitive
          | Ref    Provenance DefinitionName TypeConstructor
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

type Config = ()

type UnparsedPiece = T.Text

data Route a = Route { getRoute :: [RoutePiece a] }
           deriving (Show, Eq, Generic)

data RoutePiece a = Static T.Text | Dynamic a
                deriving (Show, Eq, Generic)

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
        , derivingConstraints = derivingConstraints tyDef
        , genericInstances = genericInstances tyDef
        }

updateTypeConstructor :: TypeConstructor -> CustomType -> CustomType
updateTypeConstructor newTyCon (CustomType _ dcons) =
  CustomType newTyCon dcons

updateDataConstructorName :: DataConstructorName -> DataConstructorName -> CustomType -> CustomType
updateDataConstructorName oldDcon newDcon (CustomType tyCon dcons) =
  let newDcons = map (\dcon@(DataConstructor n flds) -> case n == oldDcon of
                                                         True -> DataConstructor newDcon flds
                                                         False -> dcon
                     ) dcons
  in  CustomType tyCon newDcons

getTypeConstructor :: CustomType -> TypeConstructor
getTypeConstructor (CustomType tyCon _) = tyCon

dataConstructorNames :: CustomType -> [DataConstructorName]
dataConstructorNames (CustomType _ dcons) =
  map (\(DataConstructor n _) -> n) dcons

data CustomType = CustomType TypeConstructor
                             [DataConstructor]
                deriving (Show, Eq, Generic)

data Provenance = Global [T.Text]
                | RouteLocal (Route UnparsedPiece) [T.Text]
                | Local  (Route UnparsedPiece) Method [T.Text]
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
                                     , genericInstances    :: [Instance]
                                     } deriving (Show, Eq, Generic)

defaultTypeDefinition :: CustomType -> TypeDefinition
defaultTypeDefinition cty =
  TypeDefinition { customHaskType = cty
                 , derivingConstraints = []
                 , genericInstances = []
                 }

type ConstraintName = T.Text

data Instance = GenericInstance  ConstraintName
              | ExplicitInstance
              deriving (Show, Eq, Generic)

data SwaggerGenState = SwaggerGenState { typeState   :: TypeState
                                       , routeState  :: RouteState
                                       , apiContract :: ContractState
                                       , errors      :: [ErrorMessage]
                                       } deriving (Show, Eq, Generic)

type TypeState      = HM.HashMap TypeMeta TypeDefinition
type RouteState     = HM.HashMap (Route UnparsedPiece) (Route Ref)

data ParamType = QueryParam
               | FormParam
               | PathParam
               | FileParam
               | HeaderParam
               | BodyParam
               deriving (Show, Eq, Generic)

data ResponseType = ApiOutput
                  | ApiError
                  | HeaderOutput
                  deriving (Show, Eq, Generic)

data TypeMeta = ParamType    ParamType    (Route UnparsedPiece)  Method
              | ResponseType ResponseType (Route UnparsedPiece)  Method
              | Definition   Provenance   DefinitionName
              deriving (Show, Eq, Generic)

instance Hashable (RoutePiece T.Text)
instance Hashable TypeMeta
instance Hashable (Route UnparsedPiece)
instance Hashable Method
instance Hashable Provenance
instance Hashable ResponseType
instance Hashable ParamType

type DefinitionName = T.Text

type ContractState = HM.HashMap ((Route UnparsedPiece), Method)
                                (ContractInfo TypeConstructor)

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
  SwaggerGenerator { runSwaggerGenerator :: ReaderT Config (StateT SwaggerGenState IO) a }
  deriving (MonadReader Config, MonadState SwaggerGenState, Functor, Applicative, Monad)

generateSwaggerState :: Swagger -> SwaggerGenerator ()
generateSwaggerState sw = do
  globalSwaggerDefinitions (_swaggerDefinitions sw)
  generateRoutesState (_swaggerParameters sw) (_swaggerResponses sw) (_swaggerPaths sw) 

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
      _ <- paramDefinitions PathParam route meth pp
      qpTycon   <- paramDefinitions QueryParam route meth qp
      fpTycon   <- paramDefinitions FormParam route meth fp
      fipTycon  <- paramDefinitions FileParam route meth fip
      hpTycon   <- paramDefinitions HeaderParam route meth hp
      bodyTycon <- paramDefinitions BodyParam route meth bp
      let (sucs, fails, _headers) = groupOutputDefinitions globalResps opResponses
      succTycon <- outputDefinitions ApiOutput route meth sucs
      errTycon <- outputDefinitions ApiError route meth fails
      insertContract route meth qpTycon fpTycon fipTycon hpTycon bodyTycon succTycon errTycon
      pure (meth, pp)

insertRoute :: Route UnparsedPiece ->
               Route Ref           ->
               SwaggerGenerator ()
insertRoute k v =
  modify' (\sw -> sw { routeState = HM.insert k v (routeState sw) })


insertContract :: Route UnparsedPiece ->
                   Method ->
                  Maybe TypeConstructor ->
                  Maybe TypeConstructor ->
                  Maybe TypeConstructor ->
                  Maybe TypeConstructor ->
                  Maybe TypeConstructor ->
                  Maybe TypeConstructor ->
                  Maybe TypeConstructor ->
                  SwaggerGenerator ()
insertContract r m qp fp fip hp bdy suc err = do
  let cInfo = ContractInfo { queryParam  = qp
                           -- , pathParam   = ""
                           , formParam   = fp
                           , fileParam   = fip
                           -- , cookieParam = ""
                           , headerParam = hp
                           , requestBody = maybe [] (\b -> [("json", b)]) bdy
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
                Nothing          -> error "Panic: route param not found"
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
  _ <- schemaDefinitions (Global []) defs
  pure ()

outputDefinitions :: ResponseType -> Route UnparsedPiece -> Method ->
                      [(HttpStatusCode, Maybe (SW.Referenced Schema))] -> SwaggerGenerator (Maybe TypeConstructor)
outputDefinitions _ _ _  []    = pure Nothing
outputDefinitions rType r m rs = Just <$> do
  tyRefs <- mapM (uncurry outputDefinition) rs
  let tyDef = sumType (responseTypeName rType)
                      (map (\(code, refT) ->
                              (responseDataConName rType code, [refT])) tyRefs)
  cty <- insertOutputType rType r m tyDef
  pure (getTypeConstructor cty)  

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
            refT <- schemaDefinition (localProv r m) n sc
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
  in  ( queryParams  inlinePars
      , formParams   inlinePars
      , pathParams   inlinePars
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

paramDefinitions :: ParamType -> Route UnparsedPiece -> Method -> [Param] -> SwaggerGenerator (Maybe TypeConstructor)
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
      cty <- insertParamType pType rt method tyDef
      -- mapM_ updateInstance pars  
      pure (pure (getTypeConstructor cty))

  -- where -- updateInstance = const (pure ())

paramTypeName :: ParamType -> T.Text
paramTypeName = T.pack . show

responseTypeName :: ResponseType -> T.Text
responseTypeName = T.pack . show

responseDataConName :: ResponseType -> Int -> T.Text
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
                     schemaDefinition prov parName sch
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
               Just _           -> error "Panic: unexpected string"
               Nothing ->
                 case _paramSchemaEnum parSch of
                   Nothing    -> inline Text
                   Just enums -> enumDefinition enums
           Just SwaggerNumber ->
             case _paramSchemaFormat parSch of
               Just "float"  -> inline Float
               Just "double" -> inline Double
               _ -> error "TODO: swagger number"
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
            Nothing    -> error "Panic: non string in enums"

        arrayDefinition = undefined
        inline = pure . Inline

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

schemaDefinitions :: Provenance -> Definitions Schema -> SwaggerGenerator [Ref]
schemaDefinitions prov =
  OHM.foldlWithKey' (\s k sc -> do
                        xs <- s
                        x <- schemaDefinition prov k sc
                        pure (x : xs)
                    ) (pure [])

schemaDefinition :: Provenance -> DefinitionName -> Schema -> SwaggerGenerator Ref
schemaDefinition prov def sch = do
  let paramSchema = _schemaParamSchema sch
  case (_paramSchemaType paramSchema) of
      Just SwaggerObject -> customTypeDefinition (_schemaProperties sch)
      -- NOTE: Array is handled in both schemaDefinition and paramDefinition
      --       The invariant check is not being handled now (certain arrays are special)
      Just SwaggerArray  ->
        arrayDefinition (_paramSchemaItems paramSchema)
      _                  -> 
        paramSchemaDefinition prov def paramSchema

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
           rcty <- paramSchemaDefinition (extendProvenance def prov) (def <> "ArrayContents") ipar
           let unDefault = case rcty of
                             Inline (Default cty) -> cty
                             _                    -> rcty
           let arTy = case mfmt of
                 Nothing -> Array rcty
                 Just CollectionCSV -> DelimitedCollection Comma unDefault
                 Just CollectionSSV -> DelimitedCollection Space unDefault
                 Just CollectionTSV -> DelimitedCollection SlashT unDefault
                 Just CollectionPipes -> DelimitedCollection Pipe unDefault
                 Just CollectionMulti -> MultiSet unDefault
           pure (Inline arTy)
            
         objectArrayDefinition rsc = do
           case rsc of
             SW.Inline sc -> do
               rty <- schemaDefinition (extendProvenance def prov) (def <> "ArrayContents") sc
               pure (Inline (Array rty))
             SW.Ref (Reference n) -> do
               rty <- lookupGlobalDefinition n
               pure (Inline (Array rty))

         tupleDefinition rscs = do
           rtys <- mapM (\(i, rsc) -> case rsc of
                           SW.Inline sc -> schemaDefinition (extendProvenance def prov) (def <> "TupleContents" <> T.pack (show i)) sc
                           SW.Ref (Reference n) -> lookupGlobalDefinition n
                       )
                       (zip ([0 .. ] :: [Int]) rscs)
           pure (Inline (Tuple rtys))
                  
         schemaField k rsc = case rsc of
           SW.Ref (Reference n) -> do
             lookupGlobalDefinition n
           SW.Inline s -> do
             schemaDefinition (extendProvenance k prov) k s
         
referenceOf :: Provenance -> DefinitionName -> SwaggerHaskType -> Ref
referenceOf prv defn (Object ct) = Ref prv defn (getTypeConstructor ct)
referenceOf _ _ (Primitive ty)   = Inline ty

extendProvenance :: DefinitionName -> Provenance -> Provenance
extendProvenance n (Global ks)    = Global (ks ++ [n])
extendProvenance n (Local r m ks) = Local r m (ks ++ [n])
extendProvenance _ p = p

lookupGlobalDefinition :: DefinitionName -> SwaggerGenerator Ref
lookupGlobalDefinition defn = do
  mcTy <- gets (lookupDefinition defn . typeState)
  case mcTy of
    Nothing  -> error "Panic: name not found"
    Just cty -> pure (referenceOf globalProv defn (Object cty))

  where lookupDefinition defn' tyDefs =
          customHaskType <$> HM.lookup (Definition globalProv defn') tyDefs

globalProv :: Provenance
globalProv = Global []

localProv :: Route UnparsedPiece -> Method -> Provenance
localProv r m = Local r m []

routeLocalProv :: Route UnparsedPiece -> Provenance
routeLocalProv r = RouteLocal r []

insertOutputType :: ResponseType -> Route UnparsedPiece -> Method -> TypeDefinition -> SwaggerGenerator CustomType
insertOutputType rt rte meth tyDef = do
  customHaskType <$> insertWithTypeMeta (ResponseType rt rte meth) tyDef

insertParamType :: ParamType -> Route UnparsedPiece -> Method -> TypeDefinition -> SwaggerGenerator CustomType
insertParamType pt rt meth tyDef = do
  customHaskType <$> insertWithTypeMeta (ParamType pt rt meth) tyDef

insertDefinition :: Provenance -> DefinitionName -> TypeDefinition -> SwaggerGenerator Ref
insertDefinition prov def tyDef = do
  val <- insertWithTypeMeta (Definition prov def) tyDef
  pure (referenceOf prov def (Object (customHaskType val)))

insertWithTypeMeta :: TypeMeta -> TypeDefinition -> SwaggerGenerator TypeDefinition
insertWithTypeMeta tyMeta = go 0

  where go ct tyDef = do
          tyState <- gets typeState
          let clss = HM.foldl' (\cs v ->
                                   case checkNameClash v tyDef of
                                     []  -> cs        
                                     cls -> cls ++ cs
                               ) [] tyState
          case clss of
            [] -> insertDef tyDef
            _  -> go (ct + 1) (resolveClashes ct clss tyDef)
                                                  
        insertDef val = do
          let key = tyMeta
          modify' (\sws -> sws { typeState   = HM.insert key val
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

resolveClashes :: Counter -> [TypeClash] -> TypeDefinition -> TypeDefinition
resolveClashes ct cs tyDef =
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
  let pieces = splitOn '/' fp
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
parsePathParam piece =
  -- NOTE: need a parser to properly parse this out
  [ DynParamPiece (T.tail (T.init piece)) ]

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
