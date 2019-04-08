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

data Ref a = Inline a
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
data PathParamPiece = DynParamPiece T.Text
                    | StaticParamPiece T.Text
                    deriving (Show, Eq, Generic)

data SwaggerHaskType
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
  | Array (Ref SwaggerHaskType)
  | Tuple [Ref SwaggerHaskType ]
  | MultiSet (Ref SwaggerHaskType)
  | DelimitedCollection Delimiter (Ref SwaggerHaskType)
  | File
  | Null
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
                             [DataConstructor SwaggerHaskType]
                deriving (Show, Eq, Generic)

data Provenance = Global [T.Text]
                | Local  (Route UnparsedPiece) Method [T.Text]
                deriving (Show, Eq, Generic)

type TypeConstructor     = T.Text
type DataConstructorName = T.Text
type KeyName             = T.Text
type RecordName          = T.Text

data DataConstructor a = DataConstructor DataConstructorName [ (Maybe RecordName, Ref a) ]
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
                                       , apiContract :: ContractState
                                       , errors      :: [ErrorMessage]
                                       } deriving (Show, Eq, Generic)

type TypeState      = HM.HashMap TypeMeta TypeDefinition

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
                                    , requestBody  :: [ty]
                                    , queryParam   :: ty
                                    , pathParam    :: ty
                                    , formParam    :: ty
                                    , fileParam    :: ty
                                    , cookieParam  :: ty
                                    , headerParam  :: ty
                                    , apiOutput    :: ty
                                    , apiError     :: ty
                                    , headerOut    :: ty
                                    , cookieOut    :: ty
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
          updState GET (_pathItemGet pItem)
          updState PUT (_pathItemPut pItem)
          updState POST (_pathItemPost pItem)
          updState DELETE (_pathItemDelete pItem)
          updState OPTIONS (_pathItemOptions pItem)
          updState PATCH (_pathItemPatch pItem)                    

generateRouteMethodState :: Definitions Param -> Definitions Response -> [SW.Referenced Param] -> Route UnparsedPiece -> Method -> Maybe Operation -> SwaggerGenerator ()
generateRouteMethodState globalParams globalResps routeRefs route meth mOp =
  case mOp of
    Nothing -> pure ()
    Just op -> do
      let opParams    = overrideParams routeRefs (_operationParameters op)
                        -- TODO: ResponsesDefault ignored.
          opResponses = _operationResponses op
      let (pp, qp, fp, fip, hp, bp) = groupParamDefinitions globalParams routeRefs
      -- parseRouteType route meth routeRefs
      _ <- paramDefinitions PathParam route meth pp
      qpTycon <- paramDefinitions QueryParam route meth qp
      fpTycon <- paramDefinitions FormParam route meth fp
      fipTycon <- paramDefinitions FileParam route meth fip
      hpTycon <- paramDefinitions HeaderParam route meth hp
      bodyTycon <- paramDefinitions BodyParam route meth bp
      let (sucs, fails, _headers) = groupOutputDefinitions globalResps opResponses
      succTycon <- outputDefinitions ApiOutput route meth sucs
      errTycon <- outputDefinitions ApiError route meth sucs
      insertContract route meth qpTycon fpTycon fipTycon hpTycon bodyTycon succTycon errTycon
      pure ()

insertContract :: Route UnparsedPiece -> Method ->
                  TypeConstructor -> TypeConstructor -> TypeConstructor -> TypeConstructor ->
                  TypeConstructor -> TypeConstructor -> TypeConstructor -> SwaggerGenerator ()
insertContract r m qp fp fip hp bdy succ err = do
  let cInfo = ContractInfo { queryParam  = qp
                           , pathParam   = ""
                           , formParam   = fp
                           , fileParam   = fip
                           , cookieParam = ""
                           , headerParam = hp
                           , requestBody = [bdy]
                           , apiOutput   = succ
                           , apiError    = err
                           , contentTypes = []
                           , cookieOut   = ""
                           }
  modify' (\s -> s { apiContract = HM.insert (r, m) cInfo (apiContract s) })
      
parseRouteType :: Route UnparsedPiece -> [SW.Referenced Param] ->
                   SwaggerGenerator ()
parseRouteType (Route pieces) refs = undefined
{-  
  Route <$> (mapM (parseRoutePiece refs) pieces)
  where parseRoutePiece refs (Static t)    = pure (Static t)
        parseRoutePiece refs (Dynamic par) =
          let pparamPieces = parsePathParam par
              pDynParamPieces = filter isDynParamPiece pparamPieces
              mParamPieceRef  = 
-}              

-- mkRouteDefinition :: [(UnparsedPiece, Referenced Param)] -> 
                
overrideParams :: [SW.Referenced Param] -> [SW.Referenced Param] -> [SW.Referenced Param]
overrideParams routeRefs methRefs =
  L.union methRefs routeRefs

globalSwaggerDefinitions :: Definitions Schema -> SwaggerGenerator ()
globalSwaggerDefinitions defs = do
  schemaDefinitions (Global []) defs
  pure ()

outputDefinitions :: ResponseType -> Route UnparsedPiece -> Method ->
                      [(HttpStatusCode, Maybe (SW.Referenced Schema))] -> SwaggerGenerator TypeConstructor
outputDefinitions rType r m rs = do
  tyRefs <- mapM (uncurry outputDefinition) rs
  let tyDef = sumType (responseTypeName rType)
                      (map (\(code, refT) ->
                              (responseDataConName rType code, [refT])) tyRefs)
  cty <- insertOutputType rType r m tyDef
  pure (getTypeConstructor cty)  

  where outputDefinition code mrsch = case mrsch of
          Nothing -> pure (code, Inline Null)
          Just (SW.Ref (Reference name)) -> do
            refT <- lookupGlobalDefinition name
            pure (code, refT)
          Just (SW.Inline sc)            -> do
            -- TODO: a name is being made up here
            --       this name should not be clashing with any key
            --       To handle this, verify that all keys under this route/meth
            --       do not have this name
            let name = responseName code
            refT <- schemaDefinition (localProv r m) name sc
            pure (code, refT)
        responseName code = "Response" <> T.pack (show code)

groupOutputDefinitions :: Definitions Response -> Responses ->
                           ( [(HttpStatusCode, Maybe (SW.Referenced Schema))] -- ApiSuccess
                           , [(HttpStatusCode, Maybe (SW.Referenced Schema))] -- ApiError
                           , [(HttpStatusCode, HeaderName, Header)]                           -- all headers of
                                                                              -- success & failure
                           )
-- TODO: handling of default responses                           
groupOutputDefinitions globResponses responses =
  let inlineResps =
        map (\(code, rres) -> case rres of
                      SW.Ref (Reference n) -> 
                        case OHM.lookup n globResponses of
                          Nothing -> error "Panic: global response not found"
                          Just p  -> (code, p)
                      SW.Inline p -> (code, p)
                  ) $ OHM.toList (_responsesResponses responses)
      headers     = concatMap (\(code, resp) ->
                                 map (\(hn, hdr) -> (code, hn, hdr)) (OHM.toList (_responseHeaders resp))) inlineResps
      successes   = map (fmap _responseSchema) $ filter (isSuccess . fst) inlineResps
      failures    = map (fmap _responseSchema) $ filter (not . isSuccess . fst) inlineResps
      isSuccess c = c >= 200 && c < 300
  in  (successes, failures, headers)

  

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
    ParamBody parOther -> True
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

paramDefinitions :: ParamType -> Route UnparsedPiece -> Method -> [Param] -> SwaggerGenerator TypeConstructor
paramDefinitions pType route method params = do
  ptys <- mapM (paramDefinition route method) params
  let tyDef = productType (paramTypeName pType)
                          (map (\pty -> ( paramName pty
                                       , paramType pty
                                       )
                               ) ptys
                          )
  cty <- insertParamType pType route method tyDef
  -- mapM_ updateInstance pars  
  pure (getTypeConstructor cty)

  -- where -- updateInstance = const (pure ())

paramTypeName :: ParamType -> T.Text
paramTypeName = T.pack . show

responseTypeName :: ResponseType -> T.Text
responseTypeName = T.pack . show

responseDataConName :: ResponseType -> Int -> T.Text
responseDataConName rt s =
  responseTypeName rt <>
  T.pack (show s)

data ParamTypeInfo = ParamTypeInfo { paramType       :: Ref SwaggerHaskType
                                   , paramName       :: DefinitionName
                                   , isParamRequired :: Maybe Bool
                                   } deriving (Show, Eq, Generic)

paramDefinition :: Route UnparsedPiece -> Method -> Param -> SwaggerGenerator ParamTypeInfo
paramDefinition r m par = do
  let name = _paramName par
      req  = _paramRequired par
  paramTypeRef <- case _paramSchema par of
                   ParamBody (SW.Inline schema) -> do
                     schemaDefinition (localProv r m) name schema
                   ParamBody (SW.Ref (Reference name)) ->
                     lookupGlobalDefinition name
                   ParamOther pOth           -> 
                     paramSchemaDefinition (localProv r m) name (_paramOtherSchemaParamSchema pOth)
  pure (ParamTypeInfo paramTypeRef name req)

paramSchemaDefinition :: Provenance -> DefinitionName -> ParamSchema t -> SwaggerGenerator (Ref SwaggerHaskType)
paramSchemaDefinition prov def parSch = go
  where go =
         case (_paramSchemaType parSch) of
           Just SwaggerString -> 
             case _paramSchemaFormat parSch of
               Just "date"      -> inline Date
               Just "date-time" -> inline DateTime
               Just "password"  -> inline Password
               Just "byte"      -> inline Byte
               Just "binary"    -> inline Binary
               Nothing ->
                 case _paramSchemaEnum parSch of
                   Nothing    -> inline Text
                   Just enums -> enumDefinition enums
           Just SwaggerNumber ->
             case _paramSchemaFormat parSch of
               Just "float"  -> inline Float
               Just "double" -> inline Double
               _ -> undefined
           Just SwaggerInteger -> 
             case _paramSchemaFormat parSch of
               Just "int32" -> inline Int32
               Just "int64" -> inline Int64
               _ -> inline Int
           Just SwaggerBoolean -> inline Bool
           Just SwaggerArray -> arrayDefinition (_paramSchemaItems parSch)
           Just SwaggerNull -> inline Null
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

        arrayDefinition mitems = case mitems of
          Just (SwaggerItemsPrimitive mfmt ipar) -> primitiveArrayDefinition mfmt ipar
          Just (SwaggerItemsObject rsc) -> objectArrayDefinition rsc
          Just (SwaggerItemsArray rscs) -> tupleDefinition rscs
          Nothing -> error "TODO: default for array"

        primitiveArrayDefinition _mfmt _ipar = undefined
        objectArrayDefinition = undefined
        tupleDefinition = undefined
        
        inline = pure . Inline

sumType :: T.Text -> [(T.Text, [Ref SwaggerHaskType])] -> TypeDefinition
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

productType :: T.Text -> [(T.Text, Ref SwaggerHaskType)] -> TypeDefinition
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

schemaDefinitions :: Provenance -> Definitions Schema -> SwaggerGenerator [Ref SwaggerHaskType]
schemaDefinitions prov =
  OHM.foldlWithKey' (\s k sc -> do
                        xs <- s
                        x <- schemaDefinition prov k sc
                        pure (x : xs)
                    ) (pure [])

schemaDefinition :: Provenance -> DefinitionName -> Schema -> SwaggerGenerator (Ref SwaggerHaskType)
schemaDefinition prov def sch = do
  let paramSchema = _schemaParamSchema sch
  case (_paramSchemaType paramSchema) of
      Just SwaggerObject -> customTypeDefinition (_schemaProperties sch)
      -- NOTE: Array is handled in both schemaDefinition and paramDefinition
      --       The invariant check is not being handled now (certain arrays are special)
      Just SwaggerArray  -> arrayDefinition (_paramSchemaItems paramSchema)
      _                  -> paramSchemaDefinition prov def paramSchema

   where customTypeDefinition props = do
           --TODO: Additional properties not handled
           fields <- OHM.foldlWithKey' (\s k rsc -> do
                                          xs <- s
                                          x <- schemaField k rsc
                                          pure ((k, x) : xs)
                                      ) (pure []) props
           let tyDef = productType def fields
           insertDefinition prov def tyDef
           -- pure (referenceOf prov def (Object (customHaskType finalTyDef)))
           
         arrayDefinition = undefined         
         schemaField k rsc = case rsc of
           SW.Ref (Reference name) -> do
             lookupGlobalDefinition name
           SW.Inline s -> do
             schemaDefinition (extendProvenance k prov) k s
         
referenceOf :: Provenance -> DefinitionName -> SwaggerHaskType -> Ref SwaggerHaskType
referenceOf prv defn (Object ct) = Ref prv defn (getTypeConstructor ct)
-- TODO: Lists are to be handled differently
referenceOf _ _ ty               = Inline ty

extendProvenance :: DefinitionName -> Provenance -> Provenance
extendProvenance n (Global ks)    = Global (ks ++ [n])
extendProvenance n (Local r m ks) = Local r m (ks ++ [n])

lookupGlobalDefinition :: DefinitionName -> SwaggerGenerator (Ref SwaggerHaskType)
lookupGlobalDefinition defn = do
  mcTy <- gets (lookupDefinition defn . typeState)
  case mcTy of
    Nothing  -> error "Panic: name not found"
    Just cty -> pure (referenceOf globalProv defn (Object cty))

  where lookupDefinition defn tyDefs =
          customHaskType <$> HM.lookup (Definition globalProv defn) tyDefs

globalProv :: Provenance
globalProv = Global []

localProv :: Route UnparsedPiece -> Method -> Provenance
localProv r m = Local r m []

insertOutputType :: ResponseType -> Route UnparsedPiece -> Method -> TypeDefinition -> SwaggerGenerator CustomType
insertOutputType rt route meth tyDef = do
  let prov = ResponseType rt route meth
  customHaskType <$> insertWithTypeMeta (ResponseType rt route meth) tyDef

insertParamType :: ParamType -> Route UnparsedPiece -> Method -> TypeDefinition -> SwaggerGenerator CustomType
insertParamType pt route meth tyDef = do
  let prov = ParamType pt route meth
  customHaskType <$> insertWithTypeMeta (ParamType pt route meth) tyDef

insertDefinition :: Provenance -> DefinitionName -> TypeDefinition -> SwaggerGenerator (Ref SwaggerHaskType)
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

  where typeNameClashResolution ct n = n <> "_Ty"  <> T.pack (show ct)
        ctorNameClashResolution ct n = n <> "_Con" <> T.pack (show ct)
        
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
      
parsePathParam :: UnparsedPiece -> [PathParamPiece]
parsePathParam piece =
  [ DynParamPiece (T.tail (T.init piece)) ]

isDynParamPiece :: PathParamPiece -> Bool
isDynParamPiece (DynParamPiece _) = True
isDynParamPiece _                 = False

splitOn :: Char -> String -> [String]
splitOn _ ""  =  []
splitOn c s   =  cons (case break (== c) s of
                          (l, s') -> (l, case s' of
                                          []      -> []
                                          _:s''   -> splitOn c s''))
  where
    cons ~(h, t) = h : t  

