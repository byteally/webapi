{-# LANGUAGE DuplicateRecordFields              #-}
{-# LANGUAGE OverloadedStrings                  #-}
{-# LANGUAGE DerivingStrategies                 #-}
{-# LANGUAGE MultiParamTypeClasses              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving         #-}
{-# LANGUAGE FlexibleContexts                   #-}
{-# LANGUAGE ViewPatterns                       #-}
{-# LANGUAGE NamedFieldPuns                     #-}
{-# LANGUAGE StrictData                         #-}

module WebApi.OpenAPI.Codegen where

import           Control.Monad.IO.Class ( MonadIO )
import           Control.Monad.State
import qualified Data.Char as C
import           Data.Coerce
import qualified Data.HashMap.Strict.InsOrd as HMI
import qualified Data.HashSet as HS
import           Data.List ( foldl', foldl1' )
import           Data.OpenApi
import           Data.String
import           Data.Text ( Text )
import qualified Data.Text as T
import qualified Data.Text.IO as T (writeFile)
import           Data.Vector ( Vector )
import qualified Data.Vector as V
import           GHC ( runGhc )
import           GHC.Paths (libdir)
import           GHC.SourceGen
import           GhcPlugins ( getDynFlags )
import           Lens.Micro ( lens )
import           Lens.Micro ( lens, (^.) )
import           Ormolu ( ormolu, defaultConfig, Config(cfgCheckIdempotence) )
import           Outputable ( ppr, showSDoc )
import           System.Directory
import           System.FilePath

import           Data.Maybe ( mapMaybe )
import           Network.HTTP.Types ( StdMethod )
import           WebApi.OpenAPI.Analysis ( Contract (..) )
import           WebApi.OpenAPI.Utils ( HttpApiInstance, getHeaderComponent, getSchemaComponent, getParamComponent )
import qualified WebApi.OpenAPI.Utils as WO

data TypeName =
  TypeName { originalName :: Text
           , haskellName :: Text
           }
  deriving (Show, Eq)

data FieldName =
  FieldName { haskellName :: Text
            , originalName :: Text
            }
  deriving (Show, Eq)

newtype ConstructorName =
  ConstructorName { getConstructorName :: Text }
  deriving (Show, Eq)

newtype OpenApiGenM m a =
  OpenApiGenM { runOpenApi :: StateT ModelGenState m a }
  deriving newtype (Functor, Applicative, Monad, MonadState ModelGenState, MonadIO)

runCodegen :: (Monad m) => OpenApi -> OpenApiGenM m a -> m a
runCodegen oa ma =
  evalStateT (runOpenApi ma) (defModelGenState comps)

  where
    comps = oa ^. components

defModelGenState :: Components -> ModelGenState
defModelGenState comps =
  ModelGenState { seenVars = mempty
                , imports = mempty
                , keywordsToAvoid = mempty
                , openApiComponents = comps
                }

data ModelGenState =
    ModelGenState { seenVars :: HS.HashSet Text
                  , imports :: HS.HashSet Text
                  , keywordsToAvoid :: HS.HashSet Text
                  , openApiComponents :: Components
                  -- , createdSums :: HashMap Text [Referenced Schema]
                  -- , jsonInstances :: Set Text
                  }

instance HasComponents ModelGenState Components where
  components = lens get0 set0
    where
      get0 ModelGenState { openApiComponents = c } = c
      set0 oa c = oa { openApiComponents = c }

data Type =
    Text
  | Double
  | Bool
  | Int
  | Unit
  | Vector Type
  | Tuple [Type]
  | Maybe Type
  | CustomDataType TypeName
  | InlineRec Fields
  | OneOf [ Type ]
  | ApiResponses [ (HttpStatusCode, Type) ]
  | ApiResponseHeaders [ (HttpStatusCode, Type) ]
  | ApiResponseCookies [ (HttpStatusCode, Type) ]
  | Untyped
  deriving (Show, Eq)

data DataType =
  DataType { typeName  :: TypeName
           , typeStyle :: DataTypeStyle
           } deriving (Show, Eq)

newtype Fields =
  Fields { getFields :: [ (FieldName, Type) ] }
  deriving (Show, Eq)

data DataTypeStyle =
    DataTypeStyleProduct DataTypeProduct
  | DataTypeStyleSum     DataTypeSum
  deriving (Show, Eq)

data DataTypeProduct =
    DataTypeProductRecord { fields :: Fields, constructorName :: ConstructorName }
  | DataTypeProductPlain { types :: [ Type ], constructorName :: ConstructorName }
  deriving (Show, Eq)

newtype DataTypeSum =
  DataTypeSum { sums :: [ DataTypeProduct ] }
  deriving (Show, Eq)

genModel ::
  ( MonadState ModelGenState m
  ) => Reference -> HS.HashSet HttpApiInstance -> m (Vector HsDecl')
genModel ref insts = do
  sch <- getSchemaComponent (Ref ref)
  types <- schemaToTypeDef ref sch
  V.concat <$> mapM (flip genHaskDefs insts) types

-- | Type definition corresponding to the Schema
schemaToTypeDef ::
  ( MonadState ModelGenState m
  ) => Reference -> Schema -> m [ DataType ]
schemaToTypeDef name sch@Schema { _schemaRequired, _schemaType = scTy, _schemaProperties } = do
  typeName@TypeName { haskellName } <- toTypeName (getReference name)
  conName <- toConstructorName haskellName
  case scTy of
    Just OpenApiObject -> do
      prod <- (\fields -> DataTypeProductRecord { fields, constructorName = conName }) <$>
              schemaPropsToProduct name _schemaRequired _schemaProperties
      pure [DataType { typeName, typeStyle = DataTypeStyleProduct prod }]
    Nothing            -> mixed typeName sch
    _                  -> pure []

  where

    -- TODO: only _schemaOneOf is handled.
    mixed typeName Schema { _schemaOneOf } =
      case _schemaOneOf of
        Just scs -> do
          sums <- mapM (schemaToType name False) scs
          sumsDefs <- mapM go1 scs

          let t = DataType { typeName, typeStyle = typSty }
              typSty = DataTypeStyleSum (DataTypeSum { sums = go0 name (zip sums [1 .. ]) })
          pure (t : concat sumsDefs)
        Nothing -> pure []

      where
        go0 name =
          map (\(variant, ix) -> DataTypeProductPlain { constructorName = ctorName ix
                                                      , types = pure variant
                                                      }
              )

          where
            ctorName ix = ConstructorName $ (getReference name) <> T.pack (show ix)

        go1 sch =
          case sch of
            Ref ref -> getSchemaComponent sch >>= schemaToTypeDef ref
            Inline sch -> pure []

-- | Type corresponding to the Schema
schemaToType ::
  ( MonadState ModelGenState m
  ) => Reference -> Bool -> Referenced Schema -> m Type
schemaToType root isReq refSch
  | not isReq = optMaybe <$> typ
  | otherwise = typ

  where
    optMaybe v@(Vector {}) = v
    optMaybe v@Unit        = v
    optMaybe v             = Maybe v

    typ = case refSch of
      r@(Ref ref) -> getSchemaComponent r >>= go (Just ref)
      Inline sch  -> go Nothing sch

    go refMay sch@Schema { _schemaType = scTy, _schemaProperties, _schemaItems } =
      case scTy of
        Just OpenApiString  -> pure Text
        Just OpenApiNumber  -> pure Double
        Just OpenApiInteger -> pure Int
        Just OpenApiBoolean -> pure Bool
        Just OpenApiNull    -> pure Unit
        Just OpenApiArray   -> arrayHaskType
        Just OpenApiObject  -> customHaskType
        Nothing             -> customHaskType

      where
        customHaskType =
          case refMay of
            Just ref -> CustomDataType <$> toTypeName (getReference ref)
            Nothing  -> inlineSchemaObjectToType root sch

        arrayHaskType = do
          case _schemaItems of
            Just (OpenApiItemsArray schs) ->
              Tuple <$> mapM (schemaToType root False) schs
            Just (OpenApiItemsObject sch) ->
              -- NOTE: we set isReq to True to avoid (Vector (Maybe .. ))
              Vector <$> (schemaToType root True) sch
            Nothing ->
              error "TODO: unhandled arrayHaskType"

inlineSchemaObjectToType ::
  ( MonadState ModelGenState m
  ) => Reference -> Schema -> m Type
inlineSchemaObjectToType root sch@Schema { _schemaRequired, _schemaType = scTy, _schemaProperties, _schemaOneOf } =
  case scTy of
    Nothing            -> mixedHaskType
    Just OpenApiObject ->
      InlineRec <$>
      schemaPropsToProduct root _schemaRequired _schemaProperties

  where
    mixedHaskType =
      case _schemaOneOf of
        Nothing     -> pure Untyped
        Just oneOfs -> OneOf <$> mapM (schemaToType root False) oneOfs

schemaPropsToProduct ::
  ( MonadState ModelGenState m
  ) => Reference -> [Text] -> HMI.InsOrdHashMap Text (Referenced Schema) -> m Fields
schemaPropsToProduct root reqParams =
  fmap coerce . HMI.foldrWithKey go (pure mempty)

  where
    go k v a = do
      fld <- toFieldName k
      typ <- schemaToType root (k `elem` reqParams) v
      (:) <$> pure (fld, typ) <*> a

toFieldName :: (MonadState ModelGenState m) => Text -> m FieldName
toFieldName fld =
  pure $ FieldName { originalName = fld
                   , haskellName = sanitised
                   }

  where
    sanitised =
      let fld0 = removeUnsupportedSymbols fld
      in case fld0 `elem` keywords of
        True -> fld0 <> "_"
        _    -> fld0

keywords :: [ Text ]
keywords = [ "class"
           , "type"
           ]

removeUnsupportedSymbols :: Text -> Text
removeUnsupportedSymbols =
  T.filter (\x -> not (x `elem` unsupportedSymbols))

toTypeName ::
  ( MonadState ModelGenState m
  ) => Text -> m TypeName
toTypeName t =
  pure (TypeName { originalName = t
                 , haskellName = headToCap (removeUnsupportedSymbols t)
                 }
       )

  where
    headToCap (T.uncons -> Just (x, xs)) = C.toUpper x `T.cons` xs
    headToCap x                          = x

toConstructorName :: (MonadState ModelGenState m) => Text -> m ConstructorName
toConstructorName t = pure (ConstructorName { getConstructorName = t })

genHaskDefs :: (MonadState ModelGenState m) => DataType -> HS.HashSet HttpApiInstance -> m (Vector HsDecl')
genHaskDefs typ insts = do
  tyDef <- genHaskTypeDef typ
  instDefs <- HS.foldr (\a acc -> (V.++) <$> genHaskTypeInstDef typ a <*> acc) (pure mempty) insts
  pure (tyDef `V.cons` instDefs)

genHaskTypeDef ::
  ( MonadState ModelGenState m
  ) => DataType -> m HsDecl'
genHaskTypeDef DataType { typeName, typeStyle } = do
  cons <- consM
  pure (data' tyN [] cons [])

  where
    tyN = case typeName of
      TypeName { haskellName = T.unpack -> s } ->
        fromString s
    consM = case typeStyle of
      DataTypeStyleProduct p -> pure <$> prod p
      DataTypeStyleSum (DataTypeSum { sums }) -> mapM prod sums

    prod (DataTypeProductRecord { fields = Fields { getFields = flds}
                                , constructorName = ConstructorName (T.unpack -> con)
                                }) =
      recordCon (fromString con) <$> mapM (uncurry mkRecField) flds
    prod (DataTypeProductPlain { types = typs, constructorName = ConstructorName (T.unpack -> con)}) =
      prefixCon (fromString con) <$> mapM mkPlainField typs

    mkRecField FieldName { haskellName = T.unpack -> fldName } typ = do
      typ0 <- genHaskType typ
      pure (fromString fldName, field typ0)

    mkPlainField typ = do
      typ0 <- genHaskType typ
      pure (field typ0)

genHaskType ::
  ( MonadState ModelGenState m
  ) => Type -> m HsType'
genHaskType Text =
  pure $ var "Text"
genHaskType Double =
  pure $ var "Double"
genHaskType Bool =
  pure $ var "Bool"
genHaskType Int =
  pure $ var "Int"
genHaskType Unit =
  pure $ var "()"
genHaskType (Vector t) =
  (var "Vector" @@) <$> genHaskType t
genHaskType (Maybe t) =
  (var "Maybe" @@) <$> genHaskType t
genHaskType (CustomDataType (TypeName {haskellName = T.unpack -> hsName})) =
  pure $ var (fromString hsName)
genHaskType (InlineRec sty) =
  genInlineRec sty
genHaskType (OneOf tys) =
  genOneOf tys
genHaskType (ApiResponses tys) =
  genResponses "Responses" tys
genHaskType (ApiResponseHeaders tys) =
  genResponses "ResponseHeaders" tys
genHaskType (ApiResponseCookies tys) =
  genResponses "ResponseCookies" tys
genHaskType Untyped =
  pure $ var "Untyped"

genInlineRec ::
  ( MonadState ModelGenState m
  ) => Fields -> m HsType'
genInlineRec =
  fmap (var "Rec" @@ ) . go . coerce
  where
    go =
      fmap listPromotedTy .
      mapM (\(FieldName { originalName },typ)-> do
               hsTy <- genHaskType typ
               pure (tuplePromotedTy [stringTy (T.unpack originalName), hsTy])
           )

genOneOf ::
  ( MonadState ModelGenState m
  ) => [ Type ] -> m HsType'
genOneOf =
  fmap (var "OneOf" @@ ) . go
  where
    go =
      fmap listPromotedTy .
      mapM genHaskType

genResponses ::
  ( MonadState ModelGenState m
  ) => RdrNameStr -> [ (HttpStatusCode, Type) ] -> m HsType'
genResponses str =
  fmap (var str @@ ) . go
  where
    go =
      fmap listPromotedTy .
      mapM (genHaskType . snd)

genHaskTypeInstDef ::
  ( MonadState ModelGenState m
  ) => DataType -> HttpApiInstance -> m (Vector HsDecl')
genHaskTypeInstDef typ inst =
  case inst of
    WO.JSON -> genJSONHaskTypeInstDef typ
    WO.Param -> genParamHaskTypeInstDef typ
    WO.Header -> genHeaderHaskTypeInstDef typ

genJSONHaskTypeInstDef ::
  ( MonadState ModelGenState m
  ) => DataType -> m (Vector HsDecl')
genJSONHaskTypeInstDef DataType { typeName, typeStyle } =
  case typeStyle of
    DataTypeStyleSum (DataTypeSum { sums }) -> pure (genJSONSum typeName sums)
    DataTypeStyleProduct p                  -> pure (genJSONProd typeName p)

  where
    genJSONSum TypeName { haskellName = T.unpack -> fromString -> hsName } sums =
      V.fromList
      [ instance' (var "FromJSON" @@ var hsName) [funBind "parseJSON" (fromJSONExpr sums)]
      , instance' (var "ToJSON" @@ var hsName) [funBinds "toJSON" (toJSONExpr sums)]
      ]

      where
        -- Note: Sums are handled in a very specific format
        --       like data Foo = Foo1 Int | Foo2 Int and so on
        toJSONExpr =
          map (\DataTypeProductPlain { constructorName = ConstructorName (T.unpack -> fromString -> ctorName) } ->
                        match [conP ctorName [bvar "x"]] (var "toJSON" @@ var "x")
              )
        fromJSONExpr =
          match [bvar "v"] . rhsExpr

          where
            rhsExpr =
              foldl1' (\acc a -> op a "<|>" acc) .
              map (\DataTypeProductPlain { constructorName = ConstructorName (T.unpack -> fromString -> ctorName) } ->
                                 op (var ctorName) "<$>" (var "parseJSON" @@ var "v")
                  )


    genJSONProd TypeName { haskellName = T.unpack -> fromString -> hsName }
                DataTypeProductRecord { constructorName = ConstructorName (T.unpack -> ctorName), fields = getFields -> flds } =
      V.fromList
      [ instance' (var "FromJSON" @@ var hsName) [valBind "parseJSON" fromJSONExpr]
      , instance' (var "ToJSON" @@ var hsName) [funBind "toJSON" toJSONExpr]
      ]

      where

        fromJSONExpr =
          let varName = "v"
          in op (var "withObject" @@ string ctorName)
                "$"
                (lambda [conP_ varName] (fieldExpr varName flds))
          where
            fieldExpr varName =
              foldl' (\acc a -> op acc "<*>" a) (var (fromString ctorName)) . map (uncurry fldExp)

            fldExp FieldName { originalName = T.unpack -> on } typ =
              case typ of
                Maybe {}  -> op (var "v") ".:?" (string on)
                Vector {} -> op (op (var "v") ".:?" (string on)) ".!=" (var "V.empty")
                _         -> op (var "v") ".:" (string on)

            apCons (x : xs) = op (var (fromString ctorName)) "<$>" x : xs
            apCons []       = error "Panic: impossible case: flds are empty @apCons"

        toJSONExpr =
          match [conP (fromString ctorName) fldBinds]
                (var "object" @@ list (map (uncurry fldExp) flds))

          where
            fldBinds = map (bvar . fromString . T.unpack . (haskellName :: FieldName -> Text) . fst) flds

            fldExp FieldName { originalName = T.unpack -> on, haskellName = T.unpack -> fldName } typ =
              op (string on) ".=" (var (fromString fldName))

genParamHaskTypeInstDef :: ( Monad m ) => DataType -> m (Vector HsDecl')
genParamHaskTypeInstDef DataType { typeName, typeStyle } =
  case typeStyle of
    DataTypeStyleSum sums -> pure mempty
    DataTypeStyleProduct p -> pure mempty

genHeaderHaskTypeInstDef :: ( Monad m ) => DataType -> m (Vector HsDecl')
genHeaderHaskTypeInstDef DataType { typeName, typeStyle } =
  case typeStyle of
    DataTypeStyleSum sums -> pure mempty
    DataTypeStyleProduct p -> pure mempty


{-
webApiInstanceDef :: ( Monad m ) => Text -> [ Contract ] -> m (Vector HsDecl')
webApiInstanceDef name cts = do
  mapM contractInstanceDef cts
-}

contractInstanceDef :: HsType' -> Contract -> m HsDecl'
contractInstanceDef hsRouteTy Contract { route, method, operation } = do
  ResponseTypes { apiOutput, apiError, headerOut {-, cookieOut-} } <- responsesToTypes operation
  instance' (var "ApiContract" @@
             var (fromString apiName) @@
             hsMethTy @@
             hsRouteTy
            ) body
  where
    hsMethTy = toHaskMethod method
    apiName = "TODO_FILLME"
    body =
      mapMaybe (uncurry (mkTyFam hsMethTy hsRouteTy))
               [ ("QueryParam", queryParamDef operation)
               , ("HeaderIn", headerInDef operation)
               , ("CookieIn", cookieInDef operation)
               , ("RequestBody", requestBodyDef operation)
               , ("ApiOut", pure apiOutput)
               , ("ApiErr", pure apiError)
               , ("HeaderOut", pure headerOut)
               -- , ("CookieOut", pure cookieOut)
               ]

mkTyFam ::
  ( MonadState ModelGenState m
  ) => HsType' -> HsType' -> Text -> m Type -> m HsDecl'
mkTyFam hsMethTy hsRouteTy (T.unpack -> tyFamName) mty = do
  hsTy <- mty >>= genHaskType
  pure (tyFamInst (fromString tyFamName) [hsMethTy, hsRouteTy] hsTy )

-- NOTE: Invariant: all params are Inlined by this time.
byParam :: ParamLocation -> [ Referenced Param ] -> [ Param ]
byParam loc =
  mapMaybe go
  where
    go (Inline p@(Param { _paramIn }))
      | _paramIn == loc = Just p
      | otherwise       = Nothing
    go _ = error "Panic: Impossible case @byParam"

queryParamDef ::
  ( MonadState ModelGenState m
  ) => Operation -> m Type
queryParamDef Operation { _operationParameters = byParam ParamQuery -> ps } = do
  InlineRec <$> paramsToProduct ps

headerInDef ::
  ( MonadState ModelGenState m
  ) =>  Operation -> m Type
headerInDef Operation { _operationParameters = byParam ParamHeader -> ps } = do
  InlineRec <$> paramsToProduct ps

cookieInDef ::
  ( MonadState ModelGenState m
  ) =>  Operation -> m Type
cookieInDef Operation { _operationParameters = byParam ParamCookie -> ps } = do
  InlineRec <$> paramsToProduct ps

paramsToProduct ::
  ( MonadState ModelGenState m
  ) => [ Param ] -> m Fields
paramsToProduct =
  fmap coerce . mapM go

  where
    go Param { _paramSchema, _paramName, _paramRequired } = do
      schTy <- case _paramSchema of
                 Nothing  -> pure Untyped
                 Just sch -> schemaToType (Reference _paramName) (isRequired _paramRequired) sch
      fld <- toFieldName _paramName
      pure (fld, schTy)

isRequired :: Maybe Bool
isRequired = maybe False id

requestBodyDef ::
  ( MonadState ModelGenState m
  ) =>  Operation -> m Type
requestBodyDef Operation {} = undefined

data ResponseTypes =
  ResponseTypes { apiOutput :: Type
                , apiError :: Type
                , headerOut :: Type
                -- , cookieOut :: Type
                } deriving (Show, Eq)

responsesToTypes :: Operation -> m ResponseTypes
responsesToTypes Operation { _operationResponses = Responses { _responsesDefault = def
                                                             , _responsesResponses = resps
                                                             }
                           } = do
  
  where
    headers =
      HMI.foldrWithKey go (pure []) resps

      where
        go k v a = do
          v0 <- responseHeaderDef v
          ((k, v0) :) <$> a

isApiSuccess :: HttpStatusCode -> Bool
isApiSuccess i
 | i >= 200 && i < 300 = True
 | otherwise           = False

byApiSuccess :: HMI.InsOrdHashMap HttpStatusCode (Referenced Response) -> HMI.InsOrdHashMap HttpStatusCode (Referenced Response)
byApiSuccess = HMI.filterWithKey (\k _ -> isApiSuccess k)

byApiFailure :: HMI.InsOrdHashMap HttpStatusCode (Referenced Response) -> HMI.InsOrdHashMap HttpStatusCode (Referenced Response)
byApiFailure = HMI.filterWithKey (\k _ -> not (isApiSuccess k))

headerOutDef ::
  ( MonadState ModelGenState m
  ) => Operation -> m Type
headerOutDef Operation { _operationResponses = Responses { _responsesDefault = def
                                                         , _responsesResponses = resps
                                                         }
                       } = do
  

cookieOutDef ::
  ( MonadState ModelGenState m
  ) =>  Operation -> m Type
cookieOutDef Operation {} =
  undefined

apiOutDef ::
  ( MonadState ModelGenState m
  ) =>  Operation -> m Type
apiOutDef Operation { _operationResponses = Responses { _responsesDefault = def
                                                      , _responsesResponses = byApiSuccess -> vs
                                                      }
                    } =
  responsesDef def vs

apiErrDef ::
  ( MonadState ModelGenState m
  ) => Operation -> m Type
apiErrDef Operation { _operationResponses = Responses { _responsesDefault = def
                                                      , _responsesResponses = byApiFailure -> vs
                                                      }
                    } =
  responsesDef def vs

responsesDef ::
  ( MonadState ModelGenState m
  ) => Maybe (Referenced Response) -> HMI.InsOrdHashMap HttpStatusCode (Referenced Response) -> m Type
responsesDef def resps = do
  vs <- HMI.foldrWithKey go (pure []) resps
  case vs of
    [] -> maybe (pure Untyped) responseDef def
    [v] -> pure (snd v)
    vs -> pure (ApiResponses vs)

  where go k v a = do
          v0 <- responseDef v
          ((k, v0) :) <$> a

responseDef :: Referenced Response -> m Type
responseDef =
  getResponsesComponent >=> go

  where
    go = undefined

responseHeaderDef :: Referenced Response -> m Type
responseHeaderDef =
  getResponsesComponent >=> go

  where
    go Response { _responseHeaders } =
      vs <- HMI.foldrWithKey go0 [] _responseHeaders
      case vs of
        [] -> 

    go0 k v a = do
      fld <- toFieldName k
      v0 <- getHeaderComponent v >>= go1 k
      ((k, v0) :) <$> a

    go1 hdName Header { _headerSchema, _headerRequired } =
      schemaToType (Reference hdName) (isRequired _headerRequired) _headerSchema

toHaskMethod :: StdMethod -> HsType'
toHaskMethod = var . fromString . show

-- toHaskRoute :: StdMethod -> (HsType', HsDecl')
-- toHaskRoute = var . fromString . show

writeModels :: ( MonadIO m ) => Vector HsDecl' -> m ()
writeModels decls =
  liftIO $ writeModule "/tmp/out" "Model.hs" hsMod

  where
    hsMod = module' Nothing Nothing [] (V.toList decls)

writeModule :: FilePath -> String -> {-[Extension] ->-} HsModule' -> IO ()
writeModule destFp fName {-es-} hsModule = do
    dynFlags <- runGhc (Just libdir) getDynFlags
    let fileContent = {-concatMap ppExtension es <>-} showSDoc dynFlags (ppr hsModule)
    txt <- ormolu defaultConfig { cfgCheckIdempotence = True } "" fileContent
    createDirectoryIfMissing True destFp
    T.writeFile (destFp </> fName) txt -- (T.pack fileContent)

-- ppExtension :: Extension -> String
-- ppExtension e = "{-# LANGUAGE " <> show e <> " #-}\n"

unsupportedSymbols :: String
unsupportedSymbols = "-!: "
