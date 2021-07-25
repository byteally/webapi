module WebApi.OpenAPI.Codegen.TypeRep where

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
  | Responses [ (HttpStatusCode, Type) ]
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
      fld <- toField k
      typ <- schemaToType root (k `elem` reqParams) v
      (:) <$> pure (fld, typ) <*> a

toField :: (MonadState ModelGenState m) => Text -> m FieldName
toField fld =
  pure $ FieldName { originalName = fld
                   , haskellName = sanitised
                   }

  where
    sanitised =
      let fld0 = removeUnsupportedSymbols fld
      in case fld0 `elem` keywords of
        True -> fld0 <> "_"
        _    -> fld0

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

-- NOTE: Invariant: all params are Inlined by this time.
byParam :: ParamLocation -> [ Referenced Param ] -> [ Param ]
byParam loc =
  mapMaybe go
  where
    go (Inline p@(Param { _paramLocation }))
      | _paramLocation == loc = Just p
      | otherwise             = Nothing
    go _ = error "Panic: Impossible case @byParam"

queryParamDef :: Operation -> m Type
queryParamDef Operation { _operationParams = byParamLocation ParamQuery -> ps } = do
  InlineRec <$> paramsToProduct ps

headerInDef :: Operation -> m Type
headerInDef Operation { _operationParams = byParamLocation ParamHeader -> ps } = do
  InlineRec <$> paramsToProduct ps

cookieInDef :: Operation -> m Type
cookieInDef Operation { _operationParams = byParamLocation ParamCookie -> ps } = do
  InlineRec <$> paramsToProduct ps

paramsToProduct ::
  ( MonadState ModelGenState m
  ) => [ Param ] -> m Fields
paramsToProduct =
  Fields . mapM go

  where
    isOpt = maybe True id
    go Param { _paramSchema, _paramName, _paramRequired } = do
      schTy <- case _paramSchema of
                 Nothing  -> pure Untyped
                 Just sch -> schemaToType (Reference _paramName) (not $ isOpt _paramRequired) sch
      fld <- toField _paramName
      pure (fld, schTy)

isApiSuccess :: HttpStatusCode -> Bool
isApiSuccess i
 | i >= 200 && < 300 = True
 | otherwise         = False

byApiSuccess :: HMI.InsOrdHashMap HttpStatusCode (Referenced Response) -> HMI.InsOrdHashMap HttpStatusCode (Referenced Response)
byApiSuccess = HMI.filterWithKey (\k _ -> isApiSuccess k)

byApiFailure :: HMI.InsOrdHashMap HttpStatusCode (Referenced Response) -> HMI.InsOrdHashMap HttpStatusCode (Referenced Response)
byApiFailure = HMI.filterWithKey (\k _ -> not (isApiSuccess k))

headerOutDef ::
  ( MonadState ModelGenState m
  ) =>  Operation -> m Type
headerOutDef Operation { _operationResponseDefault = def, _operationResponses = Responses { _responsesResponses = vs } } =
  responsesDef vs

cookieOutDef ::
  ( MonadState ModelGenState m
  ) =>  Operation -> m Type
cookieOutDef Operation { _operationResponseDefault = def, _operationResponses = Responses { _responsesResponses = vs } } =
  responsesDef vs

apiOutDef ::
  ( MonadState ModelGenState m
  ) =>  Operation -> m Type
apiOutDef Operation { _operationResponseDefault = def, _operationResponses = Responses { _responsesResponses = byApiSuccess -> vs } } =
  responsesDef vs

apiErrDef ::
  ( MonadState ModelGenState m
  ) => Operation -> m Type
apiErrDef Operation { _operationResponseDefault = def, _operationResponses = Responses { _responsesResponses = byApiFailure -> vs } } =
  responsesDef def vs

responsesDef ::
  ( MonadState ModelGenState m
  ) => Maybe (Referenced Response) -> HMI.InsOrdHashMap HttpStatusCode (Referenced Response) -> m Type
responsesDef def resps =
  vs <- HMI.foldrWithKey go resps
  case vs of
    [] -> maybe (pure Untyped) responseDef def
    [v] -> pure fst v
    vs -> pure (Responses vs)

  where go k v a = do
          v0 <- responseDef v
          ((k, v0) :) <$> a

responseDef :: Referenced Response -> m Type
