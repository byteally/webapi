module WebApi.OpenAPI.Hask where

keywords :: [ Text ]
keywords = [ "class"
           , "type"
           ]

removeUnsupportedSymbols :: Text -> Text
removeUnsupportedSymbols =
  T.filter (\x -> not (x `elem` unsupportedSymbols))

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
genHaskType (Responses tys) =
  genResponses tys
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

genOneOf ::
  ( MonadState ModelGenState m
  ) => [ (HttpStatusCode, Type) ] -> m HsType'
genOneOf =
  fmap (var "Responses" @@ ) . go
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
contractInstanceDef hsRouteTy Contract { route, method, operation } =
  instance' (var "ApiContract" @@
             var (fromString apiName) @@
             hsMethTy @@
             hsRouteTy
            ) body
  where
    hsMethTy = toHaskMethod method
    apiName = "TODO_FILLME"
    body =
      mapMaybe (uncurry (mkTyFam hsMethTy hsRouteTy)
               [ ("QueryParam", queryParamDef operation)
               , ("HeaderIn", headerInDef operation)
               , ("CookieIn", cookieInDef operation)
               , ("RequestBody", requestBodyDef operation)
               , ("ApiOut", apiOutDef operation)
               , ("ApiErr", apiErrDef operation)
               , ("HeaderOut", headerOutDef operation)
               , ("CookieOut", cookieOutDef operation)
               ]

mkTyFam ::
  ( Monad m
  ) => HsType' -> HsType' -> Text -> m Type -> m HsDecl'
mkTyFam hsMethTy hsRouteTy (T.unpack -> tyFamName) mty = do
  hsTy <- mty >>= schemaToType
  pure (tyFamInst (fromString tyFamName) [ hsMethTy, hsRouteTy, hsTy ])

toHaskMethod :: StdMethod -> HsType'
toHaskMethod = var . fromString . show

toHaskRoute :: StdMethod -> (HsType', HsDecl')
toHaskRoute = var . fromString . show

