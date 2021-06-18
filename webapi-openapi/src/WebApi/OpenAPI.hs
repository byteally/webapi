{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE LambdaCase #-}

{-# OPTIONS_GHC -Wno-type-defaults #-}
module WebApi.OpenAPI where

import Data.ByteString.Lazy as B (readFile)
import Data.Aeson ( decode )
import Data.OpenApi
    ( Components(_componentsSchemas, _componentsParameters),
      OpenApi(_openApiComponents, _openApiPaths),
      OpenApiItems(OpenApiItemsArray, OpenApiItemsObject),
      OpenApiType(..),
      PathItem(PathItem),
      Reference(Reference),
      Referenced(..),
      Schema(_schemaType, _schemaFormat, _schemaItems, _schemaRequired,
             _schemaProperties, _schemaOneOf),
      Definitions,
      Param(_paramName, _paramSchema),
      Operation(_operationParameters))
import GHC.SourceGen
    ( data',
      field,
      newtype',
      prefixCon,
      recordCon,
      exposing,
      import',
      module',
      occNameToStr,
      tuple,
      kindedVar,
      listTy,
      stringTy,
      OccNameStr,
      RdrNameStr,
      App((@@), op),
      BVar(bvar),
      Var(var),
      HsDecl',
      HsModule',
      HsType',
      type',
      listPromotedTy,
      instance',
      tyFamInst)
import GHC.Paths (libdir)
import GHC ( runGhc )
import Data.HashMap.Strict.InsOrd as HMO (toList,lookup)
import Data.Text as T ( unpack, Text, append, splitAt, toUpper, take, pack, dropEnd, concat, split, toLower)
import Data.Text.IO as T (writeFile)
import GhcPlugins(getDynFlags,mkVarOcc)
import Control.Monad.State.Class ( MonadState(get), modify )
import Control.Monad.State.Lazy(evalState)
import Data.Set as S(Set, empty, fromList, member, insert,difference,null,toList)
import Data.String ( IsString(fromString) )
import Data.Bifunctor ( Bifunctor(bimap))
import Outputable(ppr,showSDoc)
import Ormolu
    ( ormolu, defaultConfig, Config(cfgCheckIdempotence) )
import System.Process ( callCommand )
import System.FilePath.Posix
    ( (<.>), (</>), dropExtension, takeFileName, splitDirectories )
import System.Directory ( createDirectoryIfMissing )
import Data.List as L (delete)
import Data.HashMap.Internal as HM (HashMap, singleton, unions, lookup, empty, insert)
import Language.Haskell.TH (Extension(..))
import Test.FitSpec.Utils(contained)

data ModelGenState =
    ModelGenState { seenVars :: Set Text
                  , imports :: Set Text
                  , keywordsToAvoid :: Set Text
                  , createdSums :: HashMap Text [Referenced Schema]
                  }

data PkgConfig =
    PkgConfig { authorName :: Text
              , email :: Text
              }

mkPkgConfig :: PkgConfig
mkPkgConfig = PkgConfig "\"Pankaj Singh Sijwali\"" "pankajsijwali1@gmail.com"

generateModels ::
    FilePath -> FilePath -> IO ()
generateModels fp destFp = do
    oApi <- readOpenAPI fp
    let compSchemas =  _componentsSchemas . _openApiComponents $ oApi
        compParams = _componentsParameters . _openApiComponents $ oApi
        oApiName =  "NetSuites" --_infoTitle . _openApiInfo $ oApi
        modelList = evalState
                        (mapM createModelData (HMO.toList compSchemas))
                        (ModelGenState S.empty S.empty (fromList keywords) HM.empty)
        hsModuleModel = module' (Just modName) Nothing impsModel (namedTy: Prelude.concat modelList)
        (routeInfo,typeSynList) =  unzip . Prelude.concat $
                                      evalState
                                        (mapM (createTypeSynData compParams) (HMO.toList . _openApiPaths $ oApi))
                                        (ModelGenState S.empty S.empty S.empty HM.empty)
        hsModuleTypeSyn = module' (Just typeSynName) Nothing impsTypeSyn (Prelude.concat typeSynList ++ webApiInstance oApiName routeInfo)
    writeModule (pkgHome </> "src") (modName <.> "hs") es hsModuleModel
    writeModule (pkgHome </> "src") (typeSynName <.> "hs") es2 hsModuleTypeSyn
    writeCabal pkgName mkPkgConfig modName [typeSynName] pkgHome

    where impsModel =
                 [ import' "Data.Int"
                 , exposing (import' "GHC.Types") [var "Symbol"]
                 , exposing (import' "Data.Text") [var "Text"]
                 ]
          impsTypeSyn =
                 [ import' "WebApi"
                 , import' modName
                 , import' "Data.Int"
                 ]
          namedTy = newtype' ":::" [kindedVar "fld" (var "Symbol") , bvar "a"]
                        (prefixCon "Field" [field $ var "a"]) []
          es = [TypeOperators,KindSignatures,DataKinds,DuplicateRecordFields]
          es2 = []
          keywords = ["type","class"]
          pkgName = T.unpack . flip T.append "-models" . T.pack . dropExtension . takeFileName $ fp
          pkgHome = destFp </> pkgName
          modName = "OpenApiModels"
          typeSynName = "WebApiInstances"
          dataTypeForOApi a = data' (textToOccNameStr . upperFirstChar $ a) [] [] []
          webApiInstance a b = [dataTypeForOApi a,
                                    instance' (var "WebApi" @@ var (textToRdrNameStr a))
                                              [tyFamInst "Apis" [var $ textToRdrNameStr a] (listPromotedTy (oneRoute <$> b))]]
          oneRoute (tName,methList) = var "Route" @@ listPromotedTy (var . textToRdrNameStr <$> methList) @@ var  (textToRdrNameStr tName)

createTypeSynData ::
    (MonadState ModelGenState m) =>
    Definitions Param -> (FilePath,PathItem) -> m [((Text,[Text]),[HsDecl'])]
createTypeSynData compsParam  (fp,PathItem _ _ piGet piPut piPost piDelete piOptions piHead piPatch piTrace _ piParams) = do
    let paramsMap = refParamsToParams compsParam piParams
        commonParams =
            (\case
               Right b ->
                 Right (b, case HM.lookup b paramsMap of
                              Nothing -> Nothing
                              x -> x)
               Left a -> Left a) <$> parseFilePath fp
        opList = fetchJusts [("GET",piGet), ("PUT",piPut), ("POST",piPost), ("DELETE",piDelete), ("OPTIONS",piOptions), ("HEAD",piHead), ("PATCH",piPatch), ("TRACE",piTrace)]
        overridesParam = fetchJusts $ fmap (handleOverridenParams compsParam commonParams)  <$> opList
    typSyns <- mapM createTypSynonym overridesParam
    commonTypSyn <- let diff = S.difference (pairLisToSet opList) (pairLisToSet overridesParam)
                     in if S.null diff
                         then return []
                         else do
                             ((a,_b),c) <- createTypSynonym ("",commonParams)
                             return [((a,S.toList diff),c)]

    return $ commonTypSyn ++ typSyns
    where justVal = \case
                        (_,Nothing) -> error "Unexpected Value"
                        (a,Just b) -> (a,b)
          filterJusts = \case
                           (_,Nothing) -> False
                           _ -> True
          fetchJusts = fmap justVal  . filter filterJusts
          pairLisToSet = S.fromList . fmap fst

createTypSynonym ::
    (MonadState ModelGenState m) =>
    (Text ,[Either Text (Text, Maybe (Bool, Param))]) ->
    m ((Text,[Text]),[HsDecl'])
createTypSynonym (oName,params) = do
    varName <- mkUnseenVar (mkTypeSynName params (T.append oName "R"))
    (typInfo,childTypes) <-unzip <$> mapM parseTypeSynInfo params
    return ((varName, [oName]), type' (textToOccNameStr varName)
                                       []
                                       (foldr1 (`op` ":/") typInfo): Prelude.concat childTypes)

parseTypeSynInfo ::
    (MonadState ModelGenState m) =>
    Either Text (Text, Maybe (Bool, Param)) ->
    m (HsType',[HsDecl'])
parseTypeSynInfo (Left x) = return (stringTy $ T.unpack x,[])
parseTypeSynInfo (Right (_,Nothing)) = error "Type Not Found"
parseTypeSynInfo (Right (x,Just (_,y))) =
    case _paramSchema y of
        Nothing -> error "No Parameter Schema"
        Just s  -> do
            ((_,typ),childTypes) <- parseRecordFields (x,s) True
            return (typ,childTypes)


mkTypeSynName :: [Either Text (Text, Maybe (Bool, Param))] -> Text -> Text
mkTypeSynName a = T.append
    (T.concat $ upperFirstChar . leftVal <$> filter isLeft a)
    where isLeft = \case
                        Left _ -> True
                        _      -> False
          leftVal = \case
                        Left x -> x
                        _      -> error "Unexpected Right"


mkUnseenVar :: (Monad m, MonadState ModelGenState m) => Text -> m Text
mkUnseenVar t = do
  ModelGenState { seenVars } <- get
  let newV = go (0 :: Integer) seenVars (removeUnsupportedSymbols t)
  modify (updateSeenVars newV)
  pure newV
  where go x varSet el =
         if member el varSet then (case x of
                                    0 -> go 1 varSet (T.pack (T.unpack el ++ "0"))
                                    _ -> go (x+1) varSet
                                          (T.pack (T.unpack (T.dropEnd (length . show $ x-1) el) ++ show x))) else el

updateSeenVars :: Text -> ModelGenState -> ModelGenState
updateSeenVars v (ModelGenState x y z w) =
  ModelGenState ( S.insert v x) y z w

updateSumTypes :: (Text,[Referenced Schema]) -> ModelGenState -> ModelGenState
updateSumTypes (a,b) (ModelGenState w x y z) =
    ModelGenState w x y (HM.insert a b z)

handleOverridenParams ::
    Definitions Param ->
    [Either Text (Text, Maybe (Bool, Param))] ->
    Operation ->
    Maybe [Either Text (Text, Maybe (Bool, Param))]
handleOverridenParams compsParam pathList x =
    let pMap = refParamsToParams compsParam . _operationParameters $ x
        overParams =  (\case
                            Right (a,p) ->
                                Right (a ,case HM.lookup a pMap of
                                            Nothing -> (False,p)
                                            s -> if p == s
                                                 then (False,p)
                                                 else (True,s))
                            Left a ->  Left a
                      ) <$> pathList
    in if or $ (\case
                   Left _ -> False
                   Right (_,(y,_)) -> y) <$> overParams
        then Just $ (\case
                        Right (a,(_,b)) -> Right (a,b)
                        Left a -> Left a ) <$> overParams
        else Nothing

refSchemaToSchema :: Definitions Schema -> Referenced Schema -> Either (Text,Schema) Schema
refSchemaToSchema compSchemas (Ref (Reference a)) =
    Left (a,case HMO.lookup a compSchemas of
                Nothing -> error "Reference Schema Not Found"
                Just x -> x)
refSchemaToSchema _compSchemas (Inline x) = Right x

refParamsToParams ::
    Definitions Param ->
    [Referenced Param] ->
    HashMap Text (Bool,Param)
refParamsToParams compsParam params =
     unions $ fmap (\case
                        Inline a -> HM.singleton (_paramName a) (False,a)
                        Ref (Reference a) ->
                                let param = HMO.lookup a compsParam
                                in case param of
                                      Nothing -> error "Reference not found"
                                      Just p -> HM.singleton (_paramName p) (True,p)) params

parseFilePath :: FilePath -> [Either Text Text]
parseFilePath fp = (\x ->
                        if head x == '{' && last x == '}'
                        then Right $ T.pack . tail . init $ x
                        else Left $ T.pack x ) <$> L.delete "/" (splitDirectories fp)

writeModule :: FilePath -> String -> [Extension] -> HsModule' -> IO ()
writeModule destFp fName es hsModule = do
    dynFlags <- runGhc (Just libdir) getDynFlags
    let fileContent = concatMap ppExtension es <> showSDoc dynFlags (ppr hsModule)
    txt <- ormolu defaultConfig { cfgCheckIdempotence = True } "" fileContent
    createDirectoryIfMissing True destFp
    T.writeFile (destFp </> fName) txt --(T.pack fileContent)

writeCabal :: String -> PkgConfig -> String -> [String] -> FilePath ->IO ()
writeCabal pkgName (PkgConfig aName aEmail) modName exposMods pkgHome =
    callCommand ("cd " ++ pkgHome ++ " ; " ++ str)
    where str = "cabal init --non-interactive --overwrite"
                 ++ " --package-name="
                 ++ pkgName
                 ++ " --author=" ++ T.unpack aName
                 ++ " --email=" ++ T.unpack aEmail
                 ++ " --lib"
                 ++ " --source-dir=src"
                 ++ iterateOption " --expose-module" xposedMods
                 ++ iterateOption " --dependency" dpends
          dpends = ["base","text", "ghc-prim"]
          xposedMods = modName:exposMods
          iterateOption option = concatMap (\x -> option ++ "=" ++ x)

ppExtension :: Extension -> String
ppExtension e = "{-# LANGUAGE " <> show e <> " #-}\n"

createModelData ::
    (MonadState ModelGenState m) =>
    (Text,Schema) -> m [HsDecl']
createModelData (dName,dSchema) = do
    let reqParams =  _schemaRequired dSchema
    unseenVar <- mkUnseenVar (upperFirstChar dName)
    (rFields,childTypes) <- unzip <$> mapM (\(x,y) -> parseRecordFields (x,y) (x `elem` reqParams))  (HMO.toList . _schemaProperties $ dSchema)
    ModelGenState { keywordsToAvoid } <- get
    let frFields = bimap (textToOccNameStr . lowerFirstChar) field .
                        (\(x,y)-> (if member x keywordsToAvoid
                                   then T.append x "_"
                                   else x,y)) <$> rFields
    return $ Prelude.concat childTypes ++ [data' (textToOccNameStr unseenVar) [] [recordCon (textToOccNameStr . upperFirstChar $ dName) frFields] []]

parseRecordFields ::
    (MonadState ModelGenState m) =>
    (Text, Referenced Schema) -> Bool -> m ((Text,HsType'),[HsDecl'])
parseRecordFields (dName,Ref (Reference x)) isReq =
    return ((dName,if isReq
                   then var $ textToRdrNameStr . upperFirstChar $ x
                   else var "Maybe" @@ var ( textToRdrNameStr . upperFirstChar $ x)),[])
parseRecordFields (dName,Inline dSchema) isReq = parseInlineFields (_schemaType dSchema) dName dSchema isReq

parseInlineFields ::
    (MonadState ModelGenState m) =>
    Maybe OpenApiType -> Text -> Schema -> Bool -> m ((Text,HsType'),[HsDecl'])
parseInlineFields (Just OpenApiString) dName _dSchema isReq =
    return ((dName, if isReq then var "Text" else var "Maybe" @@ var "Text"),[])
parseInlineFields (Just OpenApiNumber ) dName _dSchema isReq =
    return ((dName, if isReq then var "Double" else var "Maybe" @@ var "Double"),[])
parseInlineFields (Just OpenApiInteger) dName dSchema isReq =
    return ((dName, if isReq then var $ textToRdrNameStr parsedInt else var "Maybe" @@ var (textToRdrNameStr parsedInt)),[])
    where parsedInt = parseIntegerFld (_schemaFormat dSchema)
parseInlineFields (Just OpenApiBoolean) dName _dSchema isReq =
    return ((dName,if isReq then var "Bool" else var "Maybe" @@ var "Bool"),[])
parseInlineFields (Just OpenApiArray ) dName dSchema _isReq =
    case _schemaItems dSchema of
        Nothing -> error "No _schemaItems value for Array"
        Just (OpenApiItemsObject sch) -> do
            ((dName2,dType),childTypes) <- parseRecordFields (dName,sch) True
            return ((dName2,listTy dType),childTypes)
        Just (OpenApiItemsArray _) -> error "OpenApiItemsArray Array type"
parseInlineFields (Just OpenApiNull ) _dName _dSchema _isReq =
    error "Null OpenApi Type"
parseInlineFields (Just OpenApiObject ) dName dSchema isReq = do
    (childInlines,childTypes) <- unzip <$> mapM (\(x,y) -> parseRecordFields (x,y) (x `elem` reqParams)) (HMO.toList . _schemaProperties $ dSchema)
    let typeTuple =  tuple $ (\(x,y)-> op (stringTy $ T.unpack x) ":::" y) <$> childInlines
    return ((dName,if isReq then typeTuple else var "Maybe" @@ typeTuple),Prelude.concat childTypes)
    where reqParams =  _schemaRequired dSchema

parseInlineFields Nothing dName dSchema isReq =
    case _schemaOneOf dSchema of
        Nothing -> let props = HMO.toList . _schemaProperties $ dSchema
                   in case props of
                       [] -> error "Unexpected Schema type"
                       _ -> parseInlineFields (Just OpenApiObject) dName dSchema isReq
        Just x -> do
            ModelGenState { createdSums } <- get
            (isReg,vName) <- registerSumType (upperFirstChar dName) x createdSums
            if isReg
            then return ((dName,if isReq then var (textToRdrNameStr vName) else var "Maybe" @@ var (textToRdrNameStr vName)), [])
            else do
                let newVarList = mkNewVariables vName (length x)
                unSeenVars <- mapM mkUnseenVar newVarList
                (typList,childTypes) <- unzip <$> mapM (\(a,b) -> parseRecordFields (a,b) True)  (zip unSeenVars x)
                let oneOfTyp = data' (textToOccNameStr vName) [] ((\(a,b) -> prefixCon (textToOccNameStr a) [field b]) <$> typList) []
                return ((dName,if isReq then var (textToRdrNameStr vName) else var "Maybe" @@ var (textToRdrNameStr vName)), oneOfTyp : Prelude.concat childTypes)

registerSumType :: 
    MonadState ModelGenState m =>
     Text -> [Referenced Schema] -> HashMap Text [Referenced Schema] -> m (Bool,Text)
registerSumType tName schemaList hm =
    case HM.lookup tName hm of
        Nothing -> do
            unseenVar <- mkUnseenVar tName
            modify (updateSumTypes (unseenVar,schemaList))
            return (False,unseenVar)
        Just x -> do
            if contained x schemaList && contained schemaList x
             then return (True,tName)
             else do
                 unseenVar <- mkUnseenVar tName
                 modify (updateSumTypes (unseenVar,schemaList))
                 return (False,unseenVar)

mkNewVariables :: Text -> Int -> [Text]
mkNewVariables _dName 0 = []
mkNewVariables dName x =  mkNewVariables dName (x-1)  ++ [T.concat [dName,"C", T.pack . show $ x]]

parseIntegerFld :: Maybe Text -> Text
parseIntegerFld (Just x) = let y = upperFirstChar x
                            in if T.take 3 y == "Int"
                               then y
                               else error "Invalid Integer Format"
parseIntegerFld Nothing = "Int"

removeUnsupportedSymbols :: Text -> Text
removeUnsupportedSymbols = removeSymbol '-' . removeSymbol '!' . removeSymbol ':'

textToOccNameStr :: Text -> OccNameStr
textToOccNameStr = occNameToStr . mkVarOcc . unpack . removeUnsupportedSymbols

textToRdrNameStr :: Text -> RdrNameStr
textToRdrNameStr = fromString . T.unpack . removeUnsupportedSymbols

upperFirstChar :: Text -> Text
upperFirstChar x = let (fir,res) = T.splitAt 1 x in append (toUpper fir) res

lowerFirstChar :: Text -> Text
lowerFirstChar x = let (fir,res) = T.splitAt 1 x in append (toLower fir) res

removeSymbol :: Char -> Text -> Text
removeSymbol c x = T.concat $
    let elems = T.split (== c) x
        fir = head elems
    in fir : (upperFirstChar <$> tail elems)


retOApi :: Maybe p -> p
retOApi (Just x) = x
retOApi Nothing = error "Can't decode OpenAPI spec file"

readOpenAPI :: FilePath -> IO OpenApi
readOpenAPI fp = do
    fileContent <- B.readFile fp
    return $ retOApi (decode fileContent :: Maybe OpenApi)
