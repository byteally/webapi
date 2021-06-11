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
             _schemaProperties),
      Definitions,
      Param(_paramName, _paramSchema),
      Operation(_operationParameters) )
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
      type')
import GHC.Paths (libdir)
import GHC ( runGhc )
import Data.HashMap.Strict.InsOrd as HMO (toList,lookup)
import Data.Text as T ( unpack, Text, append, splitAt, toUpper, take, pack, dropEnd, concat)
import Data.Text.IO as T (writeFile)
import GhcPlugins(getDynFlags,mkVarOcc)
import Control.Monad.State.Class ( MonadState(get), modify )
import Control.Monad.State.Lazy(evalState)
import Data.Set (Set, empty, fromList, member, insert,)
import Data.String ( IsString(fromString) )
import Data.Bifunctor ( Bifunctor(bimap) ,first)
import Outputable(ppr,showSDoc)
import Ormolu
    ( ormolu, defaultConfig, Config(cfgCheckIdempotence) )
import System.Process ( callCommand )
import System.FilePath.Posix
    ( (<.>), (</>), dropExtension, takeFileName, splitDirectories )
import System.Directory ( createDirectoryIfMissing )
import Data.List as L (delete)
import Data.HashMap.Internal as HM (HashMap, singleton, unions, lookup)
import Data.Maybe ( catMaybes )

data ModelGenState =
    ModelGenState { seenVars :: Set Text
                  , imports :: Set Text
                  , keywordsToAvoid :: Set Text
                  }

generateModels ::
    FilePath -> FilePath -> IO ()
generateModels fp destFp = do
    oApi <- readOpenAPI fp
    let compSchemas =  _componentsSchemas . _openApiComponents $ oApi
        compParams = _componentsParameters . _openApiComponents $ oApi
        modelList = evalState
                        (mapM createModelData (toList compSchemas))
                        (ModelGenState empty empty (fromList keywords))
        hsModuleModel = module' (Just modName) Nothing impsModel (namedTy:modelList)
        typeSynList = evalState
                        (mapM (createTypeSynData compParams) (toList . _openApiPaths $ oApi))
                        (ModelGenState empty empty empty)
        hsModuleTypeSyn = module' (Just typeSynName) Nothing impsTypeSyn (Prelude.concat typeSynList)
    writeModule (pkgHome </> "src") (modName <.> "hs") es hsModuleModel
    writeModule (pkgHome </> "src") (typeSynName <.> "hs") es2 hsModuleTypeSyn
    writeCabal pkgName modName [typeSynName] pkgHome

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
          keywords = ["type"]
          pkgName = T.unpack . flip T.append "-models" . T.pack . dropExtension . takeFileName $ fp
          pkgHome = destFp </> pkgName
          modName = "OpenApiModels"
          typeSynName = "WebApiInstances"

createTypeSynData ::
    (MonadState ModelGenState m) =>
    Definitions Param -> (FilePath,PathItem) -> m [HsDecl']
createTypeSynData compsParam  (fp,PathItem _ _ piGet piPut piPost piDelete piOptions piHead piPatch piTrace _ piParams) = do
    let paramsMap = refParamsToParams compsParam piParams
        commonParams =
            (\case
               Right b ->
                 (Right b, case HM.lookup b paramsMap of
                              Nothing -> Nothing
                              x -> x)
               a -> (a,Nothing))  <$> parseFilePath fp
        opList = [("Get",piGet), ("Put",piPut), ("Post",piPost), ("Delete",piDelete), ("Options",piOptions), ("Head",piHead), ("Patch",piPatch), ("Trace",piTrace)]
        overridesParam = fmap (handleOverridenParams compsParam commonParams)  <$> opList
    if null (catMaybes (snd <$> overridesParam))
    then  (: []) <$> createTypSynonym (Nothing, Nothing) commonParams
    else mapM (`createTypSynonym` commonParams) 
              (first Just <$> filter (\(_,y) -> case y of
                                                    Nothing -> False
                                                    _ -> True) overridesParam)

createTypSynonym ::
    (MonadState ModelGenState m) =>
    (Maybe String , Maybe [(Either Text Text, Maybe (Bool, Param))]) ->
    [(Either Text Text, Maybe (Bool, Param))] -> m HsDecl'
createTypSynonym (Nothing,_) baseParams = do
    varName <- mkUnseenVar (mkTypeSynName baseParams "R")
    let typInfo = parseTypeSynInfo baseParams
    return $ type' (textToOccNameStr varName)
                   []
                   (foldr1 (`op` ":/") typInfo)
createTypSynonym (Just _, Nothing ) _ = error "Invalid State"
createTypSynonym (Just vName,Just ovParams) baseParams = do
    varName <- mkUnseenVar (mkTypeSynName baseParams (T.append (T.pack vName) "R"))
    let overrides = if length baseParams /= length ovParams
                    then error "Invalid State"
                    else zipWith applyOverride ovParams baseParams
        typInfo = parseTypeSynInfo overrides
    return $ type' (textToOccNameStr varName)
                   []
                   (foldr1 (`op` ":/") typInfo)

applyOverride ::
    (Either Text Text, Maybe (Bool, Param)) ->
    (Either Text Text, Maybe (Bool, Param)) ->
    (Either Text Text, Maybe (Bool, Param))
applyOverride (Left x,_) (Left y,_) =
    if x == y then (Left x,Nothing) else error "Invalid State"
applyOverride (Right _, Nothing) (Right _, Nothing) =
    error "No type info found"
applyOverride (Right x, Nothing) (Right y, Just z) =
    if x == y then (Right x,Just z) else error "Invalid State"
applyOverride (Right x, a) (Right y,_) =
    if x == y then (Right x, a) else error "Invalid State"
applyOverride _ _ = error "Invalid State"

parseTypeSynInfo ::
    [(Either Text Text, Maybe (Bool, Param))] ->
    [HsType']
parseTypeSynInfo = fmap
                    (\case
                      (Left x,_) -> stringTy $ T.unpack x
                      (Right _, Nothing) -> error "Type Not Found"
                      (Right _, Just (_,y)) ->
                          case _paramSchema y of
                             Nothing -> error "No Parameter Schema"
                             Just s  ->  snd $ parseRecordFields ("",s) True
                    )


mkTypeSynName :: [(Either Text Text, Maybe (Bool, Param))] -> Text -> Text
mkTypeSynName a = T.append
    (T.concat $ upperFirstChar . leftVal . fst <$> filter (\(x,_) -> isLeft x) a)
    where isLeft = \case
                        Left _ -> True
                        _      -> False
          leftVal = \case
                        Left x -> x
                        _      -> error "Unexpected Right"


mkUnseenVar :: (Monad m, MonadState ModelGenState m) => Text -> m Text
mkUnseenVar t = do
  ModelGenState { seenVars } <- get
  let newV = go (0 :: Integer) seenVars t
  modify (updateSeenVars newV)
  pure newV
  where go x varSet el =
         if member el varSet then (case x of
                                    0 -> go 1 varSet (T.pack (T.unpack el ++ "0"))
                                    _ -> go (x+1) varSet
                                          (T.pack (T.unpack (T.dropEnd (length . show $ x-1) el) ++ show x))) else el

updateSeenVars :: Text -> ModelGenState -> ModelGenState
updateSeenVars v (ModelGenState x y z) =
  ModelGenState (insert v x) y z



handleOverridenParams ::
    Definitions Param ->
    [(Either Text Text, Maybe (Bool, Param))] ->
    Maybe Operation ->
    Maybe [(Either Text Text, Maybe (Bool, Param))]
handleOverridenParams _ _ Nothing = Nothing
handleOverridenParams compsParam pathList (Just x) =
    let pMap = refParamsToParams compsParam . _operationParameters $ x
        overParams =  (\case
                            (Right a,p) ->
                                (Right a , let pVal = HM.lookup a pMap
                                           in if pVal == p
                                              then Nothing
                                              else pVal)
                            (Left a,_) ->  (Left a, Nothing)
                      ) <$> pathList
    in if null (catMaybes (snd <$> overParams))
        then Nothing
        else Just overParams

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
    T.writeFile (destFp </> fName) txt

writeCabal :: String -> String -> [String] -> FilePath ->IO ()
writeCabal pkgName modName exposMods pkgHome = do
    callCommand ("cd " ++ pkgHome ++ " ; " ++ str)
    where str = "cabal init --non-interactive --overwrite"
                 ++ " --package-name="
                 ++ pkgName
                 ++ " --author=\"Pankaj Singh Sijwali\""
                 ++ " --email=pankajsijwali1@gmail.com"
                 ++ " --lib"
                 ++ " --source-dir=src"
                 ++ iterateOption " --expose-module" xposedMods
                 ++ iterateOption " --dependency" dpends
          dpends = ["base","text", "ghc-prim"]
          xposedMods = modName:exposMods
          iterateOption option = concatMap (\x -> option ++ "=" ++ x)

data Extension =
    DeriveGeneric
  | DataKinds
  | KindSignatures
  | DuplicateRecordFields
  | PatternSynonyms
  | OverloadedStrings
  | MultiParamTypeClasses
  | MultiWayIf
  | TypeApplications
  | FlexibleContexts
  | FlexibleInstances
  | ScopedTypeVariables
  | TypeFamilies
  | UndecidableInstances
  | StandaloneDeriving
  | TypeOperators
  deriving (Show, Eq)

ppExtension :: Extension -> String
ppExtension e = "{-# LANGUAGE " <> show e <> " #-}\n"

createModelData ::
    (MonadState ModelGenState m) =>
    (Text,Schema) -> m HsDecl'
createModelData (dName,dSchema) = do
    let reqParams =  _schemaRequired dSchema
        rFields = (\(x,y) -> parseRecordFields (x,y) (x `elem` reqParams)) <$> (toList . _schemaProperties $ dSchema)
    ModelGenState { keywordsToAvoid } <- get
    let frFields = bimap textToOccNameStr field .
                        (\(x,y)-> (if member x keywordsToAvoid
                                   then T.append x "_"
                                   else x,y)) <$> rFields
    return (data' (textToOccNameStr dName) [] [recordCon (textToOccNameStr dName) frFields] [])

parseRecordFields ::
    (Text, Referenced Schema) -> Bool -> (Text,HsType')
parseRecordFields (dName,Ref (Reference x)) isReq =
    (dName,if isReq
                   then var $ textToRdrNameStr x
                   else var "Maybe" @@ var ( textToRdrNameStr x))
parseRecordFields (dName,Inline dSchema) isReq = parseInlineFields (_schemaType dSchema) dName dSchema isReq

parseInlineFields ::
    Maybe OpenApiType -> Text -> Schema -> Bool -> (Text,HsType' )
parseInlineFields (Just OpenApiString) dName _dSchema isReq =
    (dName, if isReq then var "Text" else var "Maybe" @@ var "Text")
parseInlineFields (Just OpenApiNumber ) dName _dSchema isReq =
    (dName, if isReq then var "Double" else var "Maybe" @@ var "Double")
parseInlineFields (Just OpenApiInteger) dName dSchema isReq =
    (dName, if isReq then var $ textToRdrNameStr parsedInt else var "Maybe" @@ var (textToRdrNameStr parsedInt))
    where parsedInt = parseIntegerFld (_schemaFormat dSchema)
parseInlineFields (Just OpenApiBoolean) dName _dSchema isReq =
    (dName,if isReq then var "Bool" else var "Maybe" @@ var "Bool")
parseInlineFields (Just OpenApiArray ) dName dSchema _isReq =
    case _schemaItems dSchema of
        Nothing -> error "No _schemaItems value for Array"
        Just (OpenApiItemsObject sch) ->
            let (dName2,dType) = parseRecordFields (dName,sch) True
             in (dName2,listTy dType)
        Just (OpenApiItemsArray _) -> error "OpenApiItemsArray Array type"
parseInlineFields (Just OpenApiNull ) _dName _dSchema _isReq =
    error "Null OpenApi Type"
parseInlineFields (Just OpenApiObject ) dName dSchema isReq = do
    (dName,if isReq then typeTuple else var "Maybe" @@ typeTuple)
    where reqParams =  _schemaRequired dSchema
          childInlines = (\(x,y) -> parseRecordFields (x,y) (x `elem` reqParams)) <$> (toList . _schemaProperties $ dSchema)
          typeTuple =  tuple $ (\(x,y)-> op (stringTy $ T.unpack x) ":::" y) <$> childInlines
parseInlineFields Nothing _dName _dSchema _isReq = error "No Type Defined"

parseIntegerFld :: Maybe Text -> Text
parseIntegerFld (Just x) = let y = upperFirstChar x
                            in if T.take 3 y == "Int"
                               then y
                               else error "Invalid Integer Format"
parseIntegerFld Nothing = "Int"


textToOccNameStr :: Text -> OccNameStr
textToOccNameStr = occNameToStr . mkVarOcc . unpack

textToRdrNameStr :: Text -> RdrNameStr
textToRdrNameStr = fromString . T.unpack

upperFirstChar :: Text -> Text
upperFirstChar x = let (fir,res) = T.splitAt 1 x in append (toUpper fir) res

retOApi :: Maybe p -> p
retOApi (Just x) = x
retOApi Nothing = error "Can't decode OpenAPI spec file"

readOpenAPI :: FilePath -> IO OpenApi
readOpenAPI fp = do
    fileContent <- B.readFile fp
    return $ retOApi (decode fileContent :: Maybe OpenApi)
