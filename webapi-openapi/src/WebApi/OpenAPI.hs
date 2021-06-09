{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}

module WebApi.OpenAPI where

import Data.ByteString.Lazy as B (readFile)
import Data.Aeson ( decode )
import Data.OpenApi
    ( Components(_componentsSchemas),
      OpenApi(_openApiComponents),
      OpenApiItems(OpenApiItemsArray, OpenApiItemsObject),
      OpenApiType(..),
      Reference(Reference),
      Referenced(..),
      Schema(_schemaType, _schemaFormat, _schemaItems, _schemaRequired,
             _schemaProperties) )
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
      HsType' )
import GHC.Paths (libdir)
import GHC ( runGhc )
import Data.HashMap.Strict.InsOrd(toList)
import Data.Text as T ( unpack, Text, append, splitAt, toUpper, take, pack)
import Data.Text.IO as T (writeFile)
import GhcPlugins(getDynFlags,mkVarOcc)
import Control.Monad.State.Class ( MonadState(get) )
import Control.Monad.State.Lazy(evalState)
import Data.Set (Set, empty, fromList, member)
import Data.String ( IsString(fromString) )
import Data.Bifunctor ( Bifunctor(bimap) )
import Outputable(ppr,showSDoc)
import Ormolu
    ( ormolu, defaultConfig, Config(cfgCheckIdempotence) )
import System.Process
import System.FilePath.Posix
import System.Directory

data ModelGenState =
    ModelGenState { seenVars :: Set Text
                  , imports :: Set Text
                  , keywordsToAvoid :: Set Text
                  }

generateModels ::
    FilePath -> FilePath -> IO ()
generateModels fp destFp = do
    oApi <- readOpenAPI fp
    let compSchemas = toList . _componentsSchemas . _openApiComponents $ oApi
        modelList = evalState
                        (mapM createModelData compSchemas)
                        (ModelGenState empty empty (fromList keywords))
        hsModule = module' (Just modName) Nothing imps (namedTy:modelList)
    writeModule (pkgHome </> "src") (modName <.> "hs") es hsModule
    writeCabal pkgName modName pkgHome

    where imps = [ import' "Data.Int"
                 , exposing (import' "GHC.Types") [var "Symbol"]
                 , exposing (import' "Data.Text") [var "Text"]
                 ]
          namedTy = newtype' ":::" [kindedVar "fld" (var "Symbol") , bvar "a"]
                        (prefixCon "Field" [field $ var "a"]) []
          es = [TypeOperators,KindSignatures,DataKinds,DuplicateRecordFields]
          keywords = ["type"]
          pkgName = T.unpack . flip T.append "-models" . T.pack . dropExtension . takeFileName $ fp
          pkgHome = destFp </> pkgName
          modName = "OpenApiModels"

writeModule :: FilePath -> String -> [Extension] -> HsModule' -> IO ()
writeModule destFp fName es hsModule = do
    dynFlags <- runGhc (Just libdir) getDynFlags
    let fileContent = concatMap ppExtension es <> showSDoc dynFlags (ppr hsModule)
    txt <- ormolu defaultConfig { cfgCheckIdempotence = True } "" fileContent
    createDirectoryIfMissing True destFp
    T.writeFile (destFp </> fName) txt

writeCabal :: String -> String -> FilePath ->IO ()
writeCabal pkgName modName pkgHome = do
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
          xposedMods = [modName]
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
    rFields <- mapM (\(x,y) -> parseRecordFields (x,y) (x `elem` reqParams)) (toList . _schemaProperties $ dSchema)
    ModelGenState { keywordsToAvoid } <- get
    let frFields = bimap textToOccNameStr field .
                        (\(x,y)-> (if member x keywordsToAvoid
                                   then T.append x "_"
                                   else x,y)) <$> rFields
    return (data' (textToOccNameStr dName) [] [recordCon (textToOccNameStr dName) frFields] [])

parseRecordFields ::
    (MonadState ModelGenState m) =>
    (Text, Referenced Schema) -> Bool -> m (Text,HsType')
parseRecordFields (dName,Ref (Reference x)) isReq =
    return (dName,if isReq
                   then var $ textToRdrNameStr x
                   else var "Maybe" @@ var ( textToRdrNameStr x))
parseRecordFields (dName,Inline dSchema) isReq = parseInlineFields (_schemaType dSchema) dName dSchema isReq

parseInlineFields ::
    (MonadState ModelGenState m) =>
    Maybe OpenApiType -> Text -> Schema -> Bool -> m (Text,HsType' )
parseInlineFields (Just OpenApiString) dName _dSchema isReq =
    return (dName, if isReq then var "Text" else var "Maybe" @@ var "Text")
parseInlineFields (Just OpenApiNumber ) dName _dSchema isReq =
    return (dName, if isReq then var "Double" else var "Maybe" @@ var "Double")
parseInlineFields (Just OpenApiInteger) dName dSchema isReq =
    return (dName, if isReq then var $ textToRdrNameStr parsedInt else var "Maybe" @@ var (textToRdrNameStr parsedInt))
    where parsedInt = parseIntegerFld (_schemaFormat dSchema)
parseInlineFields (Just OpenApiBoolean) dName _dSchema isReq =
    return (dName,if isReq then var "Bool" else var "Maybe" @@ var "Bool")
parseInlineFields (Just OpenApiArray ) dName dSchema _isReq =
    case _schemaItems dSchema of
        Nothing -> error "No _schemaItems value for Array"
        Just (OpenApiItemsObject sch) -> do
            (dName2,dType) <- parseRecordFields (dName,sch) True
            return (dName2,listTy dType)
        Just (OpenApiItemsArray _) -> error "OpenApiItemsArray Array type"
parseInlineFields (Just OpenApiNull ) _dName _dSchema _isReq =
    error "Null OpenApi Type"

parseInlineFields (Just OpenApiObject ) dName dSchema isReq = do
    let reqParams =  _schemaRequired dSchema
    childInlines <- mapM (\(x,y) -> parseRecordFields (x,y) (x `elem` reqParams)) (toList . _schemaProperties $ dSchema)
    let typeTuple =  tuple $ (\(x,y)-> op (stringTy $ T.unpack x) ":::" y) <$> childInlines
    return (dName,if isReq then typeTuple else var "Maybe" @@ typeTuple)
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
