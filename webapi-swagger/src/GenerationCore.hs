{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module GenerationCore where

import SwaggerGen

-- import GHC.Generics
-- import qualified Data.HashMap.Strict as HM
-- import qualified Data.HashMap.Strict.InsOrd as OHM
-- import Data.Hashable
-- import qualified Data.Aeson as A
import qualified Data.Text as T
import qualified Data.List as DL
import Language.Haskell.Exts as LHE hiding (OPTIONS, Int, Tuple, Comma)
import Data.Maybe
import HaskellValidation
import Data.HashMap.Strict as HMS
import Constants
import qualified Data.Set as Set
import System.Directory
import qualified Data.ByteString.Lazy as BSL
import Data.Swagger (Swagger)
import Data.Yaml (decodeEither')
import Control.Applicative ((<|>))
import Data.Aeson (eitherDecode)
import Control.Monad.IO.Class





runCodeGen :: FilePath -> FilePath -> String -> IO () 
runCodeGen swaggerJsonInputFilePath outputPath projectName = do
  let projectFolderGenPath = outputPath ++ projectName ++ "/"
  createDirectoryIfMissing True (projectFolderGenPath ++ "src/")
  swaggerJSONContents <- liftIO $ BSL.readFile swaggerJsonInputFilePath
  let decodedVal = eitherDecode swaggerJSONContents <|> either (Left . show) Right (decodeEither' (BSL.toStrict swaggerJSONContents))
  case decodedVal of
    Left errMsg -> error $ errMsg -- "Panic: not a valid JSON or yaml"
    Right (swaggerData :: Swagger) -> do
      finalStateVal <- runSwaggerGenerator () (generateSwaggerState swaggerData)
      writeFile (projectFolderGenPath ++ "src/CommonTypes.hs") commonTypesModuleContent
      let tyState = typeState finalStateVal
      createdModuleNames <- generateModulesFromTypeState tyState outputPath 
      writeCabalAndProjectFiles outputPath projectName False (Set.toList createdModuleNames)

  

generateModulesFromTypeState :: TypeState -> FilePath -> IO (Set.Set String)
generateModulesFromTypeState tState genPath = HMS.foldlWithKey' (parseStateAndGenerateFile genPath) (pure $ Set.empty) tState
 where
  parseStateAndGenerateFile :: FilePath -> IO (Set.Set String) -> TypeMeta -> TypeDefinition -> IO (Set.Set String)
  parseStateAndGenerateFile genDirPath ioModuleNames typeMeta typeDefn = do
    moduleNames <- ioModuleNames
    let (typesModuleDir, typesModuleName) = 
          case typeMeta of
            ParamType _ routeInfo methodName -> constructLocalTypeModulePath routeInfo (Just methodName) genDirPath
            ResponseType _ routeInfo methodName ->  constructLocalTypeModulePath routeInfo (Just methodName) genDirPath
            Definition provenance _ -> 
              case provenance of
                Global _ -> (genDirPath ++ globalTypesModulePath, hsModuleToFileName globalDefnsModuleName)
                RouteLocal routeInfo _ ->  constructLocalTypeModulePath routeInfo Nothing genDirPath
                Local routeInfo methodName _ ->  constructLocalTypeModulePath routeInfo (Just methodName) genDirPath

    tyModuleExists <- doesFileExist (typesModuleDir ++ typesModuleName) 
    let (modName:: String) = 
          case typeMeta of
            ParamType _ routeInfo methodName -> constructLocalTypeModuleName routeInfo (Just methodName)
            ResponseType _ routeInfo methodName -> constructLocalTypeModuleName routeInfo (Just methodName)
            Definition provenance _ -> 
              case provenance of
                Global _ -> globalTypesHsModuleName ++ globalDefnsModuleName
                Local routeInfo methodName _ -> constructLocalTypeModuleName routeInfo (Just methodName)
                RouteLocal routeInfo _ -> constructLocalTypeModuleName routeInfo Nothing
                  
                  
    case tyModuleExists of 
      True -> do
        let dataDecl = constructDeclFromCustomType $ customHaskType typeDefn
            newContents = "\n\n" ++ prettyPrint dataDecl
        appendFile (typesModuleDir ++ typesModuleName) newContents
        pure $ Set.insert modName moduleNames
      False -> do
        let dataDecl = constructDeclFromCustomType $ customHaskType typeDefn
            newTyModuleContents = 
              prettyPrint $ 
                Module noSrcSpan 
                      (Just $ ModuleHead noSrcSpan (ModuleName noSrcSpan modName) Nothing Nothing)
                      (fmap languageExtension languageExtensionsForTypesModule)
                      (fmap moduleImport 
                        ( (DL.zip importsForTypesModule (cycle [(False, Nothing)]) ) 
                          ++ qualTyModuleImports )) -- ++ ( qualifiedGlobalImports (getGlobalModuleNames moduleNames) ) ) )
                      [dataDecl]
        createDirectoryIfMissing True typesModuleDir
        writeFile (typesModuleDir ++ typesModuleName) newTyModuleContents
        pure $ Set.insert modName moduleNames
      
  parseRouteIntoFolderName :: Route UnparsedPiece -> String
  parseRouteIntoFolderName routeInfo =
    case getRoute routeInfo of
      [] -> error $ "Encountered empty Route! Expected the route to contain atleast one piece!"
      routeInfoList -> DL.intercalate "/" $ fmap (validateRouteDirectoryPath . showRoutePiece) routeInfoList

  showRoutePiece :: RoutePiece UnparsedPiece -> String
  showRoutePiece routePiece = 
    case routePiece of
      Static pieceTxt -> T.unpack pieceTxt
      Dynamic unpPiece -> T.unpack unpPiece

  parseRouteIntoModuleName :: Route UnparsedPiece -> String
  parseRouteIntoModuleName routeInfo =
    case getRoute routeInfo of
      [] -> error $ "Encountered empty Route! Expected the route to contain atleast one piece!"
      routeInfoList -> DL.intercalate "." $ fmap (validateRouteModuleName . showRoutePiece) routeInfoList      


  qualTyModuleImports :: [(String, (Bool, Maybe (ModuleName SrcSpanInfo)))]
  qualTyModuleImports =
    let qualImportList = qualifiedImportsForTypesModule    
    in fmap (\(fullModuleName, qual) ->  (fullModuleName, (True, Just $ ModuleName noSrcSpan qual)) ) qualImportList

  validateRouteDirectoryPath :: String -> String
  validateRouteDirectoryPath = setValidConstructorId

  validateRouteModuleName :: String -> String
  validateRouteModuleName = setValidConstructorId
 
  constructLocalTypeModulePath :: Route UnparsedPiece -> Maybe Method -> FilePath -> (String, String)
  constructLocalTypeModulePath routeInfo mMethodName genDirPath = do
    let routePath = parseRouteIntoFolderName routeInfo
    case mMethodName of
      Just methodName -> 
        ( genDirPath ++ (localRouteMethodTypesPath routePath methodName), hsModuleToFileName localRouteMethodTypesModuleName)
      Nothing -> 
        ( genDirPath ++ (routeLevelTypesPath routePath), hsModuleToFileName routeLevelTypesModuleName)

  constructLocalTypeModuleName :: Route UnparsedPiece -> Maybe Method -> String
  constructLocalTypeModuleName routeInfo mMethodName = do
    let routeModuleName = parseRouteIntoModuleName routeInfo
    case mMethodName of
      Just methodName -> 
        (localRouteMethodTypesModName routeModuleName methodName) ++ localRouteMethodTypesModuleName
      Nothing -> 
        (routeLevelTypesModName routeModuleName) ++ routeLevelTypesModuleName

  

  --  where
  --   createDataDeclarations :: [(CreateDataType, NamingCounter)] -> [Decl SrcSpanInfo]
  --   createDataDeclarations = DL.foldl' createTypeDeclFromCDT []  
  
  -- getGlobalModuleNames :: [String] -> [String]
  -- getGlobalModuleNames = DL.filter (DL.isInfixOf ".GlobalDefinitions.") 
    


customTypeForProdTy :: CustomType
customTypeForProdTy =
  CustomType "Foo" [DataConstructor "Foo" [(Just "rec1", Inline Text), (Just "rec2",Inline Bool)] ]

customTypeForEnum :: CustomType
customTypeForEnum =
    CustomType "SomeFooEnum" [DataConstructor "Available" [] , DataConstructor "Pending" [], DataConstructor "Cancelled" []]

customTypeForComplexSumTy :: CustomType
customTypeForComplexSumTy = 
  let routeDefn = Route {getRoute = [Static "getStatus", Dynamic "withCons"]}
      dCons1 = DataConstructor "AvailableCons" [(Nothing, Ref (Local  routeDefn GET ["Available"]) "defName1" "AvlTyName" )]
      dCons2 = DataConstructor "PendingCons" [(Nothing, Ref (Local  routeDefn GET ["Pending"]) "defName2" "AvlTyName" )]
  in CustomType "ComplexSumTy" [dCons1, dCons2]

constructDeclFromCustomType :: CustomType -> Decl SrcSpanInfo
constructDeclFromCustomType (CustomType tyConstructor dataConsList) = 
  DataDecl noSrcSpan  
    (dataOrNewType False)
    Nothing 
    (declHead $ T.unpack tyConstructor)
    (qualConDecls dataConsList)
    []
 where
  dataOrNewType :: Bool -> DataOrNew SrcSpanInfo
  dataOrNewType isNewType =
    if isNewType
    then NewType noSrcSpan
    else DataType noSrcSpan

  declHead :: String -> DeclHead SrcSpanInfo
  declHead declHeadName = DHead noSrcSpan (nameDecl declHeadName) 

  qualConDecls :: [DataConstructor] -> [QualConDecl SrcSpanInfo]
  qualConDecls dataCons = fmap (\(DataConstructor dConsName mRecRefList) -> 
        case fst $ DL.unzip mRecRefList of
          mRecList -> 
            case catMaybes mRecList of
              -- SumType or NewType 
              -- TODO : NewType not handled yet!
              [] -> 
                let typeConsList = fmap (typeConstructor . showRefTy) $ snd $ DL.unzip mRecRefList
                in QualConDecl noSrcSpan Nothing Nothing 
                    (ConDecl noSrcSpan (nameDecl $ T.unpack dConsName) typeConsList)

              -- ProductType
              recVals -> do
                -- TODO: We should check that the 2 lists are of equal lengths?
                let typeNames = fmap showRefTy $ snd $ DL.unzip mRecRefList
                let recordNamesWithTypes = DL.zip (fmap T.unpack recVals) typeNames
                let fieldDecls = snd $ DL.unzip $ fmap fieldDecl recordNamesWithTypes
                QualConDecl noSrcSpan Nothing Nothing 
                  (RecDecl noSrcSpan (nameDecl $ T.unpack dConsName) fieldDecls)                
    ) dataCons
  

-- -- Product Types
-- [QualConDecl noSrcSpan Nothing Nothing (RecDecl noSrcSpan (nameDecl constructorName) fieldDecls )]

-- -- FOR Complex Sum Types -> A List of these
-- QualConDecl noSrcSpan Nothing Nothing 
-- (ConDecl noSrcSpan (nameDecl cName) [typeConstructor consType])

-- -- For Enums
-- fmap (\construcorVal -> QualConDecl noSrcSpan Nothing Nothing 
--       (ConDecl noSrcSpan 
--         (nameDecl construcorVal) [] ) )


showRefTy :: Ref -> String
showRefTy (Inline prim) = showPrimitiveTy prim
showRefTy (Ref _ _ tyCon) = T.unpack tyCon

showPrimitiveTy :: Primitive -> String
showPrimitiveTy prim =
  case prim of
    Date -> "Day"
    DateTime -> "UTCTime"
    Password -> "PASSWORD (TODO)"
    Byte -> "ByteString" 
    Binary -> "ByteString"
    Text -> "Text"
    Float -> "Float"
    Double -> "Double"
    Number -> "SwaggerNumber"
    Int -> "Int"
    Int32 -> "Int32"
    Int64 -> "Int64"
    Bool -> "Bool"
    File -> "File"
    Null -> "()"
    Default innerRef -> "Default " ++ (showRefTy innerRef)
    Maybe innerRef -> "Maybe " ++ (showRefTy innerRef)
    Array innerRef -> ("[" ++ (showRefTy innerRef) ++ "]")
    Tuple innerRefList -> "(" ++ (DL.intercalate "," (fmap showRefTy innerRefList) ) ++ ")"
    MultiSet innerRef -> "MultiSet " ++ (showRefTy innerRef)
    DelimitedCollection delimiter innerRef  -> 
      let delimChar = 
            case delimiter of
              SlashT -> "\"\t\" "
              Space -> "\" \"" 
              Pipe -> "\"|\""
              Comma -> "\",\" "
      in "DelimitedCollection" ++ delimChar ++ (showRefTy innerRef)
  
      



writeCabalAndProjectFiles :: FilePath -> String -> Bool -> [String] -> IO ()
writeCabalAndProjectFiles generationPath projectName needsWebapiXml modulesForImport = do
  writeFile (generationPath ++ projectName ++ ".cabal") (cabalFileContents projectName needsWebapiXml modulesForImport)
  writeFile (generationPath ++ "LICENSE") licenseFileContents
  -- TODO : Once webapi-xml is pushed to GitHub, it needs to be added to the cabal.project file
  writeFile (generationPath ++ "cabal.project") cabalProjectFileContents
                            --

stringLiteral :: String -> Exp SrcSpanInfo
stringLiteral str = (Lit noSrcSpan (LHE.String noSrcSpan str str))

variableName :: String -> Exp SrcSpanInfo
variableName nameStr = (Var noSrcSpan (UnQual noSrcSpan (nameDecl nameStr) ) )

nameDecl :: String -> Name SrcSpanInfo
nameDecl = Ident noSrcSpan 

typeConstructor :: String -> Type SrcSpanInfo
typeConstructor typeConName = (TyCon noSrcSpan  
                                (UnQual noSrcSpan $ nameDecl typeConName)
                              )

fieldDecl :: (String, String) -> (Maybe (String, String), FieldDecl SrcSpanInfo)
fieldDecl (fieldName, fieldType) = do
  let (isChanged, fName) = setValidFieldName fieldName
  let mModRecord = 
        case isChanged of
          True -> Just (fieldName, fName)
          False -> Nothing
  let fDecl = FieldDecl noSrcSpan [nameDecl fName] (TyCon noSrcSpan (UnQual noSrcSpan (nameDecl fieldType)))
  (mModRecord, fDecl)

languageExtension :: String -> ModulePragma SrcSpanInfo
languageExtension langExtName = LanguagePragma noSrcSpan [nameDecl langExtName]


moduleImport :: (String, (Bool, Maybe (ModuleName SrcSpanInfo)) )-> ImportDecl SrcSpanInfo
moduleImport (moduleNameStr, (isQualified, qualifiedName) ) = 
  ImportDecl {
              importAnn = noSrcSpan, 
              importModule = ModuleName noSrcSpan moduleNameStr,
              importQualified = isQualified,
              importSrc = False,
              importSafe = False,
              importPkg = Nothing,
              importAs = qualifiedName,
              importSpecs = Nothing
             }