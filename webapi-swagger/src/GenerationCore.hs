{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds             #-}


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
import qualified Data.Char as Char
import Data.Vector.Sized as SV hiding ((++), foldM, forM, mapM)
import Data.Finite.Internal
import Safe




runCodeGen :: FilePath -> FilePath -> String -> IO () 
runCodeGen swaggerJsonInputFilePath outputPath projectName = do
  removeDirectoryRecursive outputPath
  let projectFolderGenPath = outputPath ++ projectName ++ "/"
  let projectSrcDir = (projectFolderGenPath ++ "src/")
  createDirectoryIfMissing True projectSrcDir
  swaggerJSONContents <- liftIO $ BSL.readFile swaggerJsonInputFilePath
  let decodedVal = eitherDecode swaggerJSONContents <|> either (Left . show) Right (decodeEither' (BSL.toStrict swaggerJSONContents))
  case decodedVal of
    Left errMsg -> error $ errMsg -- "Panic: not a valid JSON or yaml"
    Right (swaggerData :: Swagger) -> do
      finalStateVal <- runSwaggerGenerator () (generateSwaggerState swaggerData)
      writeFile (projectSrcDir ++ "CommonTypes.hs") commonTypesModuleContent
      let tyState = typeState finalStateVal
      let routeStateHM = routeState finalStateVal
      let apiContractStateHM = apiContract finalStateVal
      modulesWithQualNames <- generateModulesFromTypeState tyState projectSrcDir 
      generateContractFromContractState "SwaggerContract" apiContractStateHM routeStateHM projectFolderGenPath modulesWithQualNames
      let moduleNamesOnly =  fst $ DL.unzip $ Set.toList modulesWithQualNames
      writeCabalAndProjectFiles projectFolderGenPath projectName False moduleNamesOnly

  
  -- get a list of all keys, groupBy Route to get list of routes.
  -- Use that list to generate route `type`s and `instance WebApi`

  -- each value of the HashMap maps to one `ApiContract` instance.
  -- construct RouteName using the same function (concat the Route pieces)
  -- fields of ContractInfo that are `Nothing`, `type` instance should be left out 
  -- 

  -- TODO : Import all generated Types modules in Contract

generateContractFromContractState :: String -> ContractState -> RouteState -> FilePath -> Set.Set (String, String) -> IO ()
generateContractFromContractState contractName contractState routeStateHM genDirPath moduleAndQualNames = do
  let routeListUnGrouped = HMS.keys contractState
  let groupedRoutes = ( (DL.groupBy (\(route, _) (route2, _) -> route == route2 ) ) . DL.sort ) routeListUnGrouped 
  let unParsedRoutePaths = fmap selectFirstRouteFromList groupedRoutes
  let refRoutePaths = fmap lookupRouteInRouteState unParsedRoutePaths
  let routeNames = fmap (constructRouteName) refRoutePaths

  let routeDecls = fmap routeDeclaration (DL.zip routeNames refRoutePaths)
  let ppRouteDecls = "\n\n" ++ (DL.concat $ fmap prettyPrint routeDecls)

  let routesWithMethods:: [(String, [Method])] = fmap getRouteAndMethod groupedRoutes
  let webapiInstDecl = webApiInstance contractName routesWithMethods
  let ppWebapiInstDecl = "\n\n" ++ (prettyPrint webapiInstDecl)
  let apiInstVectorList = HMS.foldlWithKey' generateContractTypeInsts [] contractState
  let apiContractInstDecls = fmap (\(topVector, tyVectorList) -> apiInstanceDeclaration topVector tyVectorList)   apiInstVectorList
  let apiContractInsts = fmap (\decl -> "\n\n" ++ (prettyPrint decl) ) apiContractInstDecls 


  let contractLangExts = fmap languageExtension langExtsForContract
  let routeTypeModuleImports = Set.toList moduleAndQualNames
  let allQualImportsForContract = routeTypeModuleImports ++ qualImportsForContract
  let contractImports = 
        fmap moduleImport 
          ( (DL.zip importsForContract (cycle [(False, Nothing)]) ) 
            ++ (fmap (\(fullModuleName, qual) ->  (fullModuleName, (True, Just $ ModuleName noSrcSpan qual)) ) allQualImportsForContract) ) 
    -- TODO : Add qualified imports for all Local modules (generate from HMS key and routeState)
    
  

  let allContractDecls = 
        [emptyDataDeclaration contractName] ++ routeDecls ++ [webapiInstDecl] ++ apiContractInstDecls


  let finalContractModule = 
        Module noSrcSpan 
          (Just $ ModuleHead noSrcSpan (ModuleName noSrcSpan "Contract") Nothing Nothing)
          (contractLangExts)
          (contractImports)
          (allContractDecls)
  writeFile (genDirPath ++ "src/Contract.hs") (prettyPrint finalContractModule)
  -- (fmap (\(topVec, innerVecList) -> 
  --   apiInstanceDeclaration topVec innerVecList ) $ DL.concat $ fmap (constructVectorForRoute contractName) contractDetails)


-- TODO 
-- 1. Qual in Types
-- 2. Import of Globals in Types modules
-- 3. Verify imports and qualifications in contracts

-- 4. Deletion of prev Generated folder
-- 5. Generation Path refer to RouteState for Routes
 


 where
  selectFirstRouteFromList :: [(Route UnparsedPiece, Method)]  -> Route UnparsedPiece
  selectFirstRouteFromList routeList = 
    case routeList of
      [] -> error $ "Expected atleast one element in Route ([RoutePiece])! "
      (firstElem, _):_ -> firstElem

  constructRouteName :: Route Ref -> String
  constructRouteName routeInfo =
    let routePieces = getRoute routeInfo
        routePiecesStr = prettifyRouteName routePieces
    in routePiecesStr ++ "R"

  lookupRouteInRouteState ::(Route UnparsedPiece) -> Route Ref
  lookupRouteInRouteState unparsedRoute = 
    case HMS.lookup unparsedRoute routeStateHM of
      Just refRoute -> refRoute
      Nothing -> error $ "Expected to find (Route UnparsedPiece) in RouteState HashMap! "


  getRouteAndMethod :: [(Route UnparsedPiece, Method)] -> (String, [Method])
  getRouteAndMethod routeList = 
    -- case routeList of
      -- [] -> error $ "Expected atleast one element in Route ([RoutePiece])! "
      -- rList -> ([], [])
    DL.foldl' (\(unparsedRoutePieceList, methodList) (currentRoutePieces, currentMethod) -> 
                    let routeNameStr = (constructRouteName . lookupRouteInRouteState) currentRoutePieces
                    in (routeNameStr, currentMethod:methodList)  ) ("", []) routeList
      
  prettifyRouteName :: [RoutePiece Ref] -> String
  prettifyRouteName routePieces = case routePieces of
    [] -> error "Expected atleast one RoutePiece in the route! Got an empty list!"
    (Static ""):[] -> "BaseRoute"
    rPieces -> DL.concat $ flip fmap rPieces (\routePiece -> 
          case routePiece of
            Static rPieceTxt -> T.unpack $ T.toTitle rPieceTxt
            Dynamic rPieceRef -> 
              let (firstChar:remainingChar) = (showRefTy Nothing) rPieceRef
              in (Char.toUpper firstChar):remainingChar )

  generateContractTypeInsts :: [(Vector 4 String, [Vector 4 String])] 
                            -> ((Route UnparsedPiece), Method) 
                            -> (ContractInfo TypeConstructor) 
                            -> [(Vector 4 String, [Vector 4 String])]
  generateContractTypeInsts acc (unparsedRoute, method) contractInfo = do
    let refRoute = lookupRouteInRouteState unparsedRoute
    let routeNameStr = constructRouteName refRoute
    let topLevelVector = fromMaybeSV $ SV.fromList ["W.ApiContract", contractName, qualMethodName method, routeNameStr]
    let typeInsts = parseContractInfo routeNameStr unparsedRoute contractInfo method 
    (topLevelVector, typeInsts):acc
  
  fromMaybeSV :: Maybe a -> a
  fromMaybeSV = fromJustNote "Expected a list with 4 elements for WebApi instance! "
  

qualMethodName :: Method -> String
qualMethodName = ("W." ++) . show

parseContractInfo :: String -> Route UnparsedPiece -> ContractInfo TypeConstructor -> Method -> [Vector 4 String]
parseContractInfo routeNameStr unpRouteInfo contractInfo method = 
  let mApiErr = fmap (\typeInfo -> constructVector "ApiErr" (insertQual typeInfo) ) (apiError contractInfo)
      mApiOut = fmap (\typeInfo -> constructVector "ApiOut" (insertQual typeInfo) ) (apiOutput contractInfo)
      mHeaderParam = fmap (\typeInfo -> constructVector "HeaderIn" (insertQual typeInfo) ) (headerParam contractInfo)
      mFormParam = fmap (\typeInfo -> constructVector "FormParam" (insertQual typeInfo) ) (formParam contractInfo)
      mQueryParam = fmap (\typeInfo -> constructVector "QueryParam" (insertQual typeInfo) ) (queryParam contractInfo)
      mFileParam = fmap (\typeInfo -> constructVector "FileParam" (insertQual typeInfo) ) (fileParam contractInfo)
      mRequestBody =
        -- '[Content [fstElem] tyCons-sndElem ]
        case requestBody contractInfo of
          [] -> Nothing
          contentAndReqBody -> 
            let qualForType = getQualOfRoute (getRoute unpRouteInfo) (Just method) ++ "."
                reqBodyFinalTy = "'[" ++ DL.intercalate "," (fmap (\(cTy, reqBodyTy) -> "Content [" ++ (T.unpack cTy) ++ "] " ++ qualForType ++ (T.unpack reqBodyTy) ) contentAndReqBody) ++ "]"
            in Just $ constructVector "RequestBody" reqBodyFinalTy 
        
      mContentTypes = 
        case contentTypes contractInfo of
          [] -> Nothing
          _ -> 
            let cTypes = "'[" ++ (T.unpack (T.intercalate "," (contentTypes contractInfo) ) ) ++ "]"
            in Just $ constructVector "ContentTypes" cTypes 

  in catMaybes [mApiErr, mApiOut, mHeaderParam, mFormParam, mQueryParam, mFileParam, mRequestBody, mContentTypes]



-- let r1 = Route {getRoute = [(Static "somePath"),(Static "andMorePathStuff")]}
-- let r2 = Route {getRoute = [(Static "somePathinR2"), (Static "andMorePathStuffForR2")]}
-- let r3 = Route {getRoute = [(Static "somePathinR3"), (Static "andMorePathStuffForR3")]}
-- let r4 = Route {getRoute = [(Static "somePathinR4"), (Static "andMorePathStuffForR4")]}



 where
  constructVector :: String -> String -> Vector 4 String
  constructVector typeLabel typeInfo = (fromMaybeSV $ SV.fromList [typeLabel, qualMethodName method, routeNameStr, typeInfo] )
  fromMaybeSV = fromJustNote "Expected a list with 4 elements for WebApi instance! "

  insertQual :: T.Text -> String
  insertQual inputType = 
    let routePieces = getRoute unpRouteInfo
    in (getQualOfRoute routePieces (Just method) ) ++ "." ++ (T.unpack inputType)
    

  -- getRouteFirstChar :: RoutePiece UnparsedPiece -> String
  -- getRouteFirstChar unparsedPiece =
  --   case unparsedPiece of
  --     Static txt -> 
  --       let (firstChar:_) = T.unpack txt
  --       in [firstChar]
  --     Dynamic _ -> ""


showRoutePiece :: RoutePiece UnparsedPiece -> String
showRoutePiece routePiece = 
  case routePiece of
    Static pieceTxt -> T.unpack pieceTxt
    Dynamic unpPiece -> T.unpack unpPiece

-- [((r1,POST), ContractInfo {}), ((r2, GET), ContractInfo {}), ((r3, GET), ContractInfo {}), ((r4, GET), ContractInfo {}), ((r1,GET), ContractInfo {})]
  

-- constructRouteNameFromRouteList :: Route UnparsedPiece -> String
-- constructRouteNameFromRouteList = undefined

generateModulesFromTypeState :: TypeState -> FilePath -> IO (Set.Set (String, String) )
generateModulesFromTypeState tState genPath = HMS.foldlWithKey' (parseStateAndGenerateFile genPath) (pure (Set.empty) ) tState
 where
  parseStateAndGenerateFile :: FilePath -> IO (Set.Set (String, String) ) -> TypeMeta -> TypeDefinition -> IO (Set.Set (String, String) )
  parseStateAndGenerateFile genDirPath ioModuleNames typeMeta typeDefn = do
    modAndQualNames <- ioModuleNames
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
    let (modName, modQualName) = 
          case typeMeta of
            ParamType _ routeInfo methodName -> 
              let qualName = getQualOfRoute (getRoute routeInfo) (Just methodName)  
              in (constructLocalTypeModuleName routeInfo (Just methodName), qualName)
            ResponseType _ routeInfo methodName -> 
              let qualName = getQualOfRoute (getRoute routeInfo) (Just methodName)  
              in (constructLocalTypeModuleName routeInfo (Just methodName), qualName)
            Definition provenance _ -> 
              case provenance of
                Global _ -> (globalTypesHsModuleName ++ globalDefnsModuleName, globalDefnsQualName)
                Local routeInfo methodName _ -> 
                  let qualName = getQualOfRoute (getRoute routeInfo) (Just methodName)  
                  in (constructLocalTypeModuleName routeInfo (Just methodName), qualName)
                RouteLocal routeInfo _ -> 
                  let qualName = getQualOfRoute (getRoute routeInfo) Nothing
                  in (constructLocalTypeModuleName routeInfo Nothing, qualName)
                  
    let currentProvenance = getProvenance typeMeta
                  
    case tyModuleExists of 
      True -> do
        let dataDecl = constructDeclFromCustomType currentProvenance $ customHaskType typeDefn
            newContents = "\n\n" ++ prettyPrint dataDecl
        appendFile (typesModuleDir ++ typesModuleName) newContents
        pure $ Set.insert (modName, modQualName) modAndQualNames
        -- pure (Set.insert modName moduleNames, Set.insert modQualName qualNames)
      False -> do
        let dataDecl = constructDeclFromCustomType currentProvenance $ customHaskType typeDefn
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
        pure $ Set.insert (modName, modQualName) modAndQualNames
        -- pure (Set.insert modName moduleNames, Set.insert modQualName qualNames)
      
  parseRouteIntoFolderName :: Route UnparsedPiece -> String
  parseRouteIntoFolderName routeInfo =
    case getRoute routeInfo of
      [] -> error $ "Encountered empty Route! Expected the route to contain atleast one piece!"
      routeInfoList -> DL.intercalate "/" $ fmap (validateRouteDirectoryPath . showRoutePiece) routeInfoList


  parseRouteIntoModuleName :: Route UnparsedPiece -> String
  parseRouteIntoModuleName routeInfo =
    case getRoute routeInfo of
      [] -> error $ "Encountered empty Route! Expected the route to contain atleast one piece!"
      routeInfoList -> DL.intercalate "." $ fmap (validateRouteModuleName . showRoutePiece) routeInfoList      


  qualTyModuleImports :: [(String, (Bool, Maybe (ModuleName SrcSpanInfo)))]
  qualTyModuleImports =
    let qualImportList = qualifiedImportsForTypesModule    
    in fmap (\(fullModuleName, qual) ->  (fullModuleName, (True, Just $ ModuleName noSrcSpan qual)) ) qualImportList

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

  
validateRouteDirectoryPath :: String -> String
validateRouteDirectoryPath = setValidConstructorId

validateRouteModuleName :: String -> String
validateRouteModuleName = setValidConstructorId
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

constructDeclFromCustomType :: Provenance -> CustomType -> Decl SrcSpanInfo
constructDeclFromCustomType currentProvenance (CustomType tyConstructor dataConsList) = 
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
                let typeConsList = fmap (typeConstructor . (showRefTy (Just currentProvenance) ) ) $ snd $ DL.unzip mRecRefList
                in QualConDecl noSrcSpan Nothing Nothing 
                    (ConDecl noSrcSpan (nameDecl $ T.unpack dConsName) typeConsList)

              -- ProductType
              recVals -> do
                -- TODO: We should check that the 2 lists are of equal lengths?
                let typeNames = fmap (showRefTy (Just currentProvenance) ) $ snd $ DL.unzip mRecRefList
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


showRefTy :: Maybe Provenance -> Ref -> String
showRefTy mCurrentProv (Inline prim) = showPrimitiveTy mCurrentProv prim
showRefTy mCurrentProv (Ref provenance _ tyCon) = printWithQual provenance mCurrentProv tyCon

showPrimitiveTy :: Maybe Provenance -> Primitive -> String
showPrimitiveTy mCurrentProv prim =
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
    Default innerRef -> "Default " ++ (showRefTy mCurrentProv innerRef)
    Maybe innerRef -> "Maybe " ++ (showRefTy mCurrentProv innerRef)
    Array innerRef -> ("[" ++ (showRefTy mCurrentProv innerRef) ++ "]")
    Tuple innerRefList -> "(" ++ (DL.intercalate "," (fmap (showRefTy mCurrentProv) innerRefList) ) ++ ")"
    MultiSet innerRef -> "MultiSet " ++ (showRefTy mCurrentProv innerRef)
    DelimitedCollection delimiter innerRef  -> 
      let delimChar = 
            case delimiter of
              SlashT -> "\"\t\" "
              Space -> "\" \"" 
              Pipe -> "\"|\""
              Comma -> "\",\" "
      in "DelimitedCollection" ++ delimChar ++ (showRefTy mCurrentProv innerRef)
  
printWithQual :: Provenance -> Maybe Provenance -> T.Text -> String
printWithQual provenance mCurrentInpProv typeTxt = 
  case mCurrentInpProv of
    Just prov ->
        case isSameModule provenance prov of 
          True -> T.unpack typeTxt
          False ->
              case provenance of
                Global _ -> "Defns." ++ (T.unpack typeTxt)
                RouteLocal unparsedRouteInfo _ -> (getQualOfRoute (getRoute unparsedRouteInfo) Nothing) ++ "." ++ (T.unpack typeTxt)
                Local unparsedRouteInfo method _ -> (getQualOfRoute (getRoute unparsedRouteInfo) (Just method) ) ++ "." ++ (T.unpack typeTxt)
    Nothing -> (T.unpack typeTxt)
        
--  where


getQualOfRoute :: [RoutePiece UnparsedPiece] -> Maybe Method -> String
getQualOfRoute unpRoutePieces mMethod = 
  let methodTxt = 
        case mMethod of
          Just method -> show method 
          Nothing -> ""
      firstCharsOfRoutePaths = fmap (getFirstChar . validateRouteDirectoryPath . showRoutePiece) unpRoutePieces
  in firstCharsOfRoutePaths ++ methodTxt
 where
  getFirstChar :: String -> Char
  getFirstChar inpStr = 
    case inpStr of
      [] -> error $ "Encountered empty Route Piece! Debug Info: " ++ (show unpRoutePieces)
      firstChar:_ -> firstChar



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



routeDeclaration :: (String, Route Ref) -> Decl SrcSpanInfo
routeDeclaration (routeNameStr, routePathComponents) = do
  let routePieceList = getRoute routePathComponents
  case routePieceList of
    (Static _):[] -> 
      TypeDecl noSrcSpan
        (declarationHead routeNameStr)
        (TyApp noSrcSpan
          (typeConstructor "W.Static")
          (recursiveTypeForRoute routePieceList) )
    _ -> 
      TypeDecl noSrcSpan  
        (declarationHead routeNameStr)
        (recursiveTypeForRoute routePieceList)


recursiveTypeForRoute :: [RoutePiece Ref] -> Type SrcSpanInfo
recursiveTypeForRoute routeComponents = 
  case routeComponents of
    [] -> error "Did not expect an empty list here! "
    x:[] -> processPathComponent x
    prevElem:lastElem:[] -> 
      (TyInfix noSrcSpan 
        (processPathComponent prevElem)
        (unPromotedUnQualSymDecl "W.:/")
        (processPathComponent lastElem)
      )
    currentRoute:remainingRoute -> 
      (TyInfix noSrcSpan 
        (processPathComponent currentRoute)
        (unPromotedUnQualSymDecl "W.:/")
        (recursiveTypeForRoute remainingRoute)
      )   
 where 
  processPathComponent :: RoutePiece Ref -> Type SrcSpanInfo
  processPathComponent refRoutePiece = 
    case refRoutePiece of
      Static staticPathPiece -> promotedType $ T.unpack staticPathPiece
      Dynamic pType -> typeConstructor $ showRefTy Nothing pType

promotedType :: String -> Type SrcSpanInfo
promotedType typeNameData = 
  (TyPromoted noSrcSpan 
    (PromotedString noSrcSpan typeNameData typeNameData) 
  ) 

instanceHead :: String -> InstHead SrcSpanInfo
instanceHead instName = (IHCon noSrcSpan
                          (UnQual noSrcSpan $ nameDecl instName)
                        ) 

declarationHead :: String -> DeclHead SrcSpanInfo
declarationHead declHeadName = (DHead noSrcSpan (Ident noSrcSpan declHeadName) )
                        
#if MIN_VERSION_haskell_src_exts(1,20,0)
unPromotedUnQualSymDecl :: String -> MaybePromotedName SrcSpanInfo
unPromotedUnQualSymDecl str =
  (UnpromotedName noSrcSpan
   (UnQual noSrcSpan
    (Symbol noSrcSpan str)
   ))
#else
unPromotedUnQualSymDecl :: String -> QName SrcSpanInfo
unPromotedUnQualSymDecl = unQualSymDecl
#endif


webApiInstance :: String -> [(String, [Method])] -> Decl SrcSpanInfo
webApiInstance mainTypeName routeAndMethods =
  InstDecl noSrcSpan Nothing 
    (IRule noSrcSpan Nothing Nothing 
      (IHApp noSrcSpan 
        (instanceHead "W.WebApi")
        (typeConstructor mainTypeName) 
      )
    ) 
    (Just 
      [InsType noSrcSpan 
        (TyApp noSrcSpan 
          (typeConstructor "Apis")
          (typeConstructor mainTypeName)
        ) 
        (TyPromoted noSrcSpan 
          (PromotedList noSrcSpan True 
            (fmap innerRouteInstance routeAndMethods)
          )
        )
      ]
    )
 where
  innerRouteInstance :: (String, [Method]) -> Type SrcSpanInfo
  innerRouteInstance (rName, listOfMethods) =
      TyApp noSrcSpan 
        (TyApp noSrcSpan 
          (typeConstructor "W.Route") 
          (TyPromoted noSrcSpan 
            (PromotedList noSrcSpan True 
              (fmap (typeConstructor . qualMethod) listOfMethods)               
            )
          )
        ) 
        (typeConstructor rName)
      
  qualMethod :: Method -> String
  qualMethod = ("W." ++) . show


apiInstanceDeclaration :: Vector 4 String -> [Vector 4 String] -> Decl SrcSpanInfo
apiInstanceDeclaration topLevelDecl innerTypesInstList = 
  InstDecl noSrcSpan Nothing 
    (IRule noSrcSpan Nothing Nothing
      (IHApp noSrcSpan 
        (IHApp noSrcSpan 
          (IHApp noSrcSpan 
            ( instanceHead (SV.index topLevelDecl (Finite 0) ) )
            (typeConstructor $ SV.index topLevelDecl (Finite 1)  )
          )
          (typeConstructor $ SV.index topLevelDecl (Finite 2)  )
        ) 
        (typeConstructor $ SV.index topLevelDecl (Finite 3) )
      ) 
    ) (Just $ fmap apiInstanceTypeDecl innerTypesInstList)


apiInstanceTypeDecl :: Vector 4 String -> InstDecl SrcSpanInfo 
apiInstanceTypeDecl innerTypes =
  InsType noSrcSpan
    (TyApp noSrcSpan
        (TyApp noSrcSpan
          (typeConstructor (SV.index innerTypes (Finite 0) ) )
          (typeConstructor (SV.index innerTypes (Finite 1) ) )
        )
      (typeConstructor (SV.index innerTypes (Finite 2) ) )
    )
    (typeConstructor (SV.index innerTypes (Finite 3) ) )


emptyDataDeclaration :: String -> Decl SrcSpanInfo
emptyDataDeclaration declName = 
  DataDecl noSrcSpan 
    (DataType noSrcSpan) 
    Nothing
    (declarationHead declName) 
    []
#if MIN_VERSION_haskell_src_exts(1,20,0)
    []
#else
    Nothing
#endif