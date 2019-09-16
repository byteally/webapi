{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE BangPatterns #-}



module ContractGen where

import Data.Aeson 
import Data.Text as T
import qualified Data.ByteString.Lazy as BSL
import Data.HashMap.Strict.InsOrd as HMSIns
import Language.Haskell.Exts as LHE hiding (OPTIONS)
import Data.Vector.Sized as SV hiding ((++), foldM, forM, mapM)
import Safe
import Data.Finite.Internal
-- import Network.HTTP.Types.Method
import Data.Maybe
import Data.List.Split as DLS (splitOn)
import qualified Data.List as DL
import qualified Data.Map.Lazy as Map
import qualified Data.Char as Char
import Control.Monad
import Control.Monad.Trans.State.Strict
import Control.Monad.IO.Class
import System.Directory
-- import Data.String.Interpolate
import Data.Swagger hiding (get, paramSchema)
import Data.Yaml (decodeEither')
import Control.Applicative ((<|>))

import ContractGenTypes
import Constants
import qualified Data.HashMap.Strict as HMS
import qualified SwaggerGen as SG hiding (Tuple)


import Debug.Trace as DT


runDefaultPathCodeGen :: IO ()
runDefaultPathCodeGen = runCodeGen "sampleFiles/swagger-petstore-noXml.json" "/Users/kahlil/projects/ByteAlly/tmp/" "swagger-gen-proj"


runCodeGen :: FilePath -> FilePath -> String -> IO () 
runCodeGen swaggerJsonInputFilePath outputPath projectName = do
  let projectFolderGenPath = outputPath ++ projectName ++ "/"
  createDirectoryIfMissing True (projectFolderGenPath ++ "src/")
  (globalModuleNames, newTypeCreationList) <- runStateT (readSwaggerGenerateDefnModels swaggerJsonInputFilePath projectFolderGenPath projectName)  HMS.empty
  createNewTypes newTypeCreationList projectFolderGenPath globalModuleNames
  writeFile (projectFolderGenPath ++ "src/CommonTypes.hs") commonTypesModuleContent
  
 where
  createNewTypes :: HMS.HashMap LevelInfo [TypeInfo] -> FilePath -> [String] -> IO ()
  createNewTypes stateHM genPath globalModules = do
    createdModuleNames <- HMS.foldlWithKey' (writeGeneratedTypesToFile genPath) (pure globalModules) stateHM
    -- TODO : Setting xmlImport to False for now by default since it's not in scope!
    writeCabalAndProjectFiles genPath projectName False (DL.nub createdModuleNames)

createTypeDeclFromCDT :: [Decl SrcSpanInfo] -> (CreateDataType, NamingCounter) -> [Decl SrcSpanInfo]
createTypeDeclFromCDT accValue (tyInfo, mNameCounter) = do
  let typeInfo = addCtrToConsName mNameCounter tyInfo
  case typeInfo of
    ProductType newData _ -> do
      let toParamInstances =
            case (DL.isInfixOf "QueryParam" $ mName newData) of
              True -> [defaultToParamInstance (mName newData) "QueryParam"] 
              False -> 
                case (DL.isInfixOf "FormParam" $ mName newData) of
                  True -> [defaultToParamInstance (mName newData) "FormParam"]
                  False -> []
      let (modifiedRecords, dataDecl) = dataDeclaration (DataType noSrcSpan) (mName newData) (Right $ mRecordTypes newData) ["P.Eq", "P.Show", "P.Generic"] 
      -- TODO:  Commenting out all Instances for now
      -- let jsonInsts = jsonInstances (mName newData) modifiedRecords
      accValue ++ [dataDecl] ++ [] ++ [] ++ [] -- [defaultToSchemaInstance (mName newData)]
    SumType (BasicEnum tName tConstructors ogConstructors) -> do
      let toParamEncodeParamQueryParamInstance = [toParamQueryParamInstance tName] ++ [encodeParamSumTypeInstance tName (DL.zip tConstructors ogConstructors ) ]
      let fromParamDecodeParamQueryParamInstance = [fromParamQueryParamInstance tName] ++ [decodeParamSumTypeInstance tName (DL.zip ogConstructors tConstructors ) ]
      let toSchemaInstances = toSchemaInstanceForSumType tName (DL.zip ogConstructors tConstructors ) 
      accValue ++ 
        ([enumTypeDeclaration tName tConstructors ["P.Eq", "P.Show","P.Generic"] ]) 
          -- ++ (instanceDeclForShow tName) 
          -- ++ (instanceDeclForJSONForSumType tName) 
          -- ++ toParamEncodeParamQueryParamInstance 
          -- ++ fromParamDecodeParamQueryParamInstance 
          -- ++ toSchemaInstances)
    SumType (ComplexSumType tName constructorTypeList ) -> do
      accValue ++ 
        ([complexSumTypeDecl tName constructorTypeList ["P.Eq", "P.Generic", "P.Show"] ])
          -- ++ jsonInstances tName [] )
    HNewType tName alias _ -> accValue ++ [snd $ dataDeclaration (NewType noSrcSpan) (tName) (Left alias)  ["P.Eq", "P.Show", "P.Generic"] ]
 where
  addCtrToConsName :: Maybe Int -> CreateDataType -> CreateDataType
  addCtrToConsName mCounterVal cdt = 
    case mCounterVal of
      Just counterVal -> 
        case cdt of
          SumType (BasicEnum consName names ogNames) -> SumType (BasicEnum (consName ++ (show counterVal) ) names ogNames) 
          SumType (ComplexSumType consName consList ) -> SumType (ComplexSumType (consName ++ (show counterVal) ) consList )
          ProductType (NewData consName recList ) ogName -> ProductType (NewData (consName ++ (show counterVal) ) recList ) ogName
          HNewType consName ty ogName -> HNewType (consName ++ (show counterVal) ) ty ogName
      Nothing -> cdt
  traceVal :: Maybe Int -> CreateDataType -> String
  traceVal mCounterVal cdt = 
    case mCounterVal of
      Just x -> "CDT : " ++ (show cdt) ++ "\t Counter Val : " ++ (show x)
      Nothing -> ""
  
qualifiedGlobalImports :: [String] -> [(String, (Bool, Maybe (ModuleName SrcSpanInfo)))]                               
qualifiedGlobalImports moduleList =
  let moduleWithQuals = fmap (\modName -> 
            if DL.isInfixOf globalDefnsModuleName modName 
            then (modName, globalDefnsQualName)
            else if DL.isInfixOf globalRespTypesModuleName modName
                 then (modName, globalRespTypesQualName)
                 else if DL.isInfixOf globalParamTypesModuleName modName
                      then (modName, globalParamsQualName)
                      else error "Expected one of the 3 Global Defn modules!") (DL.nub moduleList)
        -- TODO : find out how duplicate entries are coming into imports so we can remove the above call to `nub`
  in fmap (\(modName, qualName) -> (modName, (True, Just $ ModuleName noSrcSpan qualName) )  ) moduleWithQuals


readSwaggerGenerateDefnModels :: FilePath -> FilePath -> String -> StateT (HMS.HashMap LevelInfo [TypeInfo]) IO [String]
readSwaggerGenerateDefnModels swaggerJsonInputFilePath contractOutputFolderPath projectName = do 
  swaggerJSONContents <- liftIO $ BSL.readFile swaggerJsonInputFilePath
  let decodedVal = eitherDecode swaggerJSONContents <|> either (Left . show) Right (decodeEither' (BSL.toStrict swaggerJSONContents))
  case decodedVal of
    Left errMsg -> error $ errMsg -- "Panic: not a valid JSON or yaml"
    Right (swaggerData :: Swagger) -> do
      (apiNameHs, contractDetails) <- getSwaggerData swaggerData
      let xmlImport = needsXmlImport contractDetails      
      newDefnTypesHM <- generateSwaggerDefinitionData (_swaggerDefinitions swaggerData) 
      globalResponseTypesHM <- generateGlobalResponseData (_swaggerResponses swaggerData)
      globalParamTypesHM <- generateGlobalParamData (_swaggerParameters swaggerData)

      -- We ignore the keys (level info) and just concat all the CDTs 
      -- let newDefnCDTList = fmap (getInnerTyFromTypeInfo) $ DL.concat $ HMS.elems newDefnTypesHM
      -- modify' (\stateValue -> DT.trace ("Adding Defns to State!") $ HMS.unionWith (++) stateValue newDefnTypesHM );
      let langExts = ["TypeFamilies", "MultiParamTypeClasses", "DeriveGeneric", "TypeOperators", "DataKinds", "TypeSynonymInstances", "FlexibleInstances"]
      let contractImports = ["Types", "Data.Int", "Data.Text"]
      let qualifiedImportsForContract =
            let webApiImports = [  ("WebApi.Contract", (True, Just $ ModuleName noSrcSpan "W"))
                                ,  ("WebApi.Param", (True, Just $ ModuleName noSrcSpan "W"))
                                ]
                webApiXmlImp  = ("WebApi.XML", (True, Just $ ModuleName noSrcSpan "W"))
            in if xmlImport
               then webApiXmlImp : webApiImports
               else webApiImports
                    
      let hContractModule = 
            Module noSrcSpan 
              (Just $ ModuleHead noSrcSpan (ModuleName noSrcSpan "Contract") Nothing Nothing)
              (fmap languageExtension langExts)
              -- ((fmap (\modName -> moduleImport (modName,(False, Nothing)) ) contractImports) -- CommonTypes
                -- ++ fmap moduleImport qualifiedImportsForContract)
              -- (generateContractBody apiNameHs contractDetails)
              --  TODO : Setting contract to be empty for now
              []
              []
      liftIO $ writeFile (contractOutputFolderPath ++ "src/Contract.hs") $ prettyPrint hContractModule
      -- let qualifiedImportsForTypes = 
      --       [("Data.ByteString.Char8", (True, Just $ ModuleName noSrcSpan "ASCII")), 
      --       ("Data.HashMap.Lazy", (True, Just $ ModuleName noSrcSpan "HM") ),
      --       ("Data.Swagger", (True, Just $ ModuleName noSrcSpan "SW") ), 
      --       ("Data.Text", (True, Just $ ModuleName noSrcSpan "P") ),
      --       ("Data.Int", (True, Just $ ModuleName noSrcSpan "P") ),
      --       ("Data.Time.Clock", (True, Just $ ModuleName noSrcSpan "P") ),
      --       ("GHC.Generics", (True, Just $ ModuleName noSrcSpan "P") ),
      --       ("Data.Aeson", (True, Just $ ModuleName noSrcSpan "P") ),
      --       ("WebApi.Param", (True, Just $ ModuleName noSrcSpan "P") ),
      --       ("Data.Text.Encoding", (True, Just $ ModuleName noSrcSpan "P") ),
      --       ("Prelude", (True, Just $ ModuleName noSrcSpan "P") )
      --       ]
      -- let hTypesModule = 
      --       Module noSrcSpan 
      --           (Just $ ModuleHead noSrcSpan (ModuleName noSrcSpan "Types") Nothing Nothing)
      --           (fmap languageExtension ["TypeFamilies", "MultiParamTypeClasses", "DeriveGeneric", "TypeOperators", "DataKinds", "TypeSynonymInstances", "FlexibleInstances", "DuplicateRecordFields", "OverloadedStrings"])
      --           (fmap moduleImport 
      --             ( (DL.zip ["Prelude ()",
      --                       "Data.Swagger.Schema", 
      --                       "CommonTypes",
      --                       "Control.Lens",
      --                       "Data.Swagger.Internal.Schema",
      --                       "Data.Swagger.ParamSchema",
      --                       -- TODO : This is kind of a hack!
      --                       "Data.Swagger.Internal hiding (Tag)"
      --                       ] (cycle [(False, Nothing)]) ) ++ qualifiedImportsForTypes ) ) --"GHC.Generics", "Data.Time.Calendar"
      --           (createDataDeclarations newDefnCDTList)
      let respsAndDefns = HMS.unionWith (++) globalResponseTypesHM newDefnTypesHM
      let globalTypesWithParams = HMS.unionWith (++) respsAndDefns globalParamTypesHM
      liftIO $ do
        -- writeFile (contractOutputFolderPath ++ "src/Types.hs") $ prettyPrint hTypesModule ++ "\n\n"
        HMS.foldlWithKey' (writeGeneratedTypesToFile contractOutputFolderPath) (pure []) globalTypesWithParams 
        -- createdModuleNames <- 
    
 where 
  
  needsXmlImport :: [ContractDetails] -> Bool
  needsXmlImport = flip DL.foldl' False (\accBool cDetail -> 
              case accBool of
                True -> True
                False ->
                  let methodMap = methodData cDetail
                  in Map.foldl' (\innerAcc apiDetails -> hasXML apiDetails || innerAcc) accBool methodMap )
 
              
-- createDataDeclarations :: [CreateDataType] -> [Decl SrcSpanInfo]
-- createDataDeclarations = DL.foldl' createTypeDeclFromCDT [] 
-- (\accValue cNewTy -> 
--   case cNewTy of
--     ProductType newDataInfo -> 
--       let (modifiedRecords, dataDecl) = dataDeclaration (DataType noSrcSpan) (mName newDataInfo) (Right $ mRecordTypes newDataInfo) ["P.Eq", "P.Show", "P.Generic"]
--           jsonInsts = jsonInstances (mName newDataInfo) modifiedRecords
--       in accValue ++ [dataDecl] ++ jsonInsts ++ [defaultToSchemaInstance (mName newDataInfo)] 
--     HNewType tName alias -> (snd $ dataDeclaration (NewType noSrcSpan) (tName) (Left alias)  ["Eq", "Show", "Generic"] ):accValue 
--     SumType _ -> error $ "Encountered a Sum Type creation while constructing initial types for Types.hs "
--       ++ "\n The value is : " ++ (show cNewTy)   ) [] newDataList



writeGeneratedTypesToFile :: FilePath  -> IO [String] -> LevelInfo -> [TypeInfo] -> IO [String]
writeGeneratedTypesToFile genPath ioModuleNames levelInfo typeInfos = do
  moduleNames <- ioModuleNames
  let (typesModuleDir, typesModuleName) = 
        case levelInfo of
          Global gType -> 
            case gType of
              DefinitionTy -> (genPath ++ globalTypesModulePath, hsModuleToFileName globalDefnsModuleName)
              ResponseTy -> (genPath ++ globalTypesModulePath, hsModuleToFileName globalRespTypesModuleName )
              ParamTy -> (genPath ++ globalTypesModulePath, hsModuleToFileName globalParamTypesModuleName)
          Local _ (rName, stdMethod) -> 
            ( genPath ++ (localRouteMethodTypesPath rName stdMethod), hsModuleToFileName localRouteMethodTypesModuleName)

  tyModuleExists <- doesFileExist (typesModuleDir ++ typesModuleName) 
  let (modName:: String) = 
        case levelInfo of
          Global glType ->
            case glType of
              DefinitionTy -> globalTypesHsModuleName ++ globalDefnsModuleName
              ResponseTy -> globalTypesHsModuleName ++ globalRespTypesModuleName
              ParamTy -> globalTypesHsModuleName ++ globalParamTypesModuleName
          Local _ (rtName, sMethod) -> (localRouteMethodTypesModName rtName sMethod) ++ localRouteMethodTypesModuleName
  case tyModuleExists of 
    True -> do
      let createDataTyList = fmap (getInnerTyAndCtr) typeInfos
          newContents = "\n\n" ++ (DL.concat $ fmap (++ "\n\n" ) $ fmap prettyPrint $ createDataDeclarations createDataTyList)
      appendFile (typesModuleDir ++ typesModuleName) newContents
      pure $ modName:moduleNames
    False -> do
      let createDataTyList = fmap (getInnerTyAndCtr) typeInfos
          newTyModuleContents = 
            prettyPrint $ 
              Module noSrcSpan 
                    (Just $ ModuleHead noSrcSpan (ModuleName noSrcSpan modName) Nothing Nothing)
                    (fmap languageExtension languageExtensionsForTypesModule)
                    (fmap moduleImport 
                      ( (DL.zip importsForTypesModule (cycle [(False, Nothing)]) ) 
                         ++ (fmap (\(fullModuleName, qual) ->  (fullModuleName, (True, Just $ ModuleName noSrcSpan qual)) ) qualifiedImportsForTypesModule) ) )-- ++ ( qualifiedGlobalImports (getGlobalModuleNames moduleNames) ) ) )
                    (createDataDeclarations $ createDataTyList)

      createDirectoryIfMissing True typesModuleDir
      writeFile (typesModuleDir ++ typesModuleName) newTyModuleContents
      pure $ modName:moduleNames

 where
  createDataDeclarations :: [(CreateDataType, NamingCounter)] -> [Decl SrcSpanInfo]
  createDataDeclarations = DL.foldl' createTypeDeclFromCDT []  

  getGlobalModuleNames :: [String] -> [String]
  getGlobalModuleNames = DL.filter (DL.isInfixOf ".GlobalDefinitions.") 
  




-- TODO: This function assumes SwaggerObject to be the type and directly reads from schemaProperties. We need to also take additionalProperties into consideration.
generateSwaggerDefinitionData :: InsOrdHashMap Text Schema -> StateT (HMS.HashMap LevelInfo [TypeInfo]) IO (HMS.HashMap LevelInfo [TypeInfo])
generateSwaggerDefinitionData defDataHM = foldlWithKey' parseSwaggerDefinition (pure HMS.empty) defDataHM
 where 
  parseSwaggerDefinition :: StateT (HMS.HashMap LevelInfo [TypeInfo]) IO (HMS.HashMap LevelInfo [TypeInfo]) -> Text -> Schema -> StateT (HMS.HashMap LevelInfo [TypeInfo]) IO (HMS.HashMap LevelInfo [TypeInfo])
  parseSwaggerDefinition scAccValue modelName modelSchema = do 
    accValue <- scAccValue 
    let (schemaProperties::InsOrdHashMap Text (Referenced Schema) ) = _schemaProperties modelSchema
    case HMSIns.null schemaProperties of
      True -> do
        hsType <- getTypeFromSwaggerType (Global DefinitionTy) DefinitionI (T.unpack modelName) (Just modelSchema) (_schemaParamSchema modelSchema)
        if hsType == (T.unpack modelName)
        -- If the name of the type returned is the same, it would mean that it's a sum type. 
        -- An alias is not necessary here as the sum type details would be stored in the State
        -- And the type will be generated later when the State value is read.
        then pure accValue
        -- Along with the Restructuring of Generated types into modules, we have scrapped `TypeAlias` and replaced it with `NewType`
        else 
          let createTypeInfo = HNewType (setValidConstructorId $ T.unpack modelName) hsType (T.unpack modelName)
          in pure $ HMS.insertWith checkForDuplicateAndAdd (Global DefinitionTy) [DefinitionType createTypeInfo Nothing] accValue
      False -> do
        prodType <- parseSchemaToCDT (Global DefinitionTy) DefinitionI modelName modelSchema
        pure $ HMS.insertWith checkForDuplicateAndAdd (Global DefinitionTy) [DefinitionType prodType Nothing] accValue

parseSchemaToCDT :: LevelInfo -> TInfo -> Text -> Schema -> StateConfig (CreateDataType)
parseSchemaToCDT levelInfo tInfo mainTypeName ilSchema = do
  let mandatoryFields = fmap T.unpack (_schemaRequired ilSchema)
  recordNamesAndTypes <- foldlWithKey' (\scAccList innerRecord iRefSchema -> do 
          accList <- scAccList
          let innerRecordName = T.unpack innerRecord
          let innerRecordTypeName = T.unpack $ T.append (T.toTitle mainTypeName) (T.toTitle innerRecord)
          innerRecordType <- case iRefSchema of
                  Ref referenceName -> pure $ T.unpack $ getReference referenceName
                  Inline irSchema -> ((getTypeFromSwaggerType levelInfo tInfo innerRecordTypeName (Just irSchema)) . _schemaParamSchema) irSchema
          let recordTypeWithMaybe = 
                case (innerRecordName `DL.elem` mandatoryFields) of 
                  True -> setValidConstructorId innerRecordType
                  False -> "P.Maybe " ++ innerRecordType
          pure $ (innerRecordName, recordTypeWithMaybe):accList ) (pure []) (_schemaProperties ilSchema)
  pure $ ProductType (NewData (setValidConstructorId $ T.unpack mainTypeName) recordNamesAndTypes) (T.unpack mainTypeName)


generateGlobalResponseData :: InsOrdHashMap Text Response -> StateConfig (HMS.HashMap LevelInfo [TypeInfo])
generateGlobalResponseData globalRespHM = foldlWithKey' parseResponseDefn (pure HMS.empty) globalRespHM
 where
  parseResponseDefn :: StateConfig (HMS.HashMap LevelInfo [TypeInfo]) -> Text -> Response -> StateConfig (HMS.HashMap LevelInfo [TypeInfo])
  parseResponseDefn scAccValue responseDefName responseObj = do
    accValue <- scAccValue
    case _responseSchema responseObj of
      Just (Ref refSchema) -> do
        let refText = getReference refSchema 
        -- NOTE : We will assume that any references here are only to Definitions types.
        let respDataTy = HNewType (setValidConstructorId $ T.unpack responseDefName) (T.unpack refText) (T.unpack responseDefName)
        -- TODO: Verify if DefinitionType is okay here.
        let newRespHM = HMS.singleton (Global ResponseTy) [DefinitionType respDataTy Nothing]
        pure $ HMS.unionWith checkForDuplicateAndAdd accValue newRespHM
      Just (Inline ilSchema) -> do
        let levelInfo = Global ResponseTy
        cdt <- parseSchemaToCDT levelInfo DefinitionI responseDefName ilSchema
        pure $ HMS.insertWith checkForDuplicateAndAdd levelInfo [DefinitionType cdt Nothing] accValue
      -- TODO: we should probably log this in the error reporting as it doesn't make much sense if it's a `Nothing`
      Nothing -> scAccValue

generateGlobalParamData :: InsOrdHashMap Text Param -> StateConfig (HMS.HashMap LevelInfo [TypeInfo])
generateGlobalParamData globalParamsHM = pure $ HMS.empty
--   foldlWithKey' parseParamDefn (pure HMS.empty) globalParamsHM
--  where
--   parseParamDefn :: StateConfig (HMS.HashMap LevelInfo [TypeInfo]) -> Text -> Param -> StateConfig (HMS.HashMap LevelInfo [TypeInfo])
--   parseParamDefn scAccValue paramDefName paramObj = do
--     accValue <- scAccValue


getSwaggerData :: Swagger -> StateT (HMS.HashMap LevelInfo [TypeInfo]) IO (String, [ContractDetails])
getSwaggerData swaggerData = do
 let apiNameFromSwagger = (_infoTitle . _swaggerInfo) swaggerData 
     validHsApiName = setValidConstructorId (T.unpack apiNameFromSwagger)
 contractDetailList <- HMSIns.foldlWithKey' (parseSwaggerPaths swaggerData) (pure []) (_swaggerPaths swaggerData)
 pure (validHsApiName, contractDetailList)

 where
  parseSwaggerPaths :: Swagger -> StateT (HMS.HashMap LevelInfo [TypeInfo]) IO [ContractDetails] -> FilePath -> PathItem -> StateT (HMS.HashMap LevelInfo [TypeInfo]) IO [ContractDetails]
  parseSwaggerPaths swaggerData contractDetailsList swFilePath swPathDetails = do
    let (refParamsHM:: InsOrdHashMap Text Param) = _swaggerParameters swaggerData
    cDetailsList <- contractDetailsList
    let swaggerPath ::[SwPathComponent] = fmap constructSwPathComps $ DLS.splitOn "/" $ removeLeadingSlash swFilePath
        mainRouteName = setValidConstructorId $ (prettifyRouteName swaggerPath) ++ "R"

    -- TODO: Add a `Static` Type component at the start if the list has just one element of type PathComp
    finalPathWithParamTypes::[PathComponent] <- forM swaggerPath (\pathComponent -> 
        case pathComponent of 
          PathParamName pathParamName -> do
            let pathLvlParams = _pathItemParameters swPathDetails
            (mParamNameList::[Maybe String]) <- mapM (getPathParamTypeFromOperation mainRouteName pathParamName refParamsHM pathLvlParams) (getListOfPathOperations swPathDetails::[(SG.Method, Maybe Operation)])
            case (DL.nub . catMaybes) mParamNameList of
              [] -> error "TODO : Please report this as a bug. Need to handle the use of Common Params!"
              singleParamType:[] -> pure (PathParamType singleParamType)
              -- TODO : If the below case is encountered we need to handle it. (add separate Routes!)
              otherVal -> error $ "Expected only a single Param Type to be present in all Methods of this path."
                ++ "Instead got : " ++ show otherVal ++ " Path : " ++ swFilePath
          PathPiece staticPathCompStr -> pure (PathComp staticPathCompStr)
          )
         
    let currentRoutePath = finalPathWithParamTypes
        methodList = [SG.GET, SG.PUT, SG.POST, SG.PATCH, SG.DELETE, SG.OPTIONS, SG.HEAD]
    currentMethodData <- Control.Monad.foldM (processPathItem mainRouteName swPathDetails swaggerData) (Map.empty) methodList
    -- TODO : Remove the routeID from ContractDetails, it is not used. Set 0 for now.
    let currentContractDetails = ContractDetails 0 mainRouteName currentRoutePath currentMethodData
    pure (currentContractDetails:cDetailsList)
   
  constructSwPathComps :: String -> SwPathComponent
  constructSwPathComps routeComponent = 
    if isParam routeComponent 
    then PathParamName $ removeCurlyBraces routeComponent
    else PathPiece routeComponent

  removeLeadingSlash :: String -> String 
  removeLeadingSlash inputRoute = fromMaybe inputRoute (DL.stripPrefix "/" inputRoute)
  
  prettifyRouteName :: [SwPathComponent] -> String
  prettifyRouteName swSinglePathComps = case swSinglePathComps of
    [] -> error "Expected atleast one element in the route! Got an empty list!"
    (PathPiece ""):[] -> "BaseRoute"
    pathComps -> DL.concat $ flip fmap pathComps (\swPathComp -> 
          case swPathComp of
            PathParamName (firstChar:remainingChar) -> (Char.toUpper firstChar):remainingChar
            PathPiece (firstChar:remainingChar) -> (Char.toUpper firstChar):remainingChar
            PathParamName [] -> 
              error $ "Path Param Name is an empty String. This should be impossible!"
                ++ "\nPlease check the Swagger Doc"
                ++ "\nFull Path : " ++ (show swSinglePathComps)
            PathPiece [] -> 
              error $ "PathPiece is an empty String. This should be impossible! "
                ++ "Please check the Swagger Doc! \n Full Path : " ++ (show swSinglePathComps) )
    
  isParam :: String -> Bool
  isParam pathComponent = (DL.isPrefixOf "{" pathComponent) && (DL.isSuffixOf "}" pathComponent)

  removeCurlyBraces :: String -> String 
  removeCurlyBraces = DL.filter (\x -> not (x == '{' || x == '}') )

  getListOfPathOperations :: PathItem -> [(SG.Method, Maybe Operation)]
  getListOfPathOperations pathItem = [(SG.GET, _pathItemGet pathItem), (SG.PUT, _pathItemPut pathItem), (SG.POST,_pathItemPost pathItem), (SG.DELETE, _pathItemDelete pathItem), (SG.OPTIONS, _pathItemOptions pathItem), (SG.HEAD, _pathItemHead pathItem), (SG.PATCH, _pathItemPatch pathItem)]
  
  getPathParamTypeFromOperation :: RouteName -> String -> InsOrdHashMap Text Param -> [Referenced Param] -> (SG.Method, Maybe Operation) -> StateT (HMS.HashMap LevelInfo [TypeInfo]) IO (Maybe String)
  getPathParamTypeFromOperation routeNameStr paramPathName refParamsHM pathLvlParams (stdMethod, mOperation) = case mOperation of
    Just operation -> do 
      let opParamList = _operationParameters operation
      let finalParams = filterOutOverriddenParams refParamsHM opParamList pathLvlParams
      mParamType <- foldM (\existingParamType refOrInlineParam -> 
        case refOrInlineParam of
          Ref (Reference pmText) -> 
            case HMSIns.lookup pmText refParamsHM of
              Just refParam -> 
                if (_paramName refParam) == T.pack paramPathName
                then 
                  case existingParamType of
                    Nothing -> do
                      pathParamType <- getParamTypeForPathParam (routeNameStr, stdMethod) refParam
                      pure $ Just pathParamType
                    Just _ -> error $ "Atleast two or more Params in the Params Ref HM match this param." 
                      ++ "This should be impossible. Please check the Swagger Spec!"
                      ++ "\nDebug Info (Path Param Name) : " ++ (show paramPathName)
                else pure existingParamType
              Nothing -> pure existingParamType
          Inline param ->
            case (_paramName param == T.pack paramPathName ) of
              True -> do
                let pSchema = _paramSchema param 
                case pSchema of
                  ParamOther pOSchema -> 
                    case _paramOtherSchemaIn pOSchema of
                      ParamPath -> do
                        pathParamType <- getParamTypeForPathParam (routeNameStr, stdMethod) param
                        pure $ Just pathParamType
                      _ -> pure existingParamType 
                  ParamBody _ -> pure existingParamType 
              False -> pure existingParamType
        ) Nothing finalParams 
      pure mParamType 
    Nothing -> pure $ Nothing

  getParamTypeForPathParam :: RouteAndMethod -> Param -> StateT (HMS.HashMap LevelInfo [TypeInfo]) IO String
  getParamTypeForPathParam (routeNameStr, stdMethod) param = 
    case (_paramSchema param) of
      ParamOther paramOtherSchema -> 
        case _paramOtherSchemaIn paramOtherSchema of
          ParamPath -> 
            -- TODO : Verify that it's okay to put DefinitionI for Path Param. 
            -- Is it okay if this is generated in the local Types.hs file? 

            -- TODO : Since Path Params can be only primitive types, we should have another function 
            --        to calculate the type. 
            getTypeFromSwaggerType (Local ParamTy (routeNameStr, stdMethod)) DefinitionI (T.unpack $ _paramName param) Nothing (_paramOtherSchemaParamSchema paramOtherSchema) 
          _ -> error $ "Expected Path Param but got another Param Type. \nParam : " ++ (show param)
      ParamBody _ -> error $ "Param matched by name in the Ref Params HM. "
                  ++ "This means it should be a Path Param but it is a Body Param. "
                  ++ "This is theoretically impossible. Please check the Swagger Doc!"
                  ++ "\nDebug Info : (Path) Param ->  \n" ++ (show param)
  

  processPathItem :: String -> PathItem -> Swagger -> (Map.Map SG.Method ApiTypeDetails) ->  SG.Method -> StateT (HMS.HashMap LevelInfo [TypeInfo]) IO (Map.Map SG.Method ApiTypeDetails)
  processPathItem mainRouteName pathItem swaggerData methodDataAcc currentMethod = do
    let commonPathParams = _pathItemParameters pathItem
    case currentMethod of
      SG.GET -> (processOperation commonPathParams mainRouteName methodDataAcc swaggerData) SG.GET $ _pathItemGet pathItem
      SG.PUT -> (processOperation commonPathParams mainRouteName methodDataAcc swaggerData) SG.PUT $ _pathItemPut pathItem
      SG.POST -> (processOperation commonPathParams mainRouteName methodDataAcc swaggerData) SG.POST $ _pathItemPost pathItem
      SG.DELETE -> (processOperation commonPathParams mainRouteName methodDataAcc swaggerData) SG.DELETE $ _pathItemDelete pathItem
      SG.OPTIONS -> (processOperation commonPathParams mainRouteName methodDataAcc swaggerData) SG.OPTIONS $ _pathItemOptions pathItem
      SG.HEAD -> (processOperation commonPathParams mainRouteName methodDataAcc swaggerData) SG.HEAD $ _pathItemHead pathItem
      SG.PATCH -> (processOperation commonPathParams mainRouteName methodDataAcc swaggerData) SG.PATCH $ _pathItemPatch pathItem
      -- TODO: If the following case is hit, we need to add it to error/log reporting.
      _ -> pure $ Map.empty


processOperation :: [Referenced Param] -> String -> Map.Map SG.Method ApiTypeDetails -> Swagger -> SG.Method -> Maybe Operation -> StateT (HMS.HashMap LevelInfo [TypeInfo]) IO (Map.Map SG.Method ApiTypeDetails)
processOperation commonPathLvlParams currentRouteName methodAcc swaggerData stdMethod mOperationData = 
  case mOperationData of
    Just operationData -> do
      let refParamsHM = _swaggerParameters swaggerData
      let apiResponses = _responsesResponses $ _operationResponses operationData
      (mApiOutType, apiErrType) <- getApiType apiResponses swaggerData
      -- TODO: Case match on ApiOut and if `Nothing` then check for default responses in `_responsesDefault $ _operationResponses operationData`
      let apiOutType = fromMaybe "()" mApiOutType
      let addPlainText = 
            case apiOutType of
              "()" -> True
              "P.Text" -> True
              _ -> False
          
      let pathLvlAndLocalParam = filterOutOverriddenParams refParamsHM (_operationParameters operationData) commonPathLvlParams
      -- Group the Referenced Params by ParamLocation and then go through each group separately.
      let (formParamList, queryParamList, fileParamList, headerInList, bodyParamList) = DL.foldl' (groupParamTypes refParamsHM) ([], [], [], [], []) pathLvlAndLocalParam
      mFormParamType <- getParamTypes formParamList FormParam
      mQueryParamType <- getParamTypes queryParamList QueryParam
      mFileParamType <- getParamTypes fileParamList FileParam
      mHeaderInType <- getParamTypes headerInList HeaderParam
      mReqBodyType <- getParamTypes bodyParamList BodyParam
      let (mContentTypes, xmlPresent) = getContentTypes (_operationProduces operationData) addPlainText
      let finalReqBodyType = flip fmap mReqBodyType (\reqBodyType -> 
            case (DL.isPrefixOf "[" reqBodyType) of
              True -> "'" ++ reqBodyType
              False -> "'[" ++ reqBodyType ++ "]" )
      let apiTypeDetails = 
            ApiTypeDetails 
              {
                apiOut = apiOutType 
              , apiErr = apiErrType 
              , formParam = mFormParamType 
              , queryParam = mQueryParamType 
              , fileParam = mFileParamType 
              , headerIn = mHeaderInType 
              , requestBody = finalReqBodyType 
              , contentTypes = mContentTypes 
              , hasXML = xmlPresent 
              }
      pure $ Map.insert stdMethod apiTypeDetails methodAcc
    Nothing -> pure methodAcc

 where
  groupParamTypes :: InsOrdHashMap Text Param -> ([Param], [Param], [Param], [Param], [Param]) -> Referenced Param -> ([Param], [Param], [Param], [Param], [Param])
  groupParamTypes refParamsHM allParamLists refParam = 
    case refParam of
      Ref (Reference paramRefName) ->
        case HMSIns.lookup paramRefName refParamsHM of
          Just paramVal -> putParamInMatchingPList allParamLists paramVal  
          Nothing -> error $ "Could not find referenced params value in the Ref Params HM! "
            ++ "Please check the Swagger Doc! "
            ++ "\nParam Name : " ++ (show paramRefName)
      Inline param -> putParamInMatchingPList allParamLists param

  putParamInMatchingPList :: ([Param], [Param], [Param], [Param], [Param]) -> Param -> ([Param], [Param], [Param], [Param], [Param])
  putParamInMatchingPList (formParamList, queryParamList, fileParamList, headerInList, bodyParamList) param =
    case _paramSchema param of
      ParamBody _ -> (formParamList, queryParamList, fileParamList, headerInList, param:bodyParamList)
      ParamOther pOtherSchema -> 
        case _paramOtherSchemaIn pOtherSchema of 
          ParamQuery -> (formParamList, param:queryParamList, fileParamList, headerInList, bodyParamList) 
          ParamHeader -> (formParamList, queryParamList, fileParamList, param:headerInList, bodyParamList)
          ParamPath -> (formParamList, queryParamList, fileParamList, headerInList, bodyParamList) 
          ParamFormData ->
            case (_paramSchema param) of
              ParamOther pSchema -> 
                case (_paramSchemaType $ _paramOtherSchemaParamSchema pSchema) of
                  Just SwaggerFile -> (formParamList, queryParamList, param:fileParamList, headerInList, bodyParamList) 
                  _ -> (param:formParamList, queryParamList, fileParamList, headerInList, bodyParamList) 
              otherParamSchema -> error $ "Expected ParamOther but encountered : " ++ (show otherParamSchema)

  getContentTypes :: Maybe MimeList -> Bool -> (Maybe String, Bool)
  getContentTypes mContentList addPlainText = do
    let plainTextList::[String] = if addPlainText then ["W.PlainText"] else []
    case mContentList of 
      Just contentList -> 
        case getMimeList contentList of
          [] -> (Nothing, False)
          mimeList ->
            let mimeTypes = '\'':DL.filter (/= '"') (show $ plainTextList ++ flip fmap mimeList 
                      (\mimeType -> case mimeType of
                          "application/xml" -> "W.XML"
                          "application/json" -> "W.JSON"
                          otherMime -> error $ "Encountered unknown MIME type. Please report this as a bug!"
                            ++ "\nMIME Type encountered is : " ++ (show otherMime) ) )
            in (Just mimeTypes, DL.isInfixOf "XML" mimeTypes)
      Nothing -> (Nothing, False)
  

  getApiType :: InsOrdHashMap HttpStatusCode (Referenced Response) -> Swagger -> StateT (HMS.HashMap LevelInfo [TypeInfo]) IO (Maybe String, Maybe String)
  getApiType responsesHM swaggerData = foldlWithKey' (\stateConfigWrappedTypes currentCode currentResponse -> do
        let newTypeName = currentRouteName ++ (show stdMethod)
        (apiOutType, apiErrType) <- stateConfigWrappedTypes
        let lvlInfo = Local ResponseTy (currentRouteName, stdMethod)
        case (currentCode >= 200 && currentCode < 300) of
          True -> do
            finalOutType <- do
              let newTypeNameConstructor = "ApiOut"
              (tyLevelInfo, currentResponseType) <- parseResponseContentGetType (lvlInfo, ApiOutI) currentResponse swaggerData newTypeNameConstructor
              fOutType <- addTypeToState (lvlInfo, ApiOutI) (tyLevelInfo, currentResponseType) newTypeNameConstructor
              pure $ Just fOutType
            pure (finalOutType, apiErrType)
          False -> do
            finalErrType <- do
              let newTypeNameConstructor = "ApiErr"
              (tyLevelInfo, currentResponseType) <- parseResponseContentGetType (lvlInfo, ApiErrI) currentResponse swaggerData newTypeNameConstructor
              fErrType <- addTypeToState (lvlInfo, ApiErrI) (tyLevelInfo, currentResponseType) newTypeNameConstructor
              pure $ Just fErrType
            pure (apiOutType, finalErrType)
    ) (pure (Nothing, Nothing)) responsesHM  
  parseResponseContentGetType :: (LevelInfo, TInfo) -> Referenced Response -> Swagger -> String -> StateT (HMS.HashMap LevelInfo [TypeInfo]) IO (LevelInfo, String)
  parseResponseContentGetType (levelInfo, tInfo) referencedResp swaggerData newTypeConsName = do
    let swResponses :: InsOrdHashMap Text Response = _swaggerResponses swaggerData
    -- let swDataDefns :: InsOrdHashMap Text Schema = _swaggerDefinitions swaggerData

    case referencedResp of
      Ref refText -> do
        let refRespName = getReference refText
        let globalRespLevel = Global ResponseTy
        pure (globalRespLevel, T.unpack refRespName)
      Inline responseSchema -> 
        case (_responseSchema responseSchema) of
          Just (Ref refText) -> pure (Global DefinitionTy, T.unpack $ getReference refText)
          Just (Inline respSchema) -> do
            typeNameStr <- ((getTypeFromSwaggerType levelInfo tInfo newTypeConsName (Just respSchema) ) . _schemaParamSchema) respSchema
            pure (levelInfo, typeNameStr)
          -- NOTE : The following case means no content is returned with the Response!
          Nothing -> pure (levelInfo, "()")
  getParamTypes :: [Param] -> ParamType -> StateT (HMS.HashMap LevelInfo [TypeInfo]) IO (Maybe String)
  getParamTypes paramList paramType = 
    case paramList of
      [] -> pure $ Nothing
      _ -> -- TODO : Refactor handling of adding Maybes and adding to State into a single function and call from all places.
        case paramType of
          FormParam -> do
            let paramNames = fmap (\param -> T.unpack $ _paramName param) paramList
            hTypesWithIsMandatory <- forM paramList (\param -> do 
              hType <- getParamTypeParam param (T.unpack ( _paramName param) ) Nothing 
              pure (isMandatory param, hType) )
            let finalHaskellTypes = fmap (\(isMandatoryType, hType) -> (addMaybeToType isMandatoryType hType) ) hTypesWithIsMandatory
            let recordTypesInfo = DL.zip paramNames finalHaskellTypes
            let newDataTypeName = setValidConstructorId "HFormParam"
            let formParamDataInfo = ProductType (NewData newDataTypeName recordTypesInfo) newDataTypeName
            let levelInfo = Local ParamTy (currentRouteName, stdMethod)
            let fParamHM = HMS.singleton levelInfo [FormParamTy formParamDataInfo Nothing]
            modifyState fParamHM
            pure $ Just newDataTypeName
          QueryParam -> do
            let paramNames = fmap (\param -> T.unpack $ _paramName param) paramList
            hTypesWithIsMandatory <- forM paramList (\param -> do 
              hType <- getParamTypeParam param (T.unpack ( _paramName param) ) Nothing
              pure (isMandatory param, hType) )
            let finalHaskellTypes = fmap (\(isMandatoryType, hType) -> (addMaybeToType isMandatoryType hType) ) hTypesWithIsMandatory
            let recordTypesInfo = DL.zip paramNames finalHaskellTypes
            let newDataTypeName = setValidConstructorId "HQueryParam"
            let queryParamDataInfo = ProductType (NewData newDataTypeName recordTypesInfo) newDataTypeName
            let levelInfo = Local ParamTy (currentRouteName, stdMethod)
            let qParamHM = HMS.singleton levelInfo [QueryParamTy queryParamDataInfo Nothing]
            modifyState qParamHM
            pure $ Just newDataTypeName
          HeaderParam -> do
            let paramNames = fmap (\param -> T.unpack $ _paramName param) paramList
            typeListWithIsMandatory <- forM paramList (\param -> do
                    hType <- getParamTypeParam param (T.unpack ( _paramName param) ) Nothing
                    pure (isMandatory param, hType) )
            let finalHaskellTypes = fmap (\(isMandatoryType, hType) -> (addMaybeToType isMandatoryType hType) ) typeListWithIsMandatory
            let recordTypesInfo = DL.zip paramNames finalHaskellTypes
            let newDataTypeName = setValidConstructorId "HHeaderParam"
            let headerParamDataInfo = ProductType (NewData newDataTypeName recordTypesInfo) newDataTypeName
            let levelInfo = Local ParamTy (currentRouteName, stdMethod)
            let headerParamHM = HMS.singleton levelInfo [HeaderInTy headerParamDataInfo Nothing]
            modifyState headerParamHM
            pure $ Just newDataTypeName
          FileParam -> do
            typeList <- forM paramList (\param -> getParamTypeParam param (T.unpack ( _paramName param) ) Nothing )
            case typeList of
              [] -> pure Nothing
              x:[] -> pure $ Just x
              _ -> error $ "Encountered list of FileParam. This is not yet handled! "
                ++ "\nDebug Info: " ++ (show paramList)
          BodyParam -> do
            listOfTypes <- forM paramList (\param -> getParamTypeParam param (T.unpack (_paramName param)) Nothing )
            case listOfTypes of
              [] -> error $ "Tried to Get Body Param type but got an empty list/string! Debug Info: " ++ show paramList
              x:[] -> pure $ Just x
              _ -> error $ "Encountered a list of Body Params. WebApi/Swagger does not support this currently! Debug Info: " ++ show paramList

  getParamTypeParam :: Param -> String -> Maybe Schema  -> StateT (HMS.HashMap LevelInfo [TypeInfo]) IO String
  getParamTypeParam inputParam paramName mOuterSchema = do
  -- TODO : This may need to be calculated or passed here (or passed back from getTypeFromSwaggerType) if/when we consider route-level common params.
    let levelInfo = Local ParamTy (currentRouteName, stdMethod)
    case _paramSchema inputParam of
      ParamBody refSchema -> 
        case refSchema of 
          Ref refType -> pure $ T.unpack (getReference refType)
          Inline rSchema -> getTypeFromSwaggerType levelInfo ReqBodyI paramName (Just rSchema) (_schemaParamSchema rSchema) 
      ParamOther pSchema ->
        let tInfo = 
              case _paramOtherSchemaIn pSchema of
                ParamQuery -> QueryParamI
                ParamHeader -> HeaderInI
                ParamFormData -> FormParamI
                -- TODO : Verify if this should ever be ParamPath. 
                -- If not, we should log this and error it when adding logging/error mechanism.
                ParamPath -> DefinitionI
        in getTypeFromSwaggerType levelInfo tInfo paramName mOuterSchema $ _paramOtherSchemaParamSchema pSchema 

  isMandatory :: Param -> Bool
  isMandatory param = 
    case _paramRequired param of
      Just True -> True
      _ -> False
  addMaybeToType :: Bool -> String -> String
  addMaybeToType isNotNull haskellType = 
    case isNotNull of
      True -> haskellType
      False -> "P.Maybe " ++ haskellType 


filterOutOverriddenParams :: InsOrdHashMap Text Param -> [Referenced Param] -> [Referenced Param] -> [Referenced Param]
filterOutOverriddenParams globalParams pathLvlParams localOpParams = do
  let pathLvlParamNames = fmap getParamName pathLvlParams
  let localOpParamNames = fmap getParamName localOpParams
  case DL.intersect pathLvlParamNames localOpParamNames of
    [] -> pathLvlParams ++ localOpParams
    ovrdnParams -> 
      let modPathLvlParams = DL.filter (\param -> not $ DL.elem (getParamName param) ovrdnParams) pathLvlParams
      in localOpParams ++ modPathLvlParams

 where
  getParamName :: Referenced Param -> String
  getParamName refParam = 
    case refParam of
      Inline paramObj -> T.unpack $ _paramName paramObj
      Ref refObj -> do
        let paramNameTxt = getReference refObj
        case HMSIns.lookup paramNameTxt globalParams of
          Just paramVal -> T.unpack $ _paramName paramVal
          Nothing -> error $ "Could not find referenced params value in the Ref Params HM! "
            ++ "Please check the Swagger Doc! "
            ++ "\nParam Name : " ++ (T.unpack paramNameTxt)


addTypeToState :: (LevelInfo, TInfo) -> (LevelInfo, String) -> String  -> (StateT (HMS.HashMap LevelInfo [TypeInfo]) IO String)
addTypeToState (levelInfo, tInfo) (tLevelInfo, currentType) newTypeName = do
        let typeWithQual = addQual levelInfo currentType tLevelInfo
        let sumTyConsName = 
              if currentType == "()"
              then "NoContent"
              else setValidConstructorId currentType
            sumTypeConstructors = [(sumTyConsName, typeWithQual)]
            sumTypeInfo = SumType (ComplexSumType newTypeName sumTypeConstructors) -- [currentType, eType] [currentType, eType] -- Note : OgNames not really applicable here so putting Haskell names
        modify' (\existingState -> HMS.insertWith (insertIntoExistingSumTy sumTyConsName) levelInfo [tInfoToTypeInfo tInfo sumTypeInfo] existingState )
        pure newTypeName

 where
  addQual :: LevelInfo -> String -> LevelInfo -> String
  addQual currentLvlInfo typeStr typeLvlInfo = 
    case currentLvlInfo == typeLvlInfo of
      True -> typeStr
      False ->
        let qualImportName = 
              case typeLvlInfo of
                Global DefinitionTy -> globalDefnsQualName 
                Global ResponseTy -> globalRespTypesQualName
                Global ParamTy -> globalParamsQualName
                _ -> error $ "Expected a Global LevelInfo if LevelInfos don't match. Got : " ++ (show typeLvlInfo)
        in qualImportName ++ "." ++ typeStr

  insertIntoExistingSumTy :: String -> [TypeInfo] -> [TypeInfo] -> [TypeInfo]
  insertIntoExistingSumTy currentTyCons newTyInfo existingTyInfos = 
    case newTyInfo of
      (ApiErrTy ty nc):[] -> (addIfNotChanged (ApiErrTy ty nc)) $ DL.foldl' (addToApiErrTyInfo currentTyCons) (False, []) existingTyInfos
      (ApiOutTy ty nc):[] -> (addIfNotChanged (ApiOutTy ty nc)) $ DL.foldl' (addToApiOutTyInfo currentTyCons) (False, []) existingTyInfos
      _ -> error $ "Encountered empty or multiple value TypeInfo list." 
            ++ "Expected only one value. Got : " ++ (show newTyInfo) 

  addToApiErrTyInfo :: String -> (Bool, [TypeInfo]) -> TypeInfo -> (Bool, [TypeInfo])
  addToApiErrTyInfo currentTyCons (isChanged, accVal) currentTyInfo = 
    case currentTyInfo of
      ApiErrTy (SumType (ComplexSumType tyName tyList )) nCtr -> 
        let modTy = ApiErrTy (SumType (ComplexSumType tyName ((currentTyCons,currentTyCons):tyList) )) nCtr
        in (True, modTy:accVal)
      _ -> (isChanged, currentTyInfo:accVal)
    
  addToApiOutTyInfo :: String -> (Bool, [TypeInfo]) -> TypeInfo -> (Bool, [TypeInfo])
  addToApiOutTyInfo currentTyCons (isChanged, accVal) currentTyInfo = 
    case currentTyInfo of
      ApiOutTy (SumType (ComplexSumType tyName tyList )) nCtr -> 
        let modTy = ApiOutTy (SumType (ComplexSumType tyName ((currentTyCons,currentTyCons):tyList) )) nCtr
        in (True, modTy:accVal)
      _ -> (isChanged, currentTyInfo:accVal)

  addIfNotChanged :: TypeInfo -> (Bool, [TypeInfo]) -> [TypeInfo] 
  addIfNotChanged newTyInfo (isChanged, tyInfoList) = 
    if isChanged 
    then tyInfoList
    else newTyInfo:tyInfoList
        
  addToStateSumType :: LevelInfo -> String -> String -> (HMS.HashMap LevelInfo [TypeInfo], Bool) -> LevelInfo -> [TypeInfo] -> (HMS.HashMap LevelInfo [TypeInfo], Bool)
  addToStateSumType currentTyLvlInfo newSumTypeName currentTypeStr (accVal, isChanged) lvlInfo tyInfoList = 
    let (modTypeInfoList, valueChanged) = DL.foldl' (addToSSTypeInfo newSumTypeName currentTypeStr) ([], isChanged) tyInfoList
    in (HMS.insertWith checkForDuplicateAndAdd lvlInfo modTypeInfoList accVal, valueChanged)

  addToSSTypeInfo :: String -> String -> ([TypeInfo], Bool) -> TypeInfo -> ([TypeInfo], Bool) 
  addToSSTypeInfo newSumTypeName currentTypeStr (accTyInfoList, isChanged) currentTyInfo = 
    case getTypeName currentTyInfo == newSumTypeName of
      True -> 
        case getInnerTyFromTypeInfo currentTyInfo of
          SumType (ComplexSumType dataName consAndTypes) -> 
            let consName = setValidConstructorId $ dataName ++ currentTypeStr
                modSumType = SumType (ComplexSumType dataName ((consName, currentTypeStr):consAndTypes) )
                modTyInfo = updateTypeInfoDataTy currentTyInfo modSumType
            in (modTyInfo:accTyInfoList, True)
          _ -> (currentTyInfo:accTyInfoList, isChanged)
      False -> (currentTyInfo:accTyInfoList, isChanged)


getTypeName :: TypeInfo -> String 
getTypeName tyInfo =
  let innerTy = getInnerTyFromTypeInfo tyInfo 
  in case innerTy of
    SumType (BasicEnum consName _ _) -> consName
    SumType (ComplexSumType consName _ ) -> consName
    ProductType (NewData consName _ ) _ -> consName
    HNewType consName _ _ -> consName


getInnerTyAndCtr :: TypeInfo -> (CreateDataType, NamingCounter)
getInnerTyAndCtr tyInfo =
  case tyInfo of
    ApiErrTy cdt nCtr -> (cdt, nCtr)
    ApiOutTy cdt nCtr -> (cdt, nCtr)
    FormParamTy cdt nCtr -> (cdt, nCtr)
    QueryParamTy cdt nCtr -> (cdt, nCtr)
    FileParamTy cdt nCtr -> (cdt, nCtr)
    HeaderInTy cdt nCtr -> (cdt, nCtr)
    ReqBodyTy cdt nCtr -> (cdt, nCtr)
    ContentTypesTy cdt nCtr -> (cdt, nCtr)
    HeaderOutTy cdt nCtr -> (cdt, nCtr)
    DefinitionType cdt nCtr -> (cdt, nCtr)

getInnerTyFromTypeInfo :: TypeInfo -> CreateDataType
getInnerTyFromTypeInfo tyInfo =
  case tyInfo of
    ApiErrTy cdt _ -> cdt 
    ApiOutTy cdt _ -> cdt 
    FormParamTy cdt _ -> cdt  
    QueryParamTy cdt _ -> cdt  
    FileParamTy cdt _ -> cdt  
    HeaderInTy cdt _ -> cdt 
    ReqBodyTy cdt _ -> cdt 
    ContentTypesTy cdt _ -> cdt 
    HeaderOutTy cdt _ -> cdt 
    DefinitionType cdt _ -> cdt 

getTypeNameWithCounter :: TypeInfo -> (String, Maybe Int)
getTypeNameWithCounter tyInfo = 
  let (innerTy, namingCtr) = getInnerTyFromTypeInfoWithCtr tyInfo 
      cName =
        case innerTy of
          SumType (BasicEnum consName _ _) -> consName
          SumType (ComplexSumType consName _ ) -> consName
          ProductType (NewData consName _ ) _ -> consName
          HNewType consName _ _ -> consName
  in (cName, namingCtr)

getInnerTyFromTypeInfoWithCtr :: TypeInfo -> (CreateDataType, NamingCounter)
getInnerTyFromTypeInfoWithCtr tyInfo =
  case tyInfo of
    ApiErrTy cdt nCtr -> (cdt, nCtr)
    ApiOutTy cdt nCtr -> (cdt, nCtr)
    FormParamTy cdt nCtr -> (cdt, nCtr)
    QueryParamTy cdt nCtr -> (cdt, nCtr)
    FileParamTy cdt nCtr -> (cdt, nCtr)
    HeaderInTy cdt nCtr -> (cdt, nCtr)
    ReqBodyTy cdt nCtr -> (cdt, nCtr)
    ContentTypesTy cdt nCtr -> (cdt, nCtr)
    HeaderOutTy cdt nCtr -> (cdt, nCtr)
    DefinitionType cdt nCtr -> (cdt, nCtr)


addCtrToTypeInfo :: NamingCounter -> TypeInfo -> TypeInfo
addCtrToTypeInfo mCtrVal tyInfo = 
  case tyInfo of
    ApiErrTy cdt _ -> ApiErrTy cdt mCtrVal
    ApiOutTy cdt _ -> ApiOutTy cdt mCtrVal
    FormParamTy cdt _ -> FormParamTy cdt mCtrVal
    QueryParamTy cdt _ -> QueryParamTy cdt mCtrVal
    FileParamTy cdt _ -> FileParamTy cdt mCtrVal
    HeaderInTy cdt _ -> HeaderInTy cdt mCtrVal
    ReqBodyTy cdt _ -> ReqBodyTy cdt mCtrVal
    ContentTypesTy cdt _ -> ContentTypesTy cdt mCtrVal
    HeaderOutTy cdt _ -> HeaderOutTy cdt mCtrVal
    DefinitionType cdt _ -> DefinitionType cdt mCtrVal

updateTypeInfoDataTy :: TypeInfo -> CreateDataType -> TypeInfo
updateTypeInfoDataTy tyInfo newCdt = 
  case tyInfo of
    ApiErrTy _ nameCtr -> ApiErrTy newCdt nameCtr
    ApiOutTy _ nameCtr -> ApiOutTy newCdt nameCtr
    FormParamTy _ nameCtr -> FormParamTy newCdt  nameCtr
    QueryParamTy _ nameCtr -> QueryParamTy newCdt  nameCtr
    FileParamTy _ nameCtr -> FileParamTy newCdt  nameCtr
    HeaderInTy _ nameCtr -> HeaderInTy newCdt nameCtr
    ReqBodyTy _ nameCtr -> ReqBodyTy newCdt nameCtr
    ContentTypesTy _ nameCtr -> ContentTypesTy newCdt nameCtr
    HeaderOutTy _ nameCtr -> HeaderOutTy newCdt nameCtr
    DefinitionType _ nameCtr -> DefinitionType newCdt nameCtr


-- NOTE: In the below function, we set the value to Nothing because in all places 
--       where the function is called, there is no chance for a naming counter to be set.
tInfoToTypeInfo :: TInfo -> CreateDataType -> TypeInfo
tInfoToTypeInfo tInfo cdt =
  case tInfo of
    ApiErrI -> ApiErrTy cdt Nothing
    ApiOutI -> ApiOutTy cdt Nothing
    FormParamI -> FormParamTy cdt Nothing
    QueryParamI -> QueryParamTy cdt Nothing
    FileParamI -> FileParamTy cdt Nothing
    HeaderInI -> HeaderInTy cdt Nothing
    ReqBodyI -> ReqBodyTy cdt Nothing
    ContentTypesI -> ContentTypesTy cdt Nothing
    HeaderOutI -> HeaderOutTy cdt Nothing
    DefinitionI -> DefinitionType cdt Nothing



getTypeFromSwaggerType :: LevelInfo -> TInfo -> String -> Maybe Schema ->  ParamSchema t -> StateT (HMS.HashMap LevelInfo [TypeInfo]) IO String 
getTypeFromSwaggerType levelInfo tInfo paramNameOrRecordName mOuterSchema paramSchema = 
    -- let mRouteName = 
    case (_paramSchemaType paramSchema) of 
      Just SwaggerString -> 
        case _paramSchemaFormat paramSchema of
          Just "date" -> pure "P.Day"
          Just "date-time" -> pure "P.UTCTime"
          Just "password" -> error $ "Encountered SwaggerString with Format as `password`. This needs to be handled! Debug Info : " ++ show paramSchema
          Just "byte" -> pure "P.ByteString"
          -- TODO : Map binary to ByteString
          Just "binary" -> error $ "Encountered SwaggerString with Format as `binary`. This needs to be handled! Debug Info: " ++ show paramSchema
          Nothing -> 
            case _paramSchemaEnum paramSchema of
              Nothing -> pure "P.Text"
              Just valueEnumList -> do
                let newSumTypeName = setValidConstructorId $ paramNameOrRecordName
                let (enumVals, ogVals) = 
                      DL.unzip $ 
                        fmap (\(Data.Aeson.String enumVal) -> 
                                  (setValidConstructorId (T.unpack enumVal), T.unpack enumVal) ) valueEnumList
                let haskellNewTypeInfo = SumType (BasicEnum newSumTypeName enumVals ogVals )
                let sumTyHM = HMS.singleton levelInfo [tInfoToTypeInfo tInfo haskellNewTypeInfo]
                modifyState sumTyHM
                pure newSumTypeName
                -- currentState <- get
                -- let onlySumTypes = DL.filter (\newTypeObj -> do
                --           case newTypeObj of 
                --             SumType (BasicEnum _ _ _) -> True
                --             _ -> False ) $ fmap getInnerTyFromTypeInfo $ DL.concat $ HMS.elems currentState
                -- let createSumType = DL.foldl' checkIfSumTypeExists (CreateSumType haskellNewTypeInfo) onlySumTypes
                -- case createSumType of
                --   CreateSumType (SumType (BasicEnum sName sNewVals sOgVals) ) -> do
                --     let sumTyInfo = (SumType (BasicEnum sName sNewVals sOgVals) )
                --     let sumTyHM = HMS.singleton levelInfo [tInfoToTypeInfo tInfo sumTyInfo]
                --     modify' (\existingState -> HMS.unionWith (++) existingState sumTyHM)
                --     pure sName
                  -- CreateSumType otherTy -> 
                  --   error $ "Expected only SumTypes here, but got : " ++ (show otherTy)
                  --     ++ "\nSince we have already filtered for only SumTypes, this should not be possible!"
                  -- ExistingType existingTyName -> pure existingTyName
          _ -> pure "P.Text" -- error $ "Encountered SwaggerString with unknown Format! Debug Info: " ++ show paramSchema
      Just SwaggerNumber -> 
        case _paramSchemaFormat paramSchema of
          Just "float" -> pure "P.Float"
          Just "double" -> pure "P.Double"
          _ -> pure "SwaggerNumber"
      Just SwaggerInteger -> 
        case _paramSchemaFormat paramSchema of
          Just "int32" -> pure "P.Int32"
          Just "int64" -> pure "P.Int64"
          _ -> pure "P.Int"
      Just SwaggerBoolean -> pure "P.Bool"
      -- As per the pattern in `PetStore`, for SwaggerArray, we check the Param Schema Items field and look for a reference Name there.
      Just SwaggerArray -> case _paramSchemaItems paramSchema of
                        Just (SwaggerItemsObject obj) -> 
                          case obj of
                            Ref reference -> pure $ "[" ++ (setValidConstructorId $ T.unpack $ getReference reference) ++ "]"
                            Inline recursiveSchema -> do
                              let innerTypeName = (setValidConstructorId $ paramNameOrRecordName ++ "Contents")
                              hType <- ( ( (getTypeFromSwaggerType levelInfo tInfo innerTypeName (Just recursiveSchema) ) . _schemaParamSchema) recursiveSchema)
                              pure $ "[" ++ hType ++ "]"
                        -- NOTE : SwaggerItemsArray is used in a case where there are tuple Schemas.
                        -- So we need to represent the following list of Ref Schemas as a tuple or newtype (over tuple).
                        -- TODO : This needs to be changed. Ref to GitHub ticket #34
                        Just (SwaggerItemsArray innerArray) -> pure $ "SwaggerItemsArrayType"
                          -- checkIfArray $ flip Control.Monad.mapM innerArray (\singleElem -> do
                          -- case singleElem of
                          --   Ref ref -> pure $ setValidConstructorId $ T.unpack $ getReference ref
                          --   Inline innerSchema -> ((getTypeFromSwaggerType mRouteName mParamNameOrRecordName (Just innerSchema) ) . _schemaParamSchema) innerSchema) 
                        Just (SwaggerItemsPrimitive mCollectionFormat innerParamSchema) -> do
                          typeName <- do
                                let paramName = paramNameOrRecordName
                                let titleCaseParamName = setValidConstructorId $ T.unpack $ T.toTitle $ T.pack paramName
                                case _paramSchemaEnum innerParamSchema of
                                  Just enumVals -> do
                                    let (enumValList, ogVals)::([String], [String]) = 
                                          DL.unzip $ fmap (\(Data.Aeson.String val) -> (T.unpack $ T.toTitle val, T.unpack val) ) enumVals
                                    let haskellNewTypeInfo = SumType (BasicEnum titleCaseParamName enumValList ogVals)
                                    let hNewTyHM = HMS.singleton levelInfo [tInfoToTypeInfo tInfo haskellNewTypeInfo]
                                    modifyState hNewTyHM
                                    pure titleCaseParamName
                                    -- TODO: Is it allowed for a CollectionFmt type to have this inner schema (complex type)? 
                                  Nothing ->  getTypeFromSwaggerType levelInfo tInfo ("Collection" ++ titleCaseParamName) Nothing innerParamSchema
                          case mCollectionFormat of
                            (Just CollectionMulti) -> pure $ "P.MultiSet " ++ typeName 
                            (Just CollectionTSV) -> pure $ "DelimitedCollection \"\t\" " ++ typeName
                            (Just CollectionSSV) -> pure $ "DelimitedCollection \" \"" ++ typeName
                            (Just CollectionPipes) -> pure $ "DelimitedCollection \"|\"" ++ typeName
                            -- Since CSV is the default, the below case takes care of (Just CSV) as well as Nothing
                            _ -> pure $ "DelimitedCollection \",\" " ++ typeName
                        Nothing -> error "Expected a SwaggerItems type due to SwaggerArray ParamSchema Type. But it did not find any! Please check the swagger spec!"
      Just SwaggerObject -> do
        let recordTypeName = 
              setValidConstructorId paramNameOrRecordName
        case mOuterSchema of
          Just outerSchema -> 
            case (HMSIns.toList $ _schemaProperties outerSchema) of
              [] -> 
                case (_schemaAdditionalProperties outerSchema) of
                  Just additionalProps -> 
                    case additionalProps of
                      AdditionalPropertiesSchema (Ref ref) -> pure $ "(HM.HashMap P.Text " ++ (setValidConstructorId $ T.unpack $ getReference ref) ++ ")"
                      AdditionalPropertiesSchema (Inline internalSchema) -> ((getTypeFromSwaggerType levelInfo tInfo recordTypeName (Just internalSchema)) . _schemaParamSchema) internalSchema
                      AdditionalPropertiesAllowed _ -> error "TODO: unhandled case of additional props"
                      
                  Nothing -> 
                    case (_paramSchemaType . _schemaParamSchema) outerSchema of
                      Just SwaggerObject -> pure $ "(HM.HashMap P.Text P.Text)"
                      _ -> error $ "Type SwaggerObject but swaggerProperties and additionalProperties are both absent! "
                        ++ "Also, the paramSchema type in the ParamSchema is not an Object! Please check the JSON! "
                        ++ "Debug Info (Schema): " ++ show outerSchema
              propertyList -> do -- TODO: This needs to be changed when we encounter _schemaProperties in some swagger doc/schema.
                innerRecordsInfo <- forM propertyList (\(recordName, iRefSchema) -> do
                      let recordNameStr = T.unpack recordName
                      innerRecordType <- case iRefSchema of
                          Ref refName -> pure $ setValidConstructorId $ T.unpack $ getReference refName
                          Inline irSchema -> ((getTypeFromSwaggerType levelInfo tInfo recordNameStr (Just irSchema)) . _schemaParamSchema) irSchema 
                      let isRequired = isRequiredType outerSchema recordNameStr
                      let typeWithMaybe = if isRequired then innerRecordType else setMaybeType innerRecordType
                      pure (recordNameStr, typeWithMaybe) )
                let finalProductTypeInfo = ProductType (NewData recordTypeName innerRecordsInfo) paramNameOrRecordName
                let hNewTyHM = HMS.singleton levelInfo [tInfoToTypeInfo tInfo finalProductTypeInfo]
                modifyState hNewTyHM
                pure recordTypeName
          Nothing -> error $ "Expected outer schema to be present when trying to construct type of SwaggerObject. Debug Info (ParamSchema):  " ++ show paramSchema
      Just SwaggerFile -> pure "W.FileInfo" -- TODO 
      Just SwaggerNull -> pure "()"
      -- NOTE: what are types which have no type info?
      Nothing          -> pure "()"
      -- x -> ("Got Unexpected Primitive Value : " ++ show x)
 where 
  -- setNewTypeConsName :: LevelInfo -> CreateDataType -> CreateDataType
  -- setNewTypeConsName lvlInfo (ProductType (NewData oldConsName inRecordInfo) ogName) = 
  --       routeNameStr = fromJustNote noRouteErrMsg maybeRouteName
  --       newConsName = setValidConstructorId $ routeNameStr ++ oldConsName
  --   in (ProductType (NewData newConsName inRecordInfo) ogName )
  -- setNewTypeConsName _ otherType =  
  --   error $ "Expected RouteName along with Product Type. "
  --     ++ "\nWe are trying to avoid a name clash so we are trying to set a new name "
  --     ++ "for the type : " ++ (show otherType) 
  --     ++ "\nWe expected it to be a Product Type!"
    
  isRequiredType :: Schema -> String -> Bool
  isRequiredType tSchema recordFieldName = DL.elem (T.pack recordFieldName) (_schemaRequired tSchema)

  setMaybeType :: String -> String
  setMaybeType = ("P.Maybe " ++ )

  isNamePresent :: String -> HMS.HashMap LevelInfo [TypeInfo] -> Bool
  isNamePresent newTypeName stateVals = 
    let tInfos = DL.concat $ HMS.elems stateVals
        stateTypeNames = fmap getTypeName tInfos
    in DL.elem newTypeName stateTypeNames

  checkIfArray :: StateT (HMS.HashMap LevelInfo [TypeInfo]) IO [String] -> StateT (HMS.HashMap LevelInfo [TypeInfo]) IO String
  checkIfArray scStringList = do
    stringList <- scStringList 
    case DL.nub stringList of
      sameElem:[] -> pure $ "[" ++ sameElem ++ "]"
      x -> error $ "Got different types in the same list. Not sure how to proceed! Please check the swagger doc! " ++ show x
  -- checkIfSumTypeExists :: SumTypeCreation -> CreateDataType -> SumTypeCreation
  -- checkIfSumTypeExists sumTypeCreation (SumType (BasicEnum typeName tVals _) ) = 
  --   case sumTypeCreation of
  --     ExistingType eTyName -> ExistingType eTyName
  --     CreateSumType (SumType (BasicEnum newTypeName newTypeVals ogVals ) ) -> do
  --       case (newTypeVals == tVals) of 
  --         True -> ExistingType typeName
  --         False -> 
  --           case (newTypeVals `DL.intersect` tVals) of
  --             [] -> CreateSumType (SumType (BasicEnum newTypeName newTypeVals ogVals ) )
  --             _ -> 
  --               let modConstructorNames = fmap (\oldCons -> setValidConstructorId $ newTypeName ++ oldCons) newTypeVals
  --               in CreateSumType (SumType (BasicEnum newTypeName modConstructorNames ogVals ) )

  --     CreateSumType xType -> 
  --       error $ "Expected only SumTypes here but got : " ++ (show xType)
  --         ++ "\nSince we already filtered for only SumTypes, this should not be possible!"

  -- checkIfSumTypeExists newType existingType =
  --   error $ "PANIC : We already filtered for only Sum Types but encountered non-sum type constructor!"
  --     ++ "\nDebugInfo : New Type to be created is : " ++ (show newType)
  --     ++ "\nExisting type is : " ++ (show existingType)


modifyState :: HMS.HashMap LevelInfo [TypeInfo] -> StateConfig ()
modifyState hNewTyHM = modify' (\existingState -> HMS.unionWith checkForDuplicateAndAdd existingState hNewTyHM)

  
checkForDuplicateAndAdd :: [TypeInfo] -> [TypeInfo] -> [TypeInfo]
checkForDuplicateAndAdd newList oldList = 
  let duplicatesRemovedTyInfoList = DL.union newList oldList
  in DL.foldl' checkForSameNamedType [] duplicatesRemovedTyInfoList

checkForSameNamedType :: [TypeInfo] -> TypeInfo -> [TypeInfo]
checkForSameNamedType accList currentTyInfo = do
  let (currentConsName, _) = getTypeNameWithCounter currentTyInfo
  -- NOTE: We discard the naming counter of current TypeInfo because we expect it to always be `Nothing`
  let mHighestCtr = DL.foldl' (getCtrIfIdenticalCons currentConsName) Nothing accList
  case mHighestCtr of
    Just ctr -> (addCtrToTypeInfo (Just (ctr + 1) ) currentTyInfo):accList
    Nothing -> currentTyInfo:accList
  
 where
  getCtrIfIdenticalCons :: String -> NamingCounter -> TypeInfo -> NamingCounter
  getCtrIfIdenticalCons currentConsName accNCtr tyInfo = do
    let (cName, mCtr) = getTypeNameWithCounter tyInfo
    case cName == currentConsName of
      True -> 
        case mCtr of
          Just ctr -> 
            case accNCtr of
              Just accCtrVal -> Just $ max accCtrVal ctr
              Nothing -> Just ctr
          Nothing -> 
            case accNCtr of
              Just accCtrVal -> Just accCtrVal
              Nothing -> Just 0
      False -> accNCtr


--  where

  -- (trace (traceStateValIfAuthor newList existingList) existingList)

  -- traceStateValIfAuthor :: [TypeInfo] -> [TypeInfo] -> String
  -- traceStateValIfAuthor newList exList = show newList
    -- case newList of
    --   someTy:[] -> 
    --     case getTypeName someTy of
    --       "Author" -> "NewList : " ++ (show newList) ++ "\nExistingList : " ++ (show exList) 
    --       "Tree" -> "NewList : " ++ (show newList) ++ "\nExistingList : " ++ (show exList) 
    --       _ -> ""
    --   _ -> "more than 1 elem or not Def Ty"


parseHaskellSrcContract :: String -> IO ()
parseHaskellSrcContract pathToFile = do
  parseResult <- parseFile pathToFile 
  case parseResult of
    ParseOk hModule -> 
      case hModule of
        Module _ (Just _) _ _ declarations -> putStrLn $ show declarations
        _ -> error "Module is not in the correct format?!"
    ParseFailed srcLoc errMsg -> putStrLn $ (show srcLoc) ++ " : " ++ errMsg


instanceTopVec :: Vector 4 String
instanceTopVec = fromJustNote "Expected a list with 4 elements for WebApi instance!" $ SV.fromList ["ApiContract", "EDITranslatorApi", "POST", "EdiToJsonR" ]

instanceTypeVec :: [Vector 4 String]
instanceTypeVec = [
                    ( fromMaybeSV $ SV.fromList ["ApiOut", "POST", "EdiToJsonR", "Value" ])
                  , ( fromMaybeSV $ SV.fromList ["ApiErr", "POST", "EdiToJsonR", "Text" ])
                  , ( fromMaybeSV $ SV.fromList ["FormParam", "POST", "EdiToJsonR", "EdiStr" ])
                  , ( fromMaybeSV $ SV.fromList ["QueryParam", "POST", "EdiToJsonR", "Maybe CharacterSet"]) 
                  ]
 where 
  fromMaybeSV :: Maybe a -> a
  fromMaybeSV = fromJustNote "Expected a list with 4 elements for WebApi instance! "

fromParamVec :: Vector 3 String
fromParamVec = fromJustNote "Expected a list with 3 elements for WebApi instance!" $ SV.fromList ["FromParam", "FormParam", "EdiStr"]

generateContractBody :: String -> [ContractDetails] -> [Decl SrcSpanInfo]
generateContractBody contractName contractDetails = 
  [emptyDataDeclaration contractName] ++ flip fmap contractDetails (\cDetail -> routeDeclaration (routeName cDetail) (routePath cDetail) ) ++ 
    [webApiInstance contractName (fmap (\ctDetail -> (routeName ctDetail , fmap qualMethod (Map.keys $ methodData ctDetail))) contractDetails ) ] ++
      (fmap (\(topVec, innerVecList) -> apiInstanceDeclaration topVec innerVecList ) $ DL.concat $ fmap (constructVectorForRoute contractName) contractDetails)
 where
  qualMethod :: SG.Method -> String
  qualMethod = ("W." ++) . show
    
  constructVectorForRoute :: String -> ContractDetails -> [(Vector 4 String, [Vector 4 String])] 
  constructVectorForRoute ctrtName ctrDetails = 
    let currentRouteName = routeName ctrDetails
    in Map.foldlWithKey' (routeDetailToVector ctrtName currentRouteName) [] (methodData ctrDetails)
  routeDetailToVector :: String -> String -> [(Vector 4 String, [Vector 4 String])] -> SG.Method -> ApiTypeDetails -> [(Vector 4 String, [Vector 4 String])]
  routeDetailToVector ctrtName routeNameStr accValue currentMethod apiDetails = 
    let qualMethodName = "W." ++ show currentMethod
        topLevelVector = fromMaybeSV $ SV.fromList ["W.ApiContract", ctrtName, qualMethodName, routeNameStr]
        respType = Just $ apiOut apiDetails
        errType = apiErr apiDetails
        formParamType = formParam apiDetails
        queryParamType = queryParam apiDetails
        fileParamType = fileParam apiDetails
        headerParamType = headerIn apiDetails
        requestBodyType = requestBody apiDetails
        contentType = contentTypes apiDetails
        instanceVectorList = 
          catMaybes $ fmap (\(typeInfo, typeLabel) -> fmap (\tInfo -> fromMaybeSV $ SV.fromList [typeLabel, qualMethodName, routeNameStr, tInfo] ) typeInfo) 
            $ DL.zip (respType:errType:formParamType:queryParamType:fileParamType:headerParamType:requestBodyType:contentType:[]) 
                ["ApiOut", "ApiErr","FormParam", "QueryParam", "FileParam", "HeaderIn", "RequestBody", "ContentTypes"]
    in (topLevelVector, instanceVectorList):accValue
  fromMaybeSV = fromJustNote "Expected a list with 4 elements for WebApi instance! "




typeAliasForDecl :: String -> String -> Decl SrcSpanInfo
typeAliasForDecl typeNameStr typeAliasStr =
  TypeDecl noSrcSpan (DHead noSrcSpan (nameDecl typeNameStr)) (typeConstructor typeAliasStr)


-- (DataDecl noSrcSpan 
--   (NewType noSrcSpan) 
--   Nothing 
--   (DHead noSrcSpan (Ident noSrcSpan "X")) 
--   [QualConDecl noSrcSpan Nothing Nothing (ConDecl noSrcSpan (Ident noSrcSpan "X") [TyCon noSrcSpan (UnQual noSrcSpan (Ident noSrcSpan "String"))])] 
--   [Deriving noSrcSpan Nothing [IRule noSrcSpan Nothing Nothing (IHCon noSrcSpan (UnQual noSrcSpan (Ident noSrcSpan "Eq"))),IRule noSrcSpan Nothing Nothing (IHCon noSrcSpan (UnQual noSrcSpan (Ident noSrcSpan "Show"))),IRule noSrcSpan Nothing Nothing (IHCon noSrcSpan (UnQual noSrcSpan (Ident noSrcSpan "Generic")))]])

#if MIN_VERSION_haskell_src_exts(1,20,0)
dataDeclaration :: (DataOrNew SrcSpanInfo) -> String -> Either String InnerRecords -> [DerivingClass] -> (ModifiedRecords, Decl SrcSpanInfo)
dataDeclaration dataOrNew dataName eStringInnerRecords derivingList = 
  let (modRecords, constructorDecl) = either (\tName -> ([], newTypeConstructorDecl dataName tName))  (constructorDeclaration dataName ) eStringInnerRecords
      decl =
        DataDecl noSrcSpan  
          dataOrNew 
          Nothing 
          (declarationHead dataName)
          constructorDecl
          [derivingDecl derivingList]
  in (modRecords, decl)
#else
dataDeclaration :: (DataOrNew SrcSpanInfo) -> String -> Either String InnerRecords -> [DerivingClass] -> (ModifiedRecords, Decl SrcSpanInfo)
dataDeclaration dataOrNew dataName eStringInnerRecords derivingList = 
  let (modRecords, constructorDecl) = either (\tName -> ([], newTypeConstructorDecl dataName tName))  (constructorDeclaration dataName ) eStringInnerRecords
      decl =
        DataDecl noSrcSpan  
          dataOrNew 
          Nothing 
          (declarationHead dataName)
          constructorDecl
          (Just $ derivingDecl derivingList)
  in (modRecords, decl)
#endif


declarationHead :: String -> DeclHead SrcSpanInfo
declarationHead declHeadName = (DHead noSrcSpan (Ident noSrcSpan declHeadName) )

newTypeConstructorDecl :: String -> String -> [QualConDecl SrcSpanInfo]
newTypeConstructorDecl consName tyName = 
  [QualConDecl noSrcSpan Nothing Nothing (ConDecl noSrcSpan (nameDecl consName) [typeConstructor tyName])] 


constructorDeclaration :: String -> InnerRecords -> (ModifiedRecords, [QualConDecl SrcSpanInfo])
constructorDeclaration constructorName innerRecords = 
  let (mModRecords, fieldDecls) = DL.unzip (fmap fieldDecl innerRecords)
      modRecords = catMaybes mModRecords
      qualConDecl = [QualConDecl noSrcSpan Nothing Nothing (RecDecl noSrcSpan (nameDecl constructorName) fieldDecls )]
  in (modRecords, qualConDecl)


stringLiteral :: String -> Exp SrcSpanInfo
stringLiteral str = (Lit noSrcSpan (LHE.String noSrcSpan str str))

variableName :: String -> Exp SrcSpanInfo
variableName nameStr = (Var noSrcSpan (UnQual noSrcSpan (nameDecl nameStr) ) )

nameDecl :: String -> Name SrcSpanInfo
nameDecl = Ident noSrcSpan 

fieldDecl :: (String, String) -> (Maybe (String, String), FieldDecl SrcSpanInfo)
fieldDecl (fieldName, fieldType) = do
  let (isChanged, fName) = setValidFieldName fieldName
  let mModRecord = 
        case isChanged of
          True -> Just (fieldName, fName)
          False -> Nothing
  let fDecl = FieldDecl noSrcSpan [nameDecl fName] (TyCon noSrcSpan (UnQual noSrcSpan (nameDecl fieldType)))
  (mModRecord, fDecl)


setValidConstructorId :: String -> String
setValidConstructorId str = 
  let (_, validName) = setValidFieldName str 
  in (Char.toUpper $ DL.head validName):(DL.tail validName)

setValidFieldName :: String -> (Bool, String)
setValidFieldName fldName = do
  -- Replace invalidId Chars, check if hs keyword and modify else return
  let (isChanged, invalidsFixed) = fixInvalidId fldName
  case isHsKeyword invalidsFixed of 
    True -> (True, invalidsFixed ++ "_")
    False -> (isChanged, invalidsFixed)

 where
  isHsKeyword :: String -> Bool
  isHsKeyword str = DL.elem str haskellKeywords


fixInvalidId :: String -> (Bool, String)
fixInvalidId idVal
    | idVal == "" = error "Encountered potential empty Haskell Identifier! Please check the Swagger JSON!" 
    | idVal == "_" = (True, "holeName") -- ?? TODO : Is this allowed? Discuss
    | idVal == "\'" = (True, "singleQuoteId") -- TODO : Is this allowed?
    | DL.length idVal == 1 && isValidHsIdChar (DL.head idVal) = (False, fmap Char.toLower idVal)
    | otherwise = do
      let newVal = replaceInvalidChars ("",DL.tail idVal) (DL.head idVal) 
      let lCaseNewVal = makeFirstCharAlpha $ (Char.toLower $ DL.head newVal):(DL.tail newVal)
      case lCaseNewVal == idVal of
        True -> (False, lCaseNewVal)
        False -> (True, lCaseNewVal)

 where

  replaceInvalidChars :: (String, String) -> Char -> String
  replaceInvalidChars (prev, next) currentChar = 
    if isValidHsIdChar currentChar && (not $ DL.null next) 
    then replaceInvalidChars (prev ++ [currentChar], DL.tail next) (DL.head next)
    else if isValidHsIdChar currentChar
         then prev ++ [currentChar]
         -- check for a prefix of invalid chars and return the rest of the next chars
         else do
          let newNext = snd $ DL.break isValidHsIdChar next
          case DL.null newNext of
            True -> prev ++ "_"
            False -> replaceInvalidChars (prev ++ "_", DL.tail newNext ) (DL.head newNext)

  isValidHsIdChar :: Char -> Bool 
  isValidHsIdChar x = (Char.isAlphaNum x) || x == '_' || x == '\''
  
  makeFirstCharAlpha :: String -> String
  makeFirstCharAlpha inpString = 
    case inpString of
      [] -> error "Encountered potential empty Haskell Identifier! Please check the Swagger JSON!"
      firstChar:_ -> 
        case Char.isAlpha firstChar of
          True -> inpString
          False -> 'h':inpString 


derivingDecl :: [String] -> Deriving SrcSpanInfo
#if MIN_VERSION_haskell_src_exts(1,20,0)
derivingDecl derivingList = Deriving noSrcSpan Nothing $ fmap iRule derivingList
#else
derivingDecl derivingList = Deriving noSrcSpan $ fmap iRule derivingList
#endif
 where 
  iRule :: String -> InstRule SrcSpanInfo
  iRule tClass = IRule noSrcSpan Nothing Nothing (IHCon noSrcSpan (UnQual noSrcSpan (nameDecl tClass)))

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



complexSumTypeDecl :: String -> [(String, String)] -> [DerivingClass] -> Decl SrcSpanInfo
complexSumTypeDecl dataName constructorsAndTypes derivingList =
  DataDecl noSrcSpan 
    (DataType noSrcSpan) Nothing 
    (declarationHead dataName) 
    (fmap tConstructors constructorsAndTypes)
#if MIN_VERSION_haskell_src_exts(1,20,0)
    [derivingDecl derivingList]
#else
    (Just $ derivingDecl derivingList)
#endif
 where 
  tConstructors :: (String, String) -> QualConDecl SrcSpanInfo
  tConstructors (cName, consType) = 
    QualConDecl noSrcSpan Nothing Nothing 
      (ConDecl noSrcSpan (nameDecl cName) [typeConstructor consType])


enumTypeDeclaration :: String -> [String] -> [DerivingClass] -> Decl SrcSpanInfo
enumTypeDeclaration dataName listOfComponents derivingList = 
  DataDecl noSrcSpan  
    (DataType noSrcSpan) Nothing 
    (declarationHead dataName)
    (sumTypeConstructor listOfComponents)
#if MIN_VERSION_haskell_src_exts(1,20,0)
    [derivingDecl derivingList]
#else
    (Just $ derivingDecl derivingList)
#endif
 where 
  sumTypeConstructor :: [String] -> [QualConDecl SrcSpanInfo]
  sumTypeConstructor = 
    fmap (\construcorVal -> QualConDecl noSrcSpan Nothing Nothing 
      (ConDecl noSrcSpan 
        (nameDecl construcorVal) [] ) )

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


apiInstanceDeclaration :: Vector 4 String -> [Vector 4 String] -> Decl SrcSpanInfo
apiInstanceDeclaration topLevelDecl innerTypesInstList = 
  InstDecl noSrcSpan Nothing 
    (IRule noSrcSpan Nothing Nothing
      (IHApp noSrcSpan 
        (IHApp noSrcSpan 
          (IHApp noSrcSpan 
            (instanceHead (SV.index topLevelDecl (Finite 0) ) )
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

instanceHead :: String -> InstHead SrcSpanInfo
instanceHead instName = (IHCon noSrcSpan
                          (UnQual noSrcSpan $ nameDecl instName)
                        ) 


typeConstructor :: String -> Type SrcSpanInfo
typeConstructor typeConName = (TyCon noSrcSpan  
                                (UnQual noSrcSpan $ nameDecl typeConName)
                              )

dataConstructor :: String -> Exp SrcSpanInfo
dataConstructor dataConName = Con noSrcSpan (UnQual noSrcSpan $ nameDecl dataConName)


fromParamInstanceDecl :: Vector 3 String -> Decl SrcSpanInfo 
fromParamInstanceDecl instTypes = 
  InstDecl noSrcSpan Nothing 
      (IRule noSrcSpan Nothing Nothing 
        (IHApp noSrcSpan 
          (IHApp noSrcSpan 
            (instanceHead $ SV.index instTypes (Finite 0) )
            (TyPromoted noSrcSpan (PromotedCon noSrcSpan True (UnQual noSrcSpan (nameDecl $ SV.index instTypes (Finite 1) ))))
          )
          (typeConstructor $ SV.index instTypes (Finite 2) )
          )
        ) 
      Nothing

recursiveTypeForRoute :: [PathComponent] -> Type SrcSpanInfo
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
  processPathComponent :: PathComponent -> Type SrcSpanInfo
  processPathComponent pathComp = 
    case pathComp of
      PathComp pComp -> promotedType pComp
      PathParamType pType -> typeConstructor pType

promotedType :: String -> Type SrcSpanInfo
promotedType typeNameData = 
  (TyPromoted noSrcSpan 
    (PromotedString noSrcSpan typeNameData typeNameData) 
  ) 

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

unQualSymDecl :: String -> QName SrcSpanInfo
unQualSymDecl str = 
  (UnQual noSrcSpan 
    (Symbol noSrcSpan str)
  )
 
patternVariable :: String -> Pat SrcSpanInfo
patternVariable varName = PVar noSrcSpan (nameDecl varName)


-- Show Instance for Enum Type 
instanceDeclForShow :: String -> [Decl SrcSpanInfo]
instanceDeclForShow dataTypeName =
  [InstDecl noSrcSpan Nothing 
    (IRule noSrcSpan Nothing Nothing 
      (IHApp noSrcSpan 
        (instanceHead "P.Show") 
        (typeConstructor dataTypeName)
      )
    )
    (Just 
      [InsDecl noSrcSpan 
        (FunBind noSrcSpan 
          [Match noSrcSpan (Ident noSrcSpan "show") 
            [PVar noSrcSpan (Ident noSrcSpan "st'")] 
            (UnGuardedRhs noSrcSpan (InfixApp noSrcSpan (Var noSrcSpan (Qual noSrcSpan (ModuleName noSrcSpan "ASCII") (nameDecl "unpack"))) 
              (QVarOp noSrcSpan (unQualSymDecl "$") ) 
              (App noSrcSpan 
                (variableName "encodeParam")
                (variableName "st'")
              ))) Nothing])]) ]

-- Instances for ToJSON and FromJSON For Sum Types
instanceDeclForJSONForSumType :: String -> [Decl SrcSpanInfo]
instanceDeclForJSONForSumType dataTypeName = [toJsonInstance, fromJsonInstance]
 where 
  toJsonInstance = 
    InstDecl noSrcSpan Nothing 
      (IRule noSrcSpan Nothing Nothing 
        (IHApp noSrcSpan 
          (instanceHead "P.ToJSON") 
          (typeConstructor dataTypeName)
        )
      )
      (Just 
        [InsDecl noSrcSpan 
          (FunBind noSrcSpan 
            [Match noSrcSpan 
              (nameDecl "toJSON") 
              [PVar noSrcSpan (nameDecl "enumVal")] 
              (UnGuardedRhs noSrcSpan (InfixApp noSrcSpan (dataConstructor "String")
                  (QVarOp noSrcSpan (unQualSymDecl "$") ) 
                  (InfixApp noSrcSpan (variableName "pack") 
                    (QVarOp noSrcSpan (unQualSymDecl "$")) 
                    (App noSrcSpan 
                      (variableName "show") 
                      (variableName "enumVal")
                    )
                  )
                )) Nothing])])
  fromJsonInstance = 
    InstDecl noSrcSpan Nothing 
      (IRule noSrcSpan Nothing Nothing 
        (IHApp noSrcSpan 
          (instanceHead "P.FromJSON") 
          (typeConstructor dataTypeName)
        )
      ) 
      (Just 
        [InsDecl noSrcSpan 
          (FunBind noSrcSpan
            [Match noSrcSpan (nameDecl "parseJSON") 
              [PVar noSrcSpan (nameDecl "jsonVal")] 
              (UnGuardedRhs noSrcSpan 
                (App noSrcSpan 
                  (App noSrcSpan 
                    (App noSrcSpan (variableName "withText") (stringLiteral "Expected Text in the JSON!" ) ) 
                    (Paren noSrcSpan (Lambda noSrcSpan [PVar noSrcSpan (nameDecl "textVal")] 
                      (Case noSrcSpan (InfixApp noSrcSpan (variableName "decodeParam") (QVarOp noSrcSpan (unQualSymDecl "$")) (App noSrcSpan (variableName "encodeUtf8") (variableName "textVal") )) 
                        [Alt noSrcSpan 
                          (PApp noSrcSpan (UnQual noSrcSpan (nameDecl "P.Just")) [PVar noSrcSpan (nameDecl "x")]) 
                          (UnGuardedRhs noSrcSpan 
                            (App noSrcSpan (variableName "pure") (variableName "x") )) 
                          Nothing
                        ,Alt noSrcSpan 
                          (PApp noSrcSpan (UnQual noSrcSpan (nameDecl "P.Nothing")) []) 
                          (UnGuardedRhs noSrcSpan (App noSrcSpan (variableName "error") (stringLiteral "Failed while parsing Status value from JSON")))
                          Nothing ]
                      )))) (variableName"jsonVal") )) Nothing])])


jsonInstances :: String -> ModifiedRecords -> [Decl SrcSpanInfo]
jsonInstances dataTypeName modRecords = [jsonInstance ToJson, jsonInstance FromJson]
 where
  jsonInstance :: JsonDirection -> Decl SrcSpanInfo
  jsonInstance jsonDirection = 
    case modRecords of
      [] -> 
        InstDecl noSrcSpan Nothing 
          (IRule noSrcSpan Nothing Nothing 
            (IHApp noSrcSpan 
              (instanceHead $ show jsonDirection) 
              (typeConstructor dataTypeName)
            )
          ) Nothing
      modRecList -> do
        let (outerFn, genericFn) = getEncodingFnStr jsonDirection
        InstDecl noSrcSpan Nothing 
          (IRule noSrcSpan Nothing Nothing 
            (IHApp noSrcSpan 
              (instanceHead $ show jsonDirection) 
              (typeConstructor dataTypeName)
            )
          ) 
          (Just [
            InsDecl noSrcSpan 
              (PatBind noSrcSpan 
                (PVar noSrcSpan (nameDecl outerFn)) 
                  (UnGuardedRhs noSrcSpan (
                    InfixApp noSrcSpan (variableName genericFn)
                    (QVarOp noSrcSpan (Qual noSrcSpan (ModuleName noSrcSpan "P") (Symbol noSrcSpan "$"))) 
                    (RecUpdate noSrcSpan (variableName "defaultOptions")
                        [FieldUpdate noSrcSpan (Qual noSrcSpan (ModuleName noSrcSpan "P") (Ident noSrcSpan "fieldLabelModifier")) 
                        (InfixApp noSrcSpan (variableName "keyMapping")
                            (QVarOp noSrcSpan (Qual noSrcSpan (ModuleName noSrcSpan "P") (Symbol noSrcSpan "$"))) 
                            (App noSrcSpan (Var noSrcSpan (Qual noSrcSpan (ModuleName noSrcSpan "HM") (nameDecl "fromList"))) 
                                (List noSrcSpan (fmap changedFieldModsHM modRecList))
                            )
                        ) ] ) ) ) Nothing) 
                ] )
  changedFieldModsHM :: (String, String) -> Exp SrcSpanInfo
  changedFieldModsHM (modFieldName, ogFieldName) = 
    Tuple noSrcSpan Boxed [stringLiteral ogFieldName, stringLiteral modFieldName]
  
  getEncodingFnStr :: JsonDirection -> (String, String)
  getEncodingFnStr jsonDir = 
    case jsonDir of
      ToJson -> ("toEncoding", "genericToEncoding")
      FromJson -> ("parseJSON", "genericParseJSON")

queryParamInstanceIRule :: String -> String -> InstRule SrcSpanInfo
queryParamInstanceIRule paramDirection sumTypeName = 
  IRule noSrcSpan Nothing Nothing 
    (IHApp noSrcSpan 
      (IHApp noSrcSpan 
        (instanceHead paramDirection)
        (typeConstructor "P.QueryParam") )
        -- (TyPromoted noSrcSpan (PromotedCon noSrcSpan True (UnQual noSrcSpan (nameDecl "QueryParam") ) ) ) ) 
      (typeConstructor sumTypeName) )

-- The ToParam 'QueryParam instance for Sum Type
toParamQueryParamInstance :: String -> Decl SrcSpanInfo
toParamQueryParamInstance sumTypeName = 
  let sumTypeVarName = (fmap Char.toLower sumTypeName) ++ "'"
  in InstDecl noSrcSpan Nothing
      (queryParamInstanceIRule "P.ToParam" sumTypeName)
      ( Just [InsDecl noSrcSpan 
              (FunBind noSrcSpan 
                [Match noSrcSpan 
                  (nameDecl "toParam") 
                  [PWildCard noSrcSpan , PVar noSrcSpan (nameDecl "pfx'"),PVar noSrcSpan (nameDecl sumTypeVarName)] 
                  (UnGuardedRhs noSrcSpan 
                    (List noSrcSpan 
                      [Tuple noSrcSpan 
                        Boxed 
                        [Var noSrcSpan 
                          (UnQual noSrcSpan 
                            (nameDecl "pfx'")
                          ),
                        InfixApp noSrcSpan 
                          (dataConstructor "P.Just")
                          (QVarOp noSrcSpan (Qual noSrcSpan (ModuleName noSrcSpan "P") (Symbol noSrcSpan "$"))) 
                          (App noSrcSpan 
                            (variableName "encodeParam")
                            (variableName sumTypeVarName )
                          )
                        ]])) 
                  Nothing ])])


encodeCaseStatementOption :: (String, String) -> Alt SrcSpanInfo
encodeCaseStatementOption (caseMatchOn, caseResult) = 
  Alt noSrcSpan 
    (PApp noSrcSpan 
      (UnQual noSrcSpan 
        (nameDecl caseMatchOn)
      ) 
      []
    ) 
    (UnGuardedRhs noSrcSpan (stringLiteral caseResult)  ) 
    Nothing
                

decodeCaseStatementOption :: (String, String) -> Alt SrcSpanInfo
decodeCaseStatementOption (caseMatchOnStr, resultOfCaseMatch) = 
  Alt noSrcSpan 
    (PLit noSrcSpan (Signless noSrcSpan) (LHE.String noSrcSpan caseMatchOnStr caseMatchOnStr ) ) 
    (UnGuardedRhs noSrcSpan (App noSrcSpan (dataConstructor "P.Just") (dataConstructor resultOfCaseMatch) )) 
    Nothing

-- the EncodeParam instance for Sum Type
encodeParamSumTypeInstance :: String -> [(String, String)] -> Decl SrcSpanInfo
encodeParamSumTypeInstance sumTypeName caseOptions =
  let sumTypeVarName = (fmap Char.toLower sumTypeName) ++ "'"
  in InstDecl noSrcSpan Nothing
      (IRule noSrcSpan Nothing Nothing (IHApp noSrcSpan (instanceHead "P.EncodeParam") (typeConstructor sumTypeName) ))
      (Just [InsDecl noSrcSpan 
        (FunBind noSrcSpan 
          [Match noSrcSpan 
            (nameDecl "encodeParam") 
            [PVar noSrcSpan (nameDecl sumTypeVarName)] 
            (UnGuardedRhs noSrcSpan 
              (Case noSrcSpan 
                (variableName sumTypeVarName)
                (fmap encodeCaseStatementOption caseOptions) )) 
            Nothing
          ])])


-- The DecodeParam Instance for Sum Type
decodeParamSumTypeInstance :: String -> [(String, String)] -> Decl SrcSpanInfo
decodeParamSumTypeInstance sumTypeName caseOptions = 
  let sumTypeVarName = (fmap Char.toLower sumTypeName) ++ "'"
  in InstDecl noSrcSpan Nothing
      (IRule noSrcSpan Nothing Nothing (IHApp noSrcSpan (instanceHead "P.DecodeParam") (typeConstructor sumTypeName) ))
      (Just [InsDecl noSrcSpan 
        (FunBind noSrcSpan 
          [Match noSrcSpan (nameDecl "decodeParam") 
            [PVar noSrcSpan (nameDecl sumTypeVarName)] 
            (UnGuardedRhs noSrcSpan 
              (Case noSrcSpan 
                (variableName sumTypeVarName) 
                ((fmap decodeCaseStatementOption caseOptions) ++ [Alt noSrcSpan (PWildCard noSrcSpan) (UnGuardedRhs noSrcSpan (dataConstructor "P.Nothing") ) Nothing] ) )) 
            Nothing
          ])])

-- The FromParam 'QueryParam instance for Sum Type
fromParamQueryParamInstance :: String -> Decl SrcSpanInfo
fromParamQueryParamInstance sumTypeName = 
  InstDecl noSrcSpan Nothing
  (IRule noSrcSpan Nothing Nothing 
    (IHApp noSrcSpan 
      (instanceHead "P.DecodeParam") 
      (typeConstructor sumTypeName) ))
  -- (queryParamInstanceIRule "P.FromParam" sumTypeName)
  (Just 
    [InsDecl noSrcSpan 
      (FunBind noSrcSpan 
        [Match noSrcSpan 
          (nameDecl "fromParam") 
          (fmap patternVariable ["pt'","key'","kvs'"])
          (UnGuardedRhs noSrcSpan 
            (Case noSrcSpan 
              (App noSrcSpan 
                (App noSrcSpan 
                  (App noSrcSpan 
                    (variableName "lookupParam")                    
                    (variableName "pt'") ) 
                  (variableName "key'") )
                (variableName"kvs'")
              ) 
              [Alt noSrcSpan 
                (PApp noSrcSpan 
                  (Qual noSrcSpan (ModuleName noSrcSpan "P") 
                    (nameDecl "Just")
                  ) 
                  [PParen noSrcSpan 
                    (PApp noSrcSpan 
                      (Qual noSrcSpan (ModuleName noSrcSpan "P") 
                        (nameDecl "Just")
                      ) [patternVariable "par'"]
                    )
                  ]
                ) 
                (UnGuardedRhs noSrcSpan 
                  (Do noSrcSpan 
                    [Qualifier noSrcSpan 
                      (Case noSrcSpan 
                        (App noSrcSpan 
                          (variableName "decodeParam")
                          (variableName "par'")
                        ) 
                        [Alt noSrcSpan 
                          (PApp noSrcSpan 
                            (Qual noSrcSpan (ModuleName noSrcSpan "P") (nameDecl "Just")) 
                            [patternVariable "v"]
                          ) 
                          (UnGuardedRhs noSrcSpan 
                            (InfixApp noSrcSpan 
                              (dataConstructor "Validation") 
                              (QVarOp noSrcSpan 
                                (unQualSymDecl "$")
                              ) 
                              (App noSrcSpan 
                                (dataConstructor "Right") 
                                (variableName "v")
                              )
                            )
                          ) 
                          Nothing,
                        Alt noSrcSpan 
                          (PWildCard noSrcSpan) 
                          (UnGuardedRhs noSrcSpan 
                            (InfixApp noSrcSpan 
                              (dataConstructor "Validation")
                              (QVarOp noSrcSpan 
                                (unQualSymDecl "$")
                              ) 
                              (App noSrcSpan 
                                (dataConstructor "Left")
                                (List noSrcSpan 
                                  [App noSrcSpan 
                                    (App noSrcSpan 
                                      (dataConstructor "ParseErr")
                                      (variableName "key'")
                                    ) 
                                    (stringLiteral ("Unable to cast to " ++ sumTypeName) )
                                  ]
                                )
                              )
                            )
                          ) 
                          Nothing
                        ]
                      )
                    ]
                  )
                ) 
                Nothing,
                Alt noSrcSpan 
                  (PWildCard noSrcSpan) 
                  (UnGuardedRhs noSrcSpan 
                    (InfixApp noSrcSpan 
                      (dataConstructor "Validation")
                      (QVarOp noSrcSpan 
                        (unQualSymDecl "$")
                      ) 
                      (App noSrcSpan 
                        (dataConstructor "Left")
                        (List noSrcSpan 
                          [App noSrcSpan 
                            (dataConstructor "NotFound")
                            (variableName "key'")
                          ]
                        )
                      )
                    )
                  ) 
                  Nothing
                ]
              )
            ) 
            Nothing
          ]
        )
      ]
    )


routeDeclaration :: String -> [PathComponent] -> Decl SrcSpanInfo
routeDeclaration routeNameStr routePathComponents = 
  case routePathComponents of
    (PathComp _):[] -> 
      TypeDecl noSrcSpan
        (declarationHead routeNameStr)
        (TyApp noSrcSpan
          (typeConstructor "W.Static")
          (recursiveTypeForRoute routePathComponents) )
    _ -> 
      TypeDecl noSrcSpan  
        (declarationHead routeNameStr)
        (recursiveTypeForRoute routePathComponents)


webApiInstance :: String -> [(String, [String])] -> Decl SrcSpanInfo
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
  innerRouteInstance :: (String, [String]) -> Type SrcSpanInfo
  innerRouteInstance (rName, listOfMethods) =
      TyApp noSrcSpan 
        (TyApp noSrcSpan 
          (typeConstructor "W.Route") 
          (TyPromoted noSrcSpan 
            (PromotedList noSrcSpan True 
              (fmap typeConstructor listOfMethods)               
            )
          )
        ) 
        (typeConstructor rName)


defaultToParamInstance :: String -> String -> Decl SrcSpanInfo
defaultToParamInstance dataTypeName paramType =
  InstDecl noSrcSpan Nothing 
    (IRule noSrcSpan Nothing Nothing 
      (IHApp noSrcSpan 
        (IHApp noSrcSpan 
          (instanceHead "P.ToParam") 
          (TyPromoted noSrcSpan (PromotedCon noSrcSpan True (UnQual noSrcSpan (nameDecl paramType))))) 
        (typeConstructor dataTypeName) )) 
    Nothing

defaultToSchemaInstance :: String -> Decl SrcSpanInfo
defaultToSchemaInstance dataTypeName =
  InstDecl noSrcSpan Nothing 
    (IRule noSrcSpan Nothing Nothing 
      (IHApp noSrcSpan 
        (instanceHead "ToSchema") 
        (typeConstructor dataTypeName)
      )
    ) Nothing

toSchemaInstanceForSumType :: String -> [(String, String)] -> [Decl SrcSpanInfo]
toSchemaInstanceForSumType typeName constructorValues = 
  toSchemaInst:multiSetToSchemaInst:multiSetToParamSchemaInst:[]
 where

  toSchemaInst :: Decl SrcSpanInfo
  toSchemaInst =
    InstDecl noSrcSpan Nothing 
      (IRule noSrcSpan Nothing Nothing 
        (IHApp noSrcSpan 
          (instanceHead "ToSchema") 
          (typeConstructor typeName)
        )
      ) 
    (Just [InsDecl noSrcSpan 
    (PatBind noSrcSpan 
      (PVar noSrcSpan 
        (nameDecl "declareNamedSchema")
      ) 
      (UnGuardedRhs noSrcSpan 
        (App noSrcSpan 
          (Var noSrcSpan (UnQual noSrcSpan (nameDecl "genericDeclareNamedSchema"))) 
          (Paren noSrcSpan 
            (App noSrcSpan 
              (App noSrcSpan 
                (App noSrcSpan 
                  (App noSrcSpan 
                    (App noSrcSpan 
                      (dataConstructor "SchemaOptions")
                      (Var noSrcSpan (Qual noSrcSpan (ModuleName noSrcSpan "P") (nameDecl "id")))) 
                      (Paren noSrcSpan 
                        (Lambda noSrcSpan [PVar noSrcSpan (nameDecl "inputConst")] 
                          (Case noSrcSpan 
                            (Var noSrcSpan (UnQual noSrcSpan (nameDecl "inputConst"))) 
                            (fmap caseMatchStatement constructorValues ++ errorCaseMatch)
                          )
                        )
                      )
                  ) 
                  (Var noSrcSpan (Qual noSrcSpan (ModuleName noSrcSpan "P") (nameDecl "id")))
                ) 
                (dataConstructor "True")
            ) 
            (dataConstructor "False")
          )
        )
      )
    ) 
    Nothing)])

  caseMatchStatement :: (String, String) -> Alt SrcSpanInfo  
  caseMatchStatement (lowerCaseCons, typeCons) = 
    (Alt noSrcSpan 
      (PLit noSrcSpan (Signless noSrcSpan) (LHE.String noSrcSpan typeCons typeCons)) 
      (UnGuardedRhs noSrcSpan (stringLiteral lowerCaseCons) ) Nothing)
  
  errorCaseMatch :: [Alt SrcSpanInfo]
  errorCaseMatch = 
    [Alt noSrcSpan 
      (PWildCard noSrcSpan) 
        (UnGuardedRhs noSrcSpan 
          (App noSrcSpan (variableName "error") (stringLiteral "Encountered invalid constructor value for sum type!"))
        ) Nothing]

  multiSetToSchemaInst :: Decl SrcSpanInfo
  multiSetToSchemaInst = 
    (InstDecl noSrcSpan Nothing 
      (IRule noSrcSpan Nothing Nothing 
        (IHApp noSrcSpan 
          (instanceHead "ToSchema") 
          (TyParen noSrcSpan 
            (TyApp noSrcSpan 
              (typeConstructor "P.MultiSet") 
              (typeConstructor typeName)
            )
          )
        )
      ) 
      (Just 
        [InsDecl noSrcSpan 
          (PatBind noSrcSpan 
            (PVar noSrcSpan (Ident noSrcSpan "declareNamedSchema")) 
            (UnGuardedRhs noSrcSpan 
              (InfixApp noSrcSpan 
                (Var noSrcSpan (UnQual noSrcSpan (Ident noSrcSpan "plain"))) 
                (QVarOp noSrcSpan (UnQual noSrcSpan (Symbol noSrcSpan "."))) 
                (Var noSrcSpan (UnQual noSrcSpan (Ident noSrcSpan "paramSchemaToSchema")))
              )
            ) Nothing ) ] ) )

  multiSetToParamSchemaInst :: Decl SrcSpanInfo
  multiSetToParamSchemaInst = 
    (InstDecl noSrcSpan Nothing 
      (IRule noSrcSpan Nothing Nothing 
        (IHApp noSrcSpan 
          (instanceHead "ToParamSchema")
          (TyParen noSrcSpan 
            (TyApp noSrcSpan 
              (typeConstructor "P.MultiSet") 
              (typeConstructor typeName)
            )
          )
        )
      )  
    (Just 
      [InsDecl noSrcSpan 
        (FunBind noSrcSpan 
          [Match noSrcSpan 
            (Ident noSrcSpan "toParamSchema") 
            [PWildCard noSrcSpan] 
            (UnGuardedRhs noSrcSpan 
              (InfixApp noSrcSpan 
                (InfixApp noSrcSpan 
                  (InfixApp noSrcSpan 
                    (InfixApp noSrcSpan 
                      (Var noSrcSpan (UnQual noSrcSpan (Ident noSrcSpan "mempty"))) 
                      (QVarOp noSrcSpan (UnQual noSrcSpan (Symbol noSrcSpan "&"))) 
                      (Var noSrcSpan (Qual noSrcSpan (ModuleName noSrcSpan "SW") (Ident noSrcSpan "type_")))
                    ) 
                    (QVarOp noSrcSpan (UnQual noSrcSpan (Symbol noSrcSpan ".~"))) 
                    (Con noSrcSpan (UnQual noSrcSpan (Ident noSrcSpan "SwaggerArray")))
                  ) 
                  (QVarOp noSrcSpan (UnQual noSrcSpan (Symbol noSrcSpan "&"))) 
                  (Var noSrcSpan (Qual noSrcSpan (ModuleName noSrcSpan "SW") (Ident noSrcSpan "items")))
                ) 
                (QVarOp noSrcSpan (UnQual noSrcSpan (Symbol noSrcSpan "?~"))) 
                (App noSrcSpan 
                  (App noSrcSpan 
                    (Con noSrcSpan 
                      (UnQual noSrcSpan (Ident noSrcSpan "SwaggerItemsPrimitive"))) 
                    (Con noSrcSpan (UnQual noSrcSpan (Ident noSrcSpan "Nothing")))
                  ) 
                  (Paren noSrcSpan 
                    (InfixApp noSrcSpan 
                      (InfixApp noSrcSpan 
                        (InfixApp noSrcSpan 
                          (InfixApp noSrcSpan 
                            (Var noSrcSpan (UnQual noSrcSpan (Ident noSrcSpan "mempty"))) 
                            (QVarOp noSrcSpan (UnQual noSrcSpan (Symbol noSrcSpan "&"))) 
                            (Var noSrcSpan (Qual noSrcSpan (ModuleName noSrcSpan "SW") (Ident noSrcSpan "type_")))
                          ) 
                          (QVarOp noSrcSpan (UnQual noSrcSpan (Symbol noSrcSpan ".~"))) 
                          (Con noSrcSpan (UnQual noSrcSpan (Ident noSrcSpan "SwaggerString")))
                        ) 
                        (QVarOp noSrcSpan (UnQual noSrcSpan (Symbol noSrcSpan "&"))) 
                        (Var noSrcSpan (Qual noSrcSpan (ModuleName noSrcSpan "SW") (Ident noSrcSpan "enum_")))
                      ) 
                      (QVarOp noSrcSpan (UnQual noSrcSpan (Symbol noSrcSpan "?~"))) 
                      (List noSrcSpan 
                        (fmap enumConstructor constructorValues)
                      )
                    )
                  )))) Nothing])]))

  enumConstructor :: (String, String) -> Exp SrcSpanInfo
  enumConstructor (ogCons, _) = 
    App noSrcSpan 
            (dataConstructor "String") 
            (stringLiteral ogCons)




writeCabalAndProjectFiles :: FilePath -> String -> Bool -> [String] -> IO ()
writeCabalAndProjectFiles generationPath projectName needsWebapiXml modulesForImport = do
  writeFile (generationPath ++ projectName ++ ".cabal") (cabalFileContents projectName needsWebapiXml modulesForImport)
  writeFile (generationPath ++ "LICENSE") licenseFileContents
  -- TODO : Once webapi-xml is pushed to GitHub, it needs to be added to the cabal.project file
  writeFile (generationPath ++ "cabal.project") cabalProjectFileContents






---------------------------------------------------------------------------------------
-- Support multiple versions of GHC (Use ifndef )
-- for LTS 9.0 -> 1.18.2
