{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}




module ContractGen where

import Control.Lens hiding (List)
import Data.Aeson 
import Data.Proxy
import Data.Text as T
import GHC.Generics
import qualified Data.ByteString.Lazy as BSL
import Data.HashMap.Strict.InsOrd as HMSIns
import Language.Haskell.Exts hiding (OPTIONS)
import Data.Vector.Sized as SV hiding ((++), foldM, forM, mapM)
import Safe
import Data.Finite.Internal
import qualified Data.HashMap.Lazy as HML
import Network.HTTP.Types.Method
import Data.Maybe
import Data.List.Split as DLS (splitOn)
import qualified Data.List as DL
import Data.String.Conv
import qualified Data.Map.Lazy as Map
import qualified Data.Char as Char
import Control.Monad
import Control.Monad.Trans.State.Strict
import Control.Monad.IO.Class

import Data.Swagger
import Data.Swagger.Declare
import Data.Swagger.Lens
import Data.Swagger.Operation


runDefaultPathCodeGen :: IO ()
runDefaultPathCodeGen = runCodeGen "webapi-swagger/sampleFiles/swagger-petstore-ex.json" "webapi-swagger/src/"


runCodeGen :: FilePath -> FilePath -> IO () 
runCodeGen swaggerJsonInputFilePath contractOutputFolderPath = do
  newTypeCreationList <- execStateT (readSwaggerGenerateDefnModels swaggerJsonInputFilePath contractOutputFolderPath)  [] 
  createNewTypes newTypeCreationList
  liftIO $ putStrLn $ show newTypeCreationList
 where
  createNewTypes typeList = do
    let hTypes = DL.foldl' createType ([]::[Decl SrcSpanInfo]) typeList
    appendFile (contractOutputFolderPath ++ "Types.hs") $ DL.unlines $ fmap prettyPrint hTypes
      
  createType accValue typeInfo = 
    case typeInfo of
#if MIN_VERSION_haskell_src_exts(1,20,0)
      ProductType newData -> accValue ++ [dataDeclaration]
#else
      ProductType newData -> accValue ++ [dataDeclaration (DataType noSrcSpan) (mName newData) (mRecordTypes newData) ["Eq", "Show", "Generic"] ] ++ (instanceDeclForJSON (mName newData) )
#endif
      SumType tName tConstructors -> do
        let toParamEncodeParamQueryParamInstance = [toParamQueryParamInstance tName] ++ [encodeParamSumTypeInstance tName (DL.zip tConstructors ( (fmap . fmap) Char.toLower tConstructors) ) ]
        let fromParamDecodeParamQueryParamInstance = [fromParamQueryParamInstance tName] ++ [decodeParamSumTypeInstance tName (DL.zip ((fmap . fmap) Char.toLower tConstructors) tConstructors ) ]
        accValue ++ ([sumTypeDeclaration tName tConstructors ["Eq", "Show", "Generic"] ] ++ (instanceDeclForJSON tName) ++ toParamEncodeParamQueryParamInstance ++ fromParamDecodeParamQueryParamInstance )



type StateConfig = StateT [CreateNewType] IO ()

readSwaggerGenerateDefnModels :: FilePath -> FilePath -> StateConfig
readSwaggerGenerateDefnModels swaggerJsonInputFilePath contractOutputFolderPath = do 
  petstoreJSONContents <- liftIO $ BSL.readFile swaggerJsonInputFilePath
  contractDetailsFromPetstore <- readSwaggerJSON petstoreJSONContents
  let decodedVal = eitherDecode petstoreJSONContents 
  case decodedVal of
    Left errMsg -> liftIO $ putStrLn errMsg
    Right (swaggerData :: Swagger) -> do
      newData <- generateSwaggerDefinitionData (_swaggerDefinitions swaggerData) 
      let hContractModule = 
            Module noSrcSpan 
              (Just $ ModuleHead noSrcSpan (ModuleName noSrcSpan "Contract") Nothing Nothing)
              (fmap languageExtension ["TypeFamilies", "MultiParamTypeClasses", "DeriveGeneric", "TypeOperators", "DataKinds", "TypeSynonymInstances", "FlexibleInstances"])
              (fmap (moduleImport (False, "")) [ "WebApi.Contract", "WebApi.Param", "Types", "Data.Int", "Data.Text"]) -- CommonTypes
              (generateContractBody "Petstore" contractDetailsFromPetstore)
      liftIO $ writeFile (contractOutputFolderPath ++ "Contract.hs") $ prettyPrint hContractModule
      let hTypesModule = 
            Module noSrcSpan 
                (Just $ ModuleHead noSrcSpan (ModuleName noSrcSpan "Types") Nothing Nothing)
                (fmap languageExtension ["TypeFamilies", "MultiParamTypeClasses", "DeriveGeneric", "TypeOperators", "DataKinds", "TypeSynonymInstances", "FlexibleInstances", "DuplicateRecordFields", "OverloadedStrings"])
                (fmap (moduleImport (False, "")) ["Data.Text","Data.Int","Data.Time.Clock", "Data.Set", "GHC.Generics", "Data.Aeson", "WebApi.Param"]) --"GHC.Generics", "Data.Time.Calendar"
                (createDataDeclarations newData)
      liftIO $ writeFile (contractOutputFolderPath ++ "Types.hs") $ prettyPrint hTypesModule ++ "\n\n"
    
 where 
  createDataDeclarations :: [NewData] -> [Decl SrcSpanInfo]
#if MIN_VERSION_haskell_src_exts(1,20,0)
  createDataDeclarations newDataList = [dataDeclaration] -- fmap (\newDataInfo -> dataDeclaration)
#else
  createDataDeclarations newDataList = DL.foldl' (\accValue newDataInfo -> 
      accValue ++ (dataDeclaration (DataType noSrcSpan) (mName newDataInfo) (mRecordTypes newDataInfo) ["Eq", "Show", "Generic"]):instanceDeclForJSON (mName newDataInfo) ) [] newDataList
#endif
 
-- TODO: This function assumes SwaggerObject to be the type and directly reads from schemaProperties. We need to also take additionalProperties into consideration.
generateSwaggerDefinitionData :: InsOrdHashMap Text Schema -> StateT [CreateNewType] IO [NewData]
generateSwaggerDefinitionData defDataHM = foldlWithKey' parseSwaggerDefinition (pure []) defDataHM
 where 
  parseSwaggerDefinition :: StateT [CreateNewType] IO [NewData] -> Text -> Schema -> StateT [CreateNewType] IO [NewData]
  parseSwaggerDefinition scAccValue modelName modelSchema = do
    accValue <- scAccValue
    let (schemaProperties::InsOrdHashMap Text (Referenced Schema) ) = _schemaProperties modelSchema
    recordNamesAndTypes <- foldlWithKey' (\scAccList innerRecord iRefSchema -> do 
            accList <- scAccList
            let innerRecordName = toS innerRecord
            innerRecordType <- case iRefSchema of
                    Ref referenceName -> pure $ toS $ getReference referenceName
                    Inline irSchema -> ((getTypeFromSwaggerType Nothing (Just irSchema)) . _schemaParamSchema) irSchema
            pure $ (innerRecordName, innerRecordType):accList ) (pure []) schemaProperties
    pure $ (NewData (toS modelName) recordNamesAndTypes):accValue


readSwaggerJSON :: BSL.ByteString -> StateT [CreateNewType] IO [ContractDetails]
readSwaggerJSON petstoreJSONContents= do
  let decodedVal = eitherDecode petstoreJSONContents
  case decodedVal of
    Left errMsg -> error errMsg
    Right (swaggerData :: Swagger) -> HMSIns.foldlWithKey' (parseSwaggerPaths (_swaggerDefinitions swaggerData) ) (pure []) (_swaggerPaths swaggerData)
 where
  parseSwaggerPaths :: Definitions Schema -> StateT [CreateNewType] IO [ContractDetails] -> FilePath -> PathItem -> StateT [CreateNewType] IO [ContractDetails]
  parseSwaggerPaths (swaggerSchema:: InsOrdHashMap Text Schema) contractDetailsList swFilePath swPathDetails = do
    cDetailsList <- contractDetailsList
    let currentRouteId = case cDetailsList of 
          [] -> 1
          xs -> (routeId $ Prelude.head cDetailsList) + 1 -- TODO: Not being used anymore. Should be removed
        splitRoutePath = DLS.splitOn "/" $ removeLeadingSlash swFilePath
        mainRouteName = (DL.concat $ prettifyRouteName splitRoutePath) ++ "R"

    -- TODO: Add a `Static` Type component at the start if the list has just one element of type PathComp
    finalPathWithParamTypes::[PathComponent] <- forM splitRoutePath (\pathComponent -> 
        case isParam pathComponent of 
          True -> do
            let pathParamName = removeCurlyBraces pathComponent
            (mParamNameList::[Maybe String]) <- mapM (getPathParamTypeFromOperation pathParamName) (getListOfPathOperations swPathDetails::[Maybe Operation])
            case (DL.nub . catMaybes) mParamNameList of
              singleParamType:[] -> pure (PathParamType singleParamType)
              otherVal -> error $ "Expected only a single Param Type to be present in all Methods of this path. Instead got : " ++ show otherVal ++ " Path : " ++ swFilePath
          False -> pure (PathComp pathComponent)
          )
         
    let currentRoutePath = finalPathWithParamTypes
        methodList = [GET, PUT, POST, PATCH, DELETE, OPTIONS, HEAD]
    currentMethodData <- Control.Monad.foldM (processPathItem mainRouteName swPathDetails) (Map.empty) methodList
    let currentContractDetails = ContractDetails currentRouteId mainRouteName currentRoutePath currentMethodData
    pure (currentContractDetails:cDetailsList)
  removeLeadingSlash inputRoute = fromMaybe inputRoute (DL.stripPrefix "/" inputRoute)
  prettifyRouteName routeList = case routeList of
    [] -> error "Expected atleast one element in the route! Got an empty list!"
    rList -> fmap (\(firstChar:remainingChar) -> (Char.toUpper firstChar):remainingChar ) $ fmap (DL.filter (\x -> not (x == '{' || x == '}') ) ) rList
  
  isParam (pathComponent::String) = (DL.isPrefixOf "{" pathComponent) && (DL.isSuffixOf "}" pathComponent)
  removeCurlyBraces = DL.filter (\x -> not (x == '{' || x == '}') )
  getListOfPathOperations pathItem = [_pathItemGet pathItem, _pathItemPut pathItem, _pathItemPost pathItem, _pathItemDelete pathItem, _pathItemOptions pathItem, _pathItemHead pathItem, _pathItemPatch pathItem]
  getPathParamTypeFromOperation paramPathName mOperation = case mOperation of
    Just operation -> do 
      let paramList = _operationParameters operation
      mParamType <- foldM (\existingParamType refParam -> 
        case refParam of
          Ref referencedParam -> pure existingParamType
          Inline param -> case (_paramName param == toS paramPathName) of
            True -> 
              case (_paramSchema param) of
                ParamOther paramOtherSchema -> 
                  case _paramOtherSchemaIn paramOtherSchema of
                    ParamPath -> do
                      parameterType <- getTypeFromSwaggerType Nothing Nothing (_paramOtherSchemaParamSchema paramOtherSchema) 
                      pure $ Just parameterType
                    _ -> pure existingParamType
                ParamBody _ -> pure existingParamType 
            False -> pure existingParamType
        ) Nothing paramList 
      pure mParamType 
    Nothing -> pure $ Nothing

  

  processPathItem :: String -> PathItem -> (Map.Map StdMethod ApiTypeDetails) ->  StdMethod -> StateT [CreateNewType] IO (Map.Map StdMethod ApiTypeDetails)
  processPathItem mainRouteName pathItem methodDataAcc currentMethod =
    case currentMethod of
      GET -> (processOperation mainRouteName methodDataAcc) GET $ _pathItemGet pathItem
      PUT -> (processOperation mainRouteName methodDataAcc) PUT $ _pathItemPut pathItem
      POST -> (processOperation mainRouteName methodDataAcc) POST $ _pathItemPost pathItem
      DELETE -> (processOperation mainRouteName methodDataAcc) DELETE $ _pathItemDelete pathItem
      OPTIONS -> (processOperation mainRouteName methodDataAcc) OPTIONS $ _pathItemOptions pathItem
      HEAD -> (processOperation mainRouteName methodDataAcc) HEAD $ _pathItemHead pathItem
      PATCH -> (processOperation mainRouteName methodDataAcc) PATCH $ _pathItemPatch pathItem
      _ -> pure $ Map.empty
  processOperation :: String -> Map.Map StdMethod ApiTypeDetails -> StdMethod -> Maybe Operation -> StateT [CreateNewType] IO (Map.Map StdMethod ApiTypeDetails)
  processOperation currentRouteName methodAcc stdMethod mOperationData = 
    case mOperationData of
      Just operationData -> do
        let apiResponses = _responsesResponses $ _operationResponses operationData
        (mApiOutType, apiErrType) <- getApiType (currentRouteName ++ show stdMethod) apiResponses
        -- TODO: Case match on ApiOut and if `Nothing` then check for default responses in `_responsesDefault $ _operationResponses operationData`
        let apiOutType = fromMaybe "()" mApiOutType
        -- Group the Referenced Params by ParamLocation and then go through each group separately.
        let (formParamList, queryParamList, fileParamList, headerInList) = DL.foldl' (groupParamTypes) ([], [], [], []) (_operationParameters operationData)
        mFormParamType <- getParamTypes (currentRouteName ++ show stdMethod) formParamList FormParam
        mQueryParamType <- getParamTypes (currentRouteName ++ show stdMethod) queryParamList QueryParam
        mFileParamType <- getParamTypes (currentRouteName ++ show stdMethod) fileParamList FileParam
        mHeaderInType <- getParamTypes (currentRouteName ++ show stdMethod) headerInList HeaderParam
        pure $ Map.insert stdMethod (ApiTypeDetails apiOutType apiErrType mFormParamType mQueryParamType mFileParamType mHeaderInType) methodAcc
      Nothing -> pure methodAcc

  groupParamTypes :: ([Param], [Param], [Param], [Param]) -> Referenced Param -> ([Param], [Param], [Param], [Param])
  groupParamTypes (formParamList, queryParamList, fileParamList, headerInList) refParam = 
    case refParam of
      Ref someRef -> error $ "Encountered a Referenced type at the first level of a reference param! Not sure what the use case of this is or how to process it! Debug Info : " ++ show refParam
      Inline param -> 
        case _paramSchema param of
          ParamBody _ -> (param:formParamList, queryParamList, fileParamList, headerInList)
          ParamOther pOtherSchema -> 
            case _paramOtherSchemaIn pOtherSchema of 
              ParamQuery -> (formParamList, param:queryParamList, fileParamList, headerInList) 
              ParamHeader -> (formParamList, queryParamList, fileParamList, param:headerInList)
              ParamPath -> (formParamList, queryParamList, fileParamList, headerInList) 
              ParamFormData ->
                case (_paramSchema param) of
                  ParamBody _ -> (param:formParamList, queryParamList, fileParamList, headerInList) 
                  ParamOther pSchema -> 
                    case (_paramSchemaType $ _paramOtherSchemaParamSchema pSchema) of
                      SwaggerFile -> (formParamList, queryParamList, param:fileParamList, headerInList) 
                      _ -> (param:formParamList, queryParamList, fileParamList, headerInList) 


  getApiType :: String -> InsOrdHashMap HttpStatusCode (Referenced Response)  -> StateT [CreateNewType] IO (Maybe String, Maybe String)
  getApiType newTypeName responsesHM = foldlWithKey' (\stateConfigWrappedTypes currentCode currentResponse -> do
        (apiOutType, apiErrType) <- stateConfigWrappedTypes
        currentResponseType <- parseResponseContentGetType currentResponse
        case (currentCode >= 200 && currentCode <= 208) of
          True -> do
            finalOutType <- do
                fOutType <- checkIfNewType apiOutType currentResponseType (newTypeName ++ "ApiOut")
                pure $ Just fOutType
            pure (finalOutType, apiErrType)
          False -> do
            case (currentCode >= 400 && currentCode <= 431 || currentCode >= 500 && currentCode <= 511) of
              True -> do
                finalErrType <- do
                      fErrType <- checkIfNewType apiErrType currentResponseType (newTypeName ++ "ApiErr")
                      pure $ Just fErrType
                pure (apiOutType, finalErrType)
              False -> error $ "Response code not matched! Response code received is: " ++ show currentCode
    ) (pure (Nothing, Nothing)) responsesHM  
  parseResponseContentGetType :: Referenced Response -> StateT [CreateNewType] IO String
  parseResponseContentGetType refResponse = 
    case refResponse of
      Ref refText -> pure $ toS $ getReference refText
      Inline responseSchema -> 
        case (_responseSchema responseSchema) of
          Just (Ref refText) -> pure $ toS $ getReference refText
          Just (Inline respSchema) -> ((getTypeFromSwaggerType Nothing (Just respSchema) ) . _schemaParamSchema) respSchema
          Nothing -> pure "Text"
  getParamTypes :: String -> [Param] -> ParamType -> StateT [CreateNewType] IO (Maybe String)
  getParamTypes newTypeName paramList paramType = 
    case paramList of
      [] -> pure $ Nothing
      _ -> 
        case paramType of
          FormParam -> do
            let paramNames = fmap (\param -> toS $ _paramName param) paramList
            hTypesWithIsMandatory <- forM paramList (\param -> do 
              hType <- getParamTypeParam param (Just $ toS ( _paramName param) ) Nothing
              pure (isMandatory param, hType) )
            let finalHaskellTypes = fmap (\(isMandatoryType, hType) -> (addMaybeToType isMandatoryType hType) ) hTypesWithIsMandatory
            let recordTypesInfo = DL.zip paramNames finalHaskellTypes
            let newDataTypeName = newTypeName ++ "FormParam"
            let formParamDataInfo = ProductType (NewData newDataTypeName recordTypesInfo)

            modify' (\existingState -> formParamDataInfo:existingState) 
            -- liftIO $ putStrLn  $ show paramList
            pure $ Just newDataTypeName
          QueryParam -> do
            let paramNames = fmap (\param -> toS $ _paramName param) paramList
            hTypesWithIsMandatory <- forM paramList (\param -> do 
              hType <- getParamTypeParam param (Just $ toS ( _paramName param) ) Nothing
              pure (isMandatory param, hType) )
            let finalHaskellTypes = fmap (\(isMandatoryType, hType) -> (addMaybeToType isMandatoryType hType) ) hTypesWithIsMandatory
            let recordTypesInfo = DL.zip paramNames finalHaskellTypes
            let newDataTypeName = newTypeName ++ "QueryParam"
            let queryParamDataInfo = ProductType (NewData newDataTypeName recordTypesInfo)
            modify' (\existingState -> queryParamDataInfo:existingState) 
            pure $ Just newDataTypeName
          HeaderParam -> do
            typeList <- forM paramList (\param -> getParamTypeParam param (Just $ toS ( _paramName param) ) Nothing )
            case typeList of
              [] -> pure Nothing
              x:[] -> pure $ Just x
              x:xs -> error "Handle case of multiple Header Params!"
          FileParam -> do
            typeList <- forM paramList (\param -> getParamTypeParam param (Just $ toS ( _paramName param) ) Nothing )
            case typeList of
              [] -> pure Nothing
              x:[] -> pure $ Just x
              x:xs -> error "Handle case of list of FileParam"
  getParamTypeParam inputParam mParamName mOuterSchema =
    case _paramSchema inputParam of
      ParamBody refSchema -> 
        case refSchema of 
          Ref refType -> pure $ toS (getReference refType)
          Inline rSchema -> getTypeFromSwaggerType Nothing (Just rSchema) (_schemaParamSchema rSchema)
      ParamOther pSchema -> getTypeFromSwaggerType mParamName mOuterSchema $ _paramOtherSchemaParamSchema pSchema

  isMandatory :: Param -> Bool
  isMandatory param = 
    case _paramRequired param of
      Just True -> True
      _ -> False
  addMaybeToType :: Bool -> String -> String
  addMaybeToType isMandatory haskellType = 
    case isMandatory of
      True -> haskellType
      False -> "Maybe " ++ haskellType 





checkIfNewType :: Maybe String -> String -> String -> (StateT [CreateNewType] IO String)
checkIfNewType existingType currentType newTypeName = 
  case existingType of 
    Just eType ->
      if (eType == currentType)
      then pure eType
      else do
        -- TODO: What if the error types are more than just 2? 
        let sumTypeInfo = SumType newTypeName [currentType, eType]
        modify' (\existingState -> sumTypeInfo:existingState) 
        pure newTypeName
    Nothing -> pure currentType

getTypeFromSwaggerType :: Maybe String -> Maybe Schema ->  ParamSchema t -> StateT [CreateNewType] IO String 
getTypeFromSwaggerType mParamName mOuterSchema paramSchema = 
    case (_paramSchemaType paramSchema) of 
      SwaggerString -> -- do-- pure "String" -- add check here for the enum field in param and accordingly create new sumtype/add to StateT and return its name.
        case _paramSchemaFormat paramSchema of
          Just "date" -> pure "Day"
          Just "date-time" -> pure "UTCTime"
          Just "password" -> error $ "Encountered SwaggerString with Format as `password`. This needs to be handled! Debug Info : " ++ show paramSchema
          Just "byte" -> pure "ByteString"
          Just "binary" -> error $ "Encountered SwaggerString with Format as `binary`. This needs to be handled! Debug Info: " ++ show paramSchema
          Nothing -> pure "Text"
          _ -> error $ "Encountered SwaggerString with unknown Format! Debug Info: " ++ show paramSchema
      SwaggerNumber -> 
        case _paramSchemaFormat paramSchema of
          Just "float" -> pure "Float"
          Just "double" -> pure "Double"
          _ -> error $ "Encountered SwaggerNumber without format specification (Double or Float) . Debug Info: " ++ show paramSchema
      SwaggerInteger -> 
        case _paramSchemaFormat paramSchema of
          Just "int32" -> pure "Int32"
          Just "int64" -> pure "Int64"
          _ -> pure "Int"
      SwaggerBoolean -> pure "Bool"
      -- As per the pattern in `PetStore`, for SwaggerArray, we check the Param Schema Items field and look for a reference Name there.
      SwaggerArray -> case _paramSchemaItems paramSchema of
                        Just (SwaggerItemsObject obj) -> 
                          case obj of
                            Ref reference -> pure $ "[" ++ (toS $ getReference reference) ++ "]"
                            Inline recursiveSchema -> do
                              hType <- ( ( (getTypeFromSwaggerType Nothing (Just recursiveSchema) ) . _schemaParamSchema) recursiveSchema)
                              pure $ "[" ++ hType ++ "]"
                        Just (SwaggerItemsArray innerArray) -> checkIfArray $ flip Control.Monad.mapM innerArray (\singleElem -> do
                          case singleElem of
                            Ref ref -> pure $ toS $ getReference ref
                            Inline innerSchema -> ((getTypeFromSwaggerType Nothing (Just innerSchema) ) . _schemaParamSchema) innerSchema) 
                        Just (SwaggerItemsPrimitive mCollectionFormat innerParamSchema) -> do
                          typeName <- do
                                let paramName = fromJustNote ("Expected a Param Name but got Nothing. Need Param Name to set the name for the new type we need to create. Debug Info: " ++ show paramSchema) mParamName
                                let titleCaseParamName = toS $ T.toTitle $ toS paramName
                                case _paramSchemaEnum innerParamSchema of
                                  Just enumVals -> do
                                    let enumValList::[String] = fmap (\(Data.Aeson.String val) -> toS $ T.toTitle val ) enumVals
                                    let haskellNewTypeInfo = SumType titleCaseParamName enumValList
                                    modify'(\existingState -> haskellNewTypeInfo:existingState)
                                    pure titleCaseParamName
                                  Nothing ->  getTypeFromSwaggerType Nothing Nothing innerParamSchema
                          case mCollectionFormat of
                            (Just CollectionMulti) -> pure $ "Set " ++ typeName 
                            (Just CollectionTSV) -> pure $ "Collection TSV " ++ typeName
                            (Just CollectionSSV) -> pure $ "Collection SSV " ++ typeName
                            (Just CollectionPipes) -> pure $ "Collection Pipes " ++ typeName
                            -- Since CSV is the default, the below case takes care of (Just CSV) as well as Nothing
                            _ -> pure $ "Collection CSV " ++ typeName
                        Nothing -> error "Expected a SwaggerItems type due to SwaggerArray ParamSchema Type. But it did not find any! Please check the swagger spec!"
      SwaggerObject -> 
        case mOuterSchema of
          Just outerSchema -> 
            case (HMSIns.toList $ _schemaProperties outerSchema) of
              [] -> 
                case (_schemaAdditionalProperties outerSchema) of
                  Just additionalProps -> 
                    case additionalProps of
                      Ref ref -> pure $ "HashMap Text " ++ (toS $ getReference ref)
                      Inline internalSchema -> ((getTypeFromSwaggerType Nothing (Just internalSchema)) . _schemaParamSchema) internalSchema
                  Nothing -> error $ "Type SwaggerObject but swaggerProperties and additionalProperties are both absent! Debug Info (Schema): " ++ show outerSchema
              propertyList -> do -- TODO: This needs to be changed when we encounter _schemaProperties in some swagger doc/schema.
                innerRecordsInfo <- forM propertyList (\(recordName, iRefSchema) -> do
                      innerRecordType <- case iRefSchema of
                          Ref refName -> pure $ toS $ getReference refName
                          Inline irSchema -> ((getTypeFromSwaggerType Nothing (Just irSchema)) . _schemaParamSchema) irSchema 
                      pure (toS recordName, innerRecordType) )
                let finalProductTypeInfo = ProductType $ NewData "ProductTypeName" innerRecordsInfo
                modify' (\existingState -> finalProductTypeInfo:existingState)
                pure "ProductTypeName"
          Nothing -> error $ "Expected outer schema to be present when trying to construct type of SwaggerObject. Debug Info (ParamSchema):  " ++ show paramSchema
      SwaggerFile -> pure "FileInfo" -- TODO 
      SwaggerNull -> pure "()"
      -- x -> ("Got Unexpected Primitive Value : " ++ show x)
 where 
  checkIfArray scStringList = do
    stringList <- scStringList 
    case DL.nub stringList of
      sameElem:[] -> pure $ "[" ++ sameElem ++ "]"
      x -> error $ "Got different types in the same list. Not sure how to proceed! Please check the swagger doc! " ++ show x


data ParamType = FormParam 
               | QueryParam
               | FileParam
               | HeaderParam
  deriving (Eq, Show)

data ContractDetails = ContractDetails
  {
    routeId :: Int 
  , routeName :: String
  , routePath :: [PathComponent]
  , methodData :: Map.Map StdMethod ApiTypeDetails
  } deriving (Eq, Show)


data ApiTypeDetails = ApiTypeDetails
  {
    apiOut :: String
  , apiErr :: Maybe String
  , formParam :: Maybe String
  , queryParam :: Maybe String
  , fileParam :: Maybe String
  , headerIn :: Maybe String
  -- TODO: cookie in/out and header out need to be added when we encounter them
  } deriving (Eq, Show)

data NewData = NewData
  {
    mName :: String
  , mRecordTypes :: InnerRecords
  } deriving (Eq, Show)

data CreateNewType = SumType String [String] | ProductType NewData 
  deriving (Eq, Show)

data PathComponent = PathComp String | PathParamType String
  deriving (Eq, Show)
    -- _operationParameters = 
    --   [Inline (Param {
    --             _paramName = "status", 
    --             _paramDescription = Just "Status values that need to be considered for filter", 
    --             _paramRequired = Just True, 
    --             _paramSchema = ParamOther (ParamOtherSchema {
    --                                         _paramOtherSchemaIn = ParamQuery, 
    --                                         _paramOtherSchemaAllowEmptyValue = Nothing, 
    --                                         _paramOtherSchemaParamSchema = ParamSchema {
    --                                                                         _paramSchemaDefault = Nothing, 
    --                                                                         _paramSchemaType = SwaggerArray, 
    --                                                                         _paramSchemaFormat = Nothing, 
    --                                                                         _paramSchemaItems = Just (SwaggerItemsPrimitive (Just CollectionMulti) (ParamSchema {
    --                                                                                                                                                 _paramSchemaDefault = Just (String "available"), 
    --                                                                                                                                                 _paramSchemaType = SwaggerString, 
    --                                                                                                                                                 _paramSchemaFormat = Nothing, 
    --                                                                                                                                                 _paramSchemaItems = Nothing, 
    --                                                                                                                                                 _paramSchemaMaximum = Nothing, 
    --                                                                                                                                                 _paramSchemaExclusiveMaximum = Nothing, 
    --                                                                                                                                                 _paramSchemaMinimum = Nothing, 
    --                                                                                                                                                 _paramSchemaExclusiveMinimum = Nothing, 
    --                                                                                                                                                 _paramSchemaMaxLength = Nothing, 
    --                                                                                                                                                 _paramSchemaMinLength = Nothing, 
    --                                                                                                                                                 _paramSchemaPattern = Nothing, 
    --                                                                                                                                                 _paramSchemaMaxItems = Nothing, 
    --                                                                                                                                                 _paramSchemaMinItems = Nothing, 
    --                                                                                                                                                 _paramSchemaUniqueItems = Nothing, 
    --                                                                                                                                                 _paramSchemaEnum = Just [String "available",String "pending",String "sold"], 
    --                                                                                                                                                 _paramSchemaMultipleOf = Nothing})), 
    --                                         _paramSchemaMaximum = Nothing, 
    --                                         _paramSchemaExclusiveMaximum = Nothing, 
    --                                         _paramSchemaMinimum = Nothing, 
    --                                         _paramSchemaExclusiveMinimum = Nothing, 
    --                                         _paramSchemaMaxLength = Nothing, 
    --                                         _paramSchemaMinLength = Nothing, 
    --                                         _paramSchemaPattern = Nothing, 
    --                                         _paramSchemaMaxItems = Nothing, 
    --                                         _paramSchemaMinItems = Nothing, 
    --                                         _paramSchemaUniqueItems = Nothing, 
    --                                         _paramSchemaEnum = Nothing, 
    --                                         _paramSchemaMultipleOf = Nothing}})})], 
    -- _operationResponses = Responses {_responsesDefault = Nothing, _responsesResponses = fromList [(400,Inline (Response {_responseDescription = "Invalid status value", _responseSchema = Nothing, _responseHeaders = fromList [], _responseExamples = Nothing})),(200,Inline (Response {_responseDescription = "successful operation", _responseSchema = Just (Inline (Schema {_schemaTitle = Nothing, _schemaDescription = Nothing, _schemaRequired = [], _schemaAllOf = Nothing, _schemaProperties = fromList [], _schemaAdditionalProperties = Nothing, _schemaDiscriminator = Nothing, _schemaReadOnly = Nothing, _schemaXml = Nothing, _schemaExternalDocs = Nothing, _schemaExample = Nothing, _schemaMaxProperties = Nothing, _schemaMinProperties = Nothing, _schemaParamSchema = ParamSchema {_paramSchemaDefault = Nothing, _paramSchemaType = SwaggerArray, _paramSchemaFormat = Nothing, _paramSchemaItems = Just (SwaggerItemsObject (Ref (Reference {getReference = "Pet"}))), _paramSchemaMaximum = Nothing, _paramSchemaExclusiveMaximum = Nothing, _paramSchemaMinimum = Nothing, _paramSchemaExclusiveMinimum = Nothing, _paramSchemaMaxLength = Nothing, _paramSchemaMinLength = Nothing, _paramSchemaPattern = Nothing, _paramSchemaMaxItems = Nothing, _paramSchemaMinItems = Nothing, _paramSchemaUniqueItems = Nothing, _paramSchemaEnum = Nothing, _paramSchemaMultipleOf = Nothing}})), _responseHeaders = fromList [], _responseExamples = Nothing}))]}, 
    -- _operationSchemes = Nothing, 
    -- _operationDeprecated = Nothing, 
    -- _operationSecurity = [SecurityRequirement {getSecurityRequirement = fromList [("petstore_auth",["write:pets","read:pets"])]}

  -- _swaggerPaths = fromList [("/user/logout",PathItem {_pathItemGet = Just (Operation {_operationTags = fromList ["user"], _operationSummary = Just "Logs out current logged in user session", _operationDescription = Just "", _operationExternalDocs = Nothing, _operationOperationId = Just "logoutUser", _operationConsumes = Nothing, _operationProduces = Just (MimeList {getMimeList = [application/xml,application/json]}), _operationParameters = [], _operationResponses = Responses {_responsesDefault = Just (Inline (Response {_responseDescription = "successful operation", _responseSchema = Nothing, _responseHeaders = fromList [], _responseExamples = Nothing})), _responsesResponses = fromList []}, _operationSchemes = Nothing, _operationDeprecated = Nothing, _operationSecurity = []}), _pathItemPut = Nothing, _pathItemPost = Nothing, _pathItemDelete = Nothing, _pathItemOptions = Nothing, _pathItemHead = Nothing, _pathItemPatch = Nothing, _pathItemParameters = []})
  --   ,("/pet/findByStatus",PathItem {_pathItemGet = Just 
  --     (Operation {_operationTags = fromList ["pet"], 
  --                 _operationSummary = Just "Finds Pets by status", 
  --                 _operationDescription = Just "Multiple status values can be provided with comma separated strings", 
  --                 _operationExternalDocs = Nothing, 
  --                 _operationOperationId = Just "findPetsByStatus", 
  --                 _operationConsumes = Nothing, 
  --                 _operationProduces = Just (MimeList {getMimeList = [application/xml,application/json]}), 
  --                 _operationParameters = [Inline (Param {_paramName = "status", _paramDescription = Just "Status values that need to be considered for filter", _paramRequired = Just True, _paramSchema = ParamOther (ParamOtherSchema {_paramOtherSchemaIn = ParamQuery, _paramOtherSchemaAllowEmptyValue = Nothing, _paramOtherSchemaParamSchema = ParamSchema {_paramSchemaDefault = Nothing, _paramSchemaType = SwaggerArray, _paramSchemaFormat = Nothing, _paramSchemaItems = Just (SwaggerItemsPrimitive (Just CollectionMulti) (ParamSchema {_paramSchemaDefault = Just (String "available"), _paramSchemaType = SwaggerString, _paramSchemaFormat = Nothing, _paramSchemaItems = Nothing, _paramSchemaMaximum = Nothing, _paramSchemaExclusiveMaximum = Nothing, _paramSchemaMinimum = Nothing, _paramSchemaExclusiveMinimum = Nothing, _paramSchemaMaxLength = Nothing, _paramSchemaMinLength = Nothing, _paramSchemaPattern = Nothing, _paramSchemaMaxItems = Nothing, _paramSchemaMinItems = Nothing, _paramSchemaUniqueItems = Nothing, _paramSchemaEnum = Just [String "available",String "pending",String "sold"], _paramSchemaMultipleOf = Nothing})), _paramSchemaMaximum = Nothing, _paramSchemaExclusiveMaximum = Nothing, _paramSchemaMinimum = Nothing, _paramSchemaExclusiveMinimum = Nothing, _paramSchemaMaxLength = Nothing, _paramSchemaMinLength = Nothing, _paramSchemaPattern = Nothing, _paramSchemaMaxItems = Nothing, _paramSchemaMinItems = Nothing, _paramSchemaUniqueItems = Nothing, _paramSchemaEnum = Nothing, _paramSchemaMultipleOf = Nothing}})})], 
  --                 _operationResponses = Responses {_responsesDefault = Nothing, _responsesResponses = fromList [(400,Inline (Response {_responseDescription = "Invalid status value", _responseSchema = Nothing, _responseHeaders = fromList [], _responseExamples = Nothing})),(200,Inline (Response {_responseDescription = "successful operation", _responseSchema = Just (Inline (Schema {_schemaTitle = Nothing, _schemaDescription = Nothing, _schemaRequired = [], _schemaAllOf = Nothing, _schemaProperties = fromList [], _schemaAdditionalProperties = Nothing, _schemaDiscriminator = Nothing, _schemaReadOnly = Nothing, _schemaXml = Nothing, _schemaExternalDocs = Nothing, _schemaExample = Nothing, _schemaMaxProperties = Nothing, _schemaMinProperties = Nothing, _schemaParamSchema = ParamSchema {_paramSchemaDefault = Nothing, _paramSchemaType = SwaggerArray, _paramSchemaFormat = Nothing, _paramSchemaItems = Just (SwaggerItemsObject (Ref (Reference {getReference = "Pet"}))), _paramSchemaMaximum = Nothing, _paramSchemaExclusiveMaximum = Nothing, _paramSchemaMinimum = Nothing, _paramSchemaExclusiveMinimum = Nothing, _paramSchemaMaxLength = Nothing, _paramSchemaMinLength = Nothing, _paramSchemaPattern = Nothing, _paramSchemaMaxItems = Nothing, _paramSchemaMinItems = Nothing, _paramSchemaUniqueItems = Nothing, _paramSchemaEnum = Nothing, _paramSchemaMultipleOf = Nothing}})), _responseHeaders = fromList [], _responseExamples = Nothing}))]}, 
  --                 _operationSchemes = Nothing, 
  --                 _operationDeprecated = Nothing, 
  --                 _operationSecurity = [SecurityRequirement {getSecurityRequirement = fromList [("petstore_auth",["write:pets","read:pets"])]}]}), 
  --       _pathItemPut = Nothing, _pathItemPost = Nothing, _pathItemDelete = Nothing, _pathItemOptions = Nothing, _pathItemHead = Nothing, _pathItemPatch = Nothing, _pathItemParameters = []})
  --   ,("/user/login",PathItem {_pathItemGet = Just (Operation {_operationTags = fromList ["user"], _operationSummary = Just "Logs user into the system", _operationDescription = Just "", _operationExternalDocs = Nothing, _operationOperationId = Just "loginUser", _operationConsumes = Nothing, _operationProduces = Just (MimeList {getMimeList = [application/xml,application/json]}), _operationParameters = [Inline (Param {_paramName = "username", _paramDescription = Just "The user name for login", _paramRequired = Just True, _paramSchema = ParamOther (ParamOtherSchema {_paramOtherSchemaIn = ParamQuery, _paramOtherSchemaAllowEmptyValue = Nothing, _paramOtherSchemaParamSchema = ParamSchema {_paramSchemaDefault = Nothing, _paramSchemaType = SwaggerString, _paramSchemaFormat = Nothing, _paramSchemaItems = Nothing, _paramSchemaMaximum = Nothing, _paramSchemaExclusiveMaximum = Nothing, _paramSchemaMinimum = Nothing, _paramSchemaExclusiveMinimum = Nothing, _paramSchemaMaxLength = Nothing, _paramSchemaMinLength = Nothing, _paramSchemaPattern = Nothing, _paramSchemaMaxItems = Nothing, _paramSchemaMinItems = Nothing, _paramSchemaUniqueItems = Nothing, _paramSchemaEnum = Nothing, _paramSchemaMultipleOf = Nothing}})}),Inline (Param {_paramName = "password", _paramDescription = Just "The password for login in clear text", _paramRequired = Just True, _paramSchema = ParamOther (ParamOtherSchema {_paramOtherSchemaIn = ParamQuery, _paramOtherSchemaAllowEmptyValue = Nothing, _paramOtherSchemaParamSchema = ParamSchema {_paramSchemaDefault = Nothing, _paramSchemaType = SwaggerString, _paramSchemaFormat = Nothing, _paramSchemaItems = Nothing, _paramSchemaMaximum = Nothing, _paramSchemaExclusiveMaximum = Nothing, _paramSchemaMinimum = Nothing, _paramSchemaExclusiveMinimum = Nothing, _paramSchemaMaxLength = Nothing, _paramSchemaMinLength = Nothing, _paramSchemaPattern = Nothing, _paramSchemaMaxItems = Nothing, _paramSchemaMinItems = Nothing, _paramSchemaUniqueItems = Nothing, _paramSchemaEnum = Nothing, _paramSchemaMultipleOf = Nothing}})})], _operationResponses = Responses {_responsesDefault = Nothing, _responsesResponses = fromList [(400,Inline (Response {_responseDescription = "Invalid username/password supplied", _responseSchema = Nothing, _responseHeaders = fromList [], _responseExamples = Nothing})),(200,Inline (Response {_responseDescription = "successful operation", _responseSchema = Just (Inline (Schema {_schemaTitle = Nothing, _schemaDescription = Nothing, _schemaRequired = [], _schemaAllOf = Nothing, _schemaProperties = fromList [], _schemaAdditionalProperties = Nothing, _schemaDiscriminator = Nothing, _schemaReadOnly = Nothing, _schemaXml = Nothing, _schemaExternalDocs = Nothing, _schemaExample = Nothing, _schemaMaxProperties = Nothing, _schemaMinProperties = Nothing, _schemaParamSchema = ParamSchema {_paramSchemaDefault = Nothing, _paramSchemaType = SwaggerString, _paramSchemaFormat = Nothing, _paramSchemaItems = Nothing, _paramSchemaMaximum = Nothing, _paramSchemaExclusiveMaximum = Nothing, _paramSchemaMinimum = Nothing, _paramSchemaExclusiveMinimum = Nothing, _paramSchemaMaxLength = Nothing, _paramSchemaMinLength = Nothing, _paramSchemaPattern = Nothing, _paramSchemaMaxItems = Nothing, _paramSchemaMinItems = Nothing, _paramSchemaUniqueItems = Nothing, _paramSchemaEnum = Nothing, _paramSchemaMultipleOf = Nothing}})), _responseHeaders = fromList [("X-Rate-Limit",Header {_headerDescription = Just "calls per hour allowed by the user", _headerParamSchema = ParamSchema {_paramSchemaDefault = Nothing, _paramSchemaType = SwaggerInteger, _paramSchemaFormat = Just "int32", _paramSchemaItems = Nothing, _paramSchemaMaximum = Nothing, _paramSchemaExclusiveMaximum = Nothing, _paramSchemaMinimum = Nothing, _paramSchemaExclusiveMinimum = Nothing, _paramSchemaMaxLength = Nothing, _paramSchemaMinLength = Nothing, _paramSchemaPattern = Nothing, _paramSchemaMaxItems = Nothing, _paramSchemaMinItems = Nothing, _paramSchemaUniqueItems = Nothing, _paramSchemaEnum = Nothing, _paramSchemaMultipleOf = Nothing}})
  --   ,("X-Expires-After",Header {_headerDescription = Just "date in UTC when token expires", _headerParamSchema = ParamSchema {_paramSchemaDefault = Nothing, _paramSchemaType = SwaggerString, _paramSchemaFormat = Just "date-time", _paramSchemaItems = Nothing, _paramSchemaMaximum = Nothing, _paramSchemaExclusiveMaximum = Nothing, _paramSchemaMinimum = Nothing, _paramSchemaExclusiveMinimum = Nothing, _paramSchemaMaxLength = Nothing, _paramSchemaMinLength = Nothing, _paramSchemaPattern = Nothing, _paramSchemaMaxItems = Nothing, _paramSchemaMinItems = Nothing, _paramSchemaUniqueItems = Nothing, _paramSchemaEnum = Nothing, _paramSchemaMultipleOf = Nothing}})], _responseExamples = Nothing}))]}, _operationSchemes = Nothing, _operationDeprecated = Nothing, _operationSecurity = []}), _pathItemPut = Nothing, _pathItemPost = Nothing, _pathItemDelete = Nothing, _pathItemOptions = Nothing, _pathItemHead = Nothing, _pathItemPatch = Nothing, _pathItemParameters = []})
  --   ,("/user",PathItem {_pathItemGet = Nothing, _pathItemPut = Nothing, _pathItemPost = Just (Operation {_operationTags = fromList ["user"], _operationSummary = Just "Create user", _operationDescription = Just "This can only be done by the logged in user.", _operationExternalDocs = Nothing, _operationOperationId = Just "createUser", _operationConsumes = Nothing, _operationProduces = Just (MimeList {getMimeList = [application/xml,application/json]}), _operationParameters = [Inline (Param {_paramName = "body", _paramDescription = Just "Created user object", _paramRequired = Just True, _paramSchema = ParamBody (Ref (Reference {getReference = "User"}))})], _operationResponses = Responses {_responsesDefault = Just (Inline (Response {_responseDescription = "successful operation", _responseSchema = Nothing, _responseHeaders = fromList [], _responseExamples = Nothing})), _responsesResponses = fromList []}, _operationSchemes = Nothing, _operationDeprecated = Nothing, _operationSecurity = []}), _pathItemDelete = Nothing, _pathItemOptions = Nothing, _pathItemHead = Nothing, _pathItemPatch = Nothing, _pathItemParameters = []})
  --   ,("/pet/findByTags",PathItem {_pathItemGet = Just (Operation {_operationTags = fromList ["pet"], _operationSummary = Just "Finds Pets by tags", _operationDescription = Just "Muliple tags can be provided with comma separated strings. Use         tag1, tag2, tag3 for testing.", _operationExternalDocs = Nothing, _operationOperationId = Just "findPetsByTags", _operationConsumes = Nothing, _operationProduces = Just (MimeList {getMimeList = [application/xml,application/json]}), _operationParameters = [Inline (Param {_paramName = "tags", _paramDescription = Just "Tags to filter by", _paramRequired = Just True, _paramSchema = ParamOther (ParamOtherSchema {_paramOtherSchemaIn = ParamQuery, _paramOtherSchemaAllowEmptyValue = Nothing, _paramOtherSchemaParamSchema = ParamSchema {_paramSchemaDefault = Nothing, _paramSchemaType = SwaggerArray, _paramSchemaFormat = Nothing, _paramSchemaItems = Just (SwaggerItemsPrimitive (Just CollectionMulti) (ParamSchema {_paramSchemaDefault = Nothing, _paramSchemaType = SwaggerString, _paramSchemaFormat = Nothing, _paramSchemaItems = Nothing, _paramSchemaMaximum = Nothing, _paramSchemaExclusiveMaximum = Nothing, _paramSchemaMinimum = Nothing, _paramSchemaExclusiveMinimum = Nothing, _paramSchemaMaxLength = Nothing, _paramSchemaMinLength = Nothing, _paramSchemaPattern = Nothing, _paramSchemaMaxItems = Nothing, _paramSchemaMinItems = Nothing, _paramSchemaUniqueItems = Nothing, _paramSchemaEnum = Nothing, _paramSchemaMultipleOf = Nothing})), _paramSchemaMaximum = Nothing, _paramSchemaExclusiveMaximum = Nothing, _paramSchemaMinimum = Nothing, _paramSchemaExclusiveMinimum = Nothing, _paramSchemaMaxLength = Nothing, _paramSchemaMinLength = Nothing, _paramSchemaPattern = Nothing, _paramSchemaMaxItems = Nothing, _paramSchemaMinItems = Nothing, _paramSchemaUniqueItems = Nothing, _paramSchemaEnum = Nothing, _paramSchemaMultipleOf = Nothing}})})], _operationResponses = Responses {_responsesDefault = Nothing, _responsesResponses = fromList [(400,Inline (Response {_responseDescription = "Invalid tag value", _responseSchema = Nothing, _responseHeaders = fromList [], _responseExamples = Nothing})),(200,Inline (Response {_responseDescription = "successful operation", _responseSchema = Just (Inline (Schema {_schemaTitle = Nothing, _schemaDescription = Nothing, _schemaRequired = [], _schemaAllOf = Nothing, _schemaProperties = fromList [], _schemaAdditionalProperties = Nothing, _schemaDiscriminator = Nothing, _schemaReadOnly = Nothing, _schemaXml = Nothing, _schemaExternalDocs = Nothing, _schemaExample = Nothing, _schemaMaxProperties = Nothing, _schemaMinProperties = Nothing, _schemaParamSchema = ParamSchema {_paramSchemaDefault = Nothing, _paramSchemaType = SwaggerArray, _paramSchemaFormat = Nothing, _paramSchemaItems = Just (SwaggerItemsObject (Ref (Reference {getReference = "Pet"}))), _paramSchemaMaximum = Nothing, _paramSchemaExclusiveMaximum = Nothing, _paramSchemaMinimum = Nothing, _paramSchemaExclusiveMinimum = Nothing, _paramSchemaMaxLength = Nothing, _paramSchemaMinLength = Nothing, _paramSchemaPattern = Nothing, _paramSchemaMaxItems = Nothing, _paramSchemaMinItems = Nothing, _paramSchemaUniqueItems = Nothing, _paramSchemaEnum = Nothing, _paramSchemaMultipleOf = Nothing}})), _responseHeaders = fromList [], _responseExamples = Nothing}))]}, _operationSchemes = Nothing, _operationDeprecated = Just True, _operationSecurity = [SecurityRequirement {getSecurityRequirement = fromList [("petstore_auth",["write:pets","read:pets"])]}]}), _pathItemPut = Nothing, _pathItemPost = Nothing, _pathItemDelete = Nothing, _pathItemOptions = Nothing, _pathItemHead = Nothing, _pathItemPatch = Nothing, _pathItemParameters = []})
    
  --   ,("/store/order",PathItem {_pathItemGet = Nothing, _pathItemPut = Nothing, _pathItemPost = Just (Operation {_operationTags = fromList ["store"], _operationSummary = Just "Place an order for a pet", _operationDescription = Just "", _operationExternalDocs = Nothing, _operationOperationId = Just "placeOrder", _operationConsumes = Nothing, _operationProduces = Just (MimeList {getMimeList = [application/xml,application/json]}), _operationParameters = [Inline (Param {_paramName = "body", _paramDescription = Just "order placed for purchasing the pet", _paramRequired = Just True, _paramSchema = ParamBody (Ref (Reference {getReference = "Order"}))})], _operationResponses = Responses {_responsesDefault = Nothing, _responsesResponses = fromList [(400,Inline (Response {_responseDescription = "Invalid Order", _responseSchema = Nothing, _responseHeaders = fromList [], _responseExamples = Nothing})),(200,Inline (Response {_responseDescription = "successful operation", _responseSchema = Just (Ref (Reference {getReference = "Order"})), _responseHeaders = fromList [], _responseExamples = Nothing}))]}, _operationSchemes = Nothing, _operationDeprecated = Nothing, _operationSecurity = []}), _pathItemDelete = Nothing, _pathItemOptions = Nothing, _pathItemHead = Nothing, _pathItemPatch = Nothing, _pathItemParameters = []})
  --   ,("/pet/{petId}/uploadImage",PathItem {_pathItemGet = Nothing, _pathItemPut = Nothing, _pathItemPost = Just (Operation {_operationTags = fromList ["pet"], _operationSummary = Just "uploads an image", _operationDescription = Just "", _operationExternalDocs = Nothing, _operationOperationId = Just "uploadFile", _operationConsumes = Just (MimeList {getMimeList = [multipart/form-data]}), _operationProduces = Just (MimeList {getMimeList = [application/json]}), _operationParameters = [Inline (Param {_paramName = "petId", _paramDescription = Just "ID of pet to update", _paramRequired = Just True, _paramSchema = ParamOther (ParamOtherSchema {_paramOtherSchemaIn = ParamPath, _paramOtherSchemaAllowEmptyValue = Nothing, _paramOtherSchemaParamSchema = ParamSchema {_paramSchemaDefault = Nothing, _paramSchemaType = SwaggerInteger, _paramSchemaFormat = Just "int64", _paramSchemaItems = Nothing, _paramSchemaMaximum = Nothing, _paramSchemaExclusiveMaximum = Nothing, _paramSchemaMinimum = Nothing, _paramSchemaExclusiveMinimum = Nothing, _paramSchemaMaxLength = Nothing, _paramSchemaMinLength = Nothing, _paramSchemaPattern = Nothing, _paramSchemaMaxItems = Nothing, _paramSchemaMinItems = Nothing, _paramSchemaUniqueItems = Nothing, _paramSchemaEnum = Nothing, _paramSchemaMultipleOf = Nothing}})}),Inline (Param {_paramName = "additionalMetadata", _paramDescription = Just "Additional data to pass to server", _paramRequired = Just False, _paramSchema = ParamOther (ParamOtherSchema {_paramOtherSchemaIn = ParamFormData, _paramOtherSchemaAllowEmptyValue = Nothing, _paramOtherSchemaParamSchema = ParamSchema {_paramSchemaDefault = Nothing, _paramSchemaType = SwaggerString, _paramSchemaFormat = Nothing, _paramSchemaItems = Nothing, _paramSchemaMaximum = Nothing, _paramSchemaExclusiveMaximum = Nothing, _paramSchemaMinimum = Nothing, _paramSchemaExclusiveMinimum = Nothing, _paramSchemaMaxLength = Nothing, _paramSchemaMinLength = Nothing, _paramSchemaPattern = Nothing, _paramSchemaMaxItems = Nothing, _paramSchemaMinItems = Nothing, _paramSchemaUniqueItems = Nothing, _paramSchemaEnum = Nothing, _paramSchemaMultipleOf = Nothing}})}),Inline (Param {_paramName = "file", _paramDescription = Just "file to upload", _paramRequired = Just False, _paramSchema = ParamOther (ParamOtherSchema {_paramOtherSchemaIn = ParamFormData, _paramOtherSchemaAllowEmptyValue = Nothing, _paramOtherSchemaParamSchema = ParamSchema {_paramSchemaDefault = Nothing, _paramSchemaType = SwaggerFile, _paramSchemaFormat = Nothing, _paramSchemaItems = Nothing, _paramSchemaMaximum = Nothing, _paramSchemaExclusiveMaximum = Nothing, _paramSchemaMinimum = Nothing, _paramSchemaExclusiveMinimum = Nothing, _paramSchemaMaxLength = Nothing, _paramSchemaMinLength = Nothing, _paramSchemaPattern = Nothing, _paramSchemaMaxItems = Nothing, _paramSchemaMinItems = Nothing, _paramSchemaUniqueItems = Nothing, _paramSchemaEnum = Nothing, _paramSchemaMultipleOf = Nothing}})})], _operationResponses = Responses {_responsesDefault = Nothing, _responsesResponses = fromList [(200,Inline (Response {_responseDescription = "successful operation", _responseSchema = Just (Ref (Reference {getReference = "ApiResponse"})), _responseHeaders = fromList [], _responseExamples = Nothing}))]}, _operationSchemes = Nothing, _operationDeprecated = Nothing, _operationSecurity = [SecurityRequirement {getSecurityRequirement = fromList [("petstore_auth",["write:pets","read:pets"])]}]}), _pathItemDelete = Nothing, _pathItemOptions = Nothing, _pathItemHead = Nothing, _pathItemPatch = Nothing, _pathItemParameters = []})
  --   ,("/store/order/{orderId}",PathItem {_pathItemGet = Just (Operation {_operationTags = fromList ["store"], _operationSummary = Just "Find purchase order by ID", _operationDescription = Just "For valid response try integer IDs with value >= 1 and <= 10.         Other values will generated exceptions", _operationExternalDocs = Nothing, _operationOperationId = Just "getOrderById", _operationConsumes = Nothing, _operationProduces = Just (MimeList {getMimeList = [application/xml,application/json]}), _operationParameters = [Inline (Param {_paramName = "orderId", _paramDescription = Just "ID of pet that needs to be fetched", _paramRequired = Just True, _paramSchema = ParamOther (ParamOtherSchema {_paramOtherSchemaIn = ParamPath, _paramOtherSchemaAllowEmptyValue = Nothing, _paramOtherSchemaParamSchema = ParamSchema {_paramSchemaDefault = Nothing, _paramSchemaType = SwaggerInteger, _paramSchemaFormat = Just "int64", _paramSchemaItems = Nothing, _paramSchemaMaximum = Just 10.0, _paramSchemaExclusiveMaximum = Nothing, _paramSchemaMinimum = Just 1.0, _paramSchemaExclusiveMinimum = Nothing, _paramSchemaMaxLength = Nothing, _paramSchemaMinLength = Nothing, _paramSchemaPattern = Nothing, _paramSchemaMaxItems = Nothing, _paramSchemaMinItems = Nothing, _paramSchemaUniqueItems = Nothing, _paramSchemaEnum = Nothing, _paramSchemaMultipleOf = Nothing}})})], _operationResponses = Responses {_responsesDefault = Nothing, _responsesResponses = fromList [(400,Inline (Response {_responseDescription = "Invalid ID supplied", _responseSchema = Nothing, _responseHeaders = fromList [], _responseExamples = Nothing})),(404,Inline (Response {_responseDescription = "Order not found", _responseSchema = Nothing, _responseHeaders = fromList [], _responseExamples = Nothing})),(200,Inline (Response {_responseDescription = "successful operation", _responseSchema = Just (Ref (Reference {getReference = "Order"})), _responseHeaders = fromList [], _responseExamples = Nothing}))]}, _operationSchemes = Nothing, _operationDeprecated = Nothing, _operationSecurity = []}), _pathItemPut = Nothing, _pathItemPost = Nothing, _pathItemDelete = Just (Operation {_operationTags = fromList ["store"], _operationSummary = Just "Delete purchase order by ID", _operationDescription = Just "For valid response try integer IDs with positive integer value.         Negative or non-integer values will generate API errors", _operationExternalDocs = Nothing, _operationOperationId = Just "deleteOrder", _operationConsumes = Nothing, _operationProduces = Just (MimeList {getMimeList = [application/xml,application/json]}), _operationParameters = [Inline (Param {_paramName = "orderId", _paramDescription = Just "ID of the order that needs to be deleted", _paramRequired = Just True, _paramSchema = ParamOther (ParamOtherSchema {_paramOtherSchemaIn = ParamPath, _paramOtherSchemaAllowEmptyValue = Nothing, _paramOtherSchemaParamSchema = ParamSchema {_paramSchemaDefault = Nothing, _paramSchemaType = SwaggerInteger, _paramSchemaFormat = Just "int64", _paramSchemaItems = Nothing, _paramSchemaMaximum = Nothing, _paramSchemaExclusiveMaximum = Nothing, _paramSchemaMinimum = Just 1.0, _paramSchemaExclusiveMinimum = Nothing, _paramSchemaMaxLength = Nothing, _paramSchemaMinLength = Nothing, _paramSchemaPattern = Nothing, _paramSchemaMaxItems = Nothing, _paramSchemaMinItems = Nothing, _paramSchemaUniqueItems = Nothing, _paramSchemaEnum = Nothing, _paramSchemaMultipleOf = Nothing}})})], _operationResponses = Responses {_responsesDefault = Nothing, _responsesResponses = fromList [(400,Inline (Response {_responseDescription = "Invalid ID supplied", _responseSchema = Nothing, _responseHeaders = fromList [], _responseExamples = Nothing})),(404,Inline (Response {_responseDescription = "Order not found", _responseSchema = Nothing, _responseHeaders = fromList [], _responseExamples = Nothing}))]}, _operationSchemes = Nothing, _operationDeprecated = Nothing, _operationSecurity = []}), _pathItemOptions = Nothing, _pathItemHead = Nothing, _pathItemPatch = Nothing, _pathItemParameters = []})
  --   ,("/store/inventory",PathItem {_pathItemGet = Just (Operation {_operationTags = fromList ["store"], _operationSummary = Just "Returns pet inventories by status", _operationDescription = Just "Returns a map of status codes to quantities", _operationExternalDocs = Nothing, _operationOperationId = Just "getInventory", _operationConsumes = Nothing, _operationProduces = Just (MimeList {getMimeList = [application/json]}), _operationParameters = [], _operationResponses = Responses {_responsesDefault = Nothing, _responsesResponses = fromList [(200,Inline (Response {_responseDescription = "successful operation", _responseSchema = Just (Inline (Schema {_schemaTitle = Nothing, _schemaDescription = Nothing, _schemaRequired = [], _schemaAllOf = Nothing, _schemaProperties = fromList [], _schemaAdditionalProperties = Just (Inline (Schema {_schemaTitle = Nothing, _schemaDescription = Nothing, _schemaRequired = [], _schemaAllOf = Nothing, _schemaProperties = fromList [], _schemaAdditionalProperties = Nothing, _schemaDiscriminator = Nothing, _schemaReadOnly = Nothing, _schemaXml = Nothing, _schemaExternalDocs = Nothing, _schemaExample = Nothing, _schemaMaxProperties = Nothing, _schemaMinProperties = Nothing, _schemaParamSchema = ParamSchema {_paramSchemaDefault = Nothing, _paramSchemaType = SwaggerInteger, _paramSchemaFormat = Just "int32", _paramSchemaItems = Nothing, _paramSchemaMaximum = Nothing, _paramSchemaExclusiveMaximum = Nothing, _paramSchemaMinimum = Nothing, _paramSchemaExclusiveMinimum = Nothing, _paramSchemaMaxLength = Nothing, _paramSchemaMinLength = Nothing, _paramSchemaPattern = Nothing, _paramSchemaMaxItems = Nothing, _paramSchemaMinItems = Nothing, _paramSchemaUniqueItems = Nothing, _paramSchemaEnum = Nothing, _paramSchemaMultipleOf = Nothing}})), _schemaDiscriminator = Nothing, _schemaReadOnly = Nothing, _schemaXml = Nothing, _schemaExternalDocs = Nothing, _schemaExample = Nothing, _schemaMaxProperties = Nothing, _schemaMinProperties = Nothing, _schemaParamSchema = ParamSchema {_paramSchemaDefault = Nothing, _paramSchemaType = SwaggerObject, _paramSchemaFormat = Nothing, _paramSchemaItems = Nothing, _paramSchemaMaximum = Nothing, _paramSchemaExclusiveMaximum = Nothing, _paramSchemaMinimum = Nothing, _paramSchemaExclusiveMinimum = Nothing, _paramSchemaMaxLength = Nothing, _paramSchemaMinLength = Nothing, _paramSchemaPattern = Nothing, _paramSchemaMaxItems = Nothing, _paramSchemaMinItems = Nothing, _paramSchemaUniqueItems = Nothing, _paramSchemaEnum = Nothing, _paramSchemaMultipleOf = Nothing}})), _responseHeaders = fromList [], _responseExamples = Nothing}))]}, _operationSchemes = Nothing, _operationDeprecated = Nothing, _operationSecurity = [SecurityRequirement {getSecurityRequirement = fromList [("api_key",[])]}]}), _pathItemPut = Nothing, _pathItemPost = Nothing, _pathItemDelete = Nothing, _pathItemOptions = Nothing, _pathItemHead = Nothing, _pathItemPatch = Nothing, _pathItemParameters = []})
  --   ,("/pet/{petId}",PathItem {_pathItemGet = Just (Operation {_operationTags = fromList ["pet"], _operationSummary = Just "Find pet by ID", _operationDescription = Just "Returns a single pet", _operationExternalDocs = Nothing, _operationOperationId = Just "getPetById", _operationConsumes = Nothing, _operationProduces = Just (MimeList {getMimeList = [application/xml,application/json]}), _operationParameters = [Inline (Param {_paramName = "petId", _paramDescription = Just "ID of pet to return", _paramRequired = Just True, _paramSchema = ParamOther (ParamOtherSchema {_paramOtherSchemaIn = ParamPath, _paramOtherSchemaAllowEmptyValue = Nothing, _paramOtherSchemaParamSchema = ParamSchema {_paramSchemaDefault = Nothing, _paramSchemaType = SwaggerInteger, _paramSchemaFormat = Just "int64", _paramSchemaItems = Nothing, _paramSchemaMaximum = Nothing, _paramSchemaExclusiveMaximum = Nothing, _paramSchemaMinimum = Nothing, _paramSchemaExclusiveMinimum = Nothing, _paramSchemaMaxLength = Nothing, _paramSchemaMinLength = Nothing, _paramSchemaPattern = Nothing, _paramSchemaMaxItems = Nothing, _paramSchemaMinItems = Nothing, _paramSchemaUniqueItems = Nothing, _paramSchemaEnum = Nothing, _paramSchemaMultipleOf = Nothing}})})], _operationResponses = Responses {_responsesDefault = Nothing, _responsesResponses = fromList [(400,Inline (Response {_responseDescription = "Invalid ID supplied", _responseSchema = Nothing, _responseHeaders = fromList [], _responseExamples = Nothing})),(404,Inline (Response {_responseDescription = "Pet not found", _responseSchema = Nothing, _responseHeaders = fromList [], _responseExamples = Nothing})),(200,Inline (Response {_responseDescription = "successful operation", _responseSchema = Just (Ref (Reference {getReference = "Pet"})), _responseHeaders = fromList [], _responseExamples = Nothing}))]}, _operationSchemes = Nothing, _operationDeprecated = Nothing, _operationSecurity = [SecurityRequirement {getSecurityRequirement = fromList [("api_key",[])]}]}), _pathItemPut = Nothing, _pathItemPost = Just (Operation {_operationTags = fromList ["pet"], _operationSummary = Just "Updates a pet in the store with form data", _operationDescription = Just "", _operationExternalDocs = Nothing, _operationOperationId = Just "updatePetWithForm", _operationConsumes = Just (MimeList {getMimeList = [application/x-www-form-urlencoded]}), _operationProduces = Just (MimeList {getMimeList = [application/xml,application/json]}), _operationParameters = [Inline (Param {_paramName = "petId", _paramDescription = Just "ID of pet that needs to be updated", _paramRequired = Just True, _paramSchema = ParamOther (ParamOtherSchema {_paramOtherSchemaIn = ParamPath, _paramOtherSchemaAllowEmptyValue = Nothing, _paramOtherSchemaParamSchema = ParamSchema {_paramSchemaDefault = Nothing, _paramSchemaType = SwaggerInteger, _paramSchemaFormat = Just "int64", _paramSchemaItems = Nothing, _paramSchemaMaximum = Nothing, _paramSchemaExclusiveMaximum = Nothing, _paramSchemaMinimum = Nothing, _paramSchemaExclusiveMinimum = Nothing, _paramSchemaMaxLength = Nothing, _paramSchemaMinLength = Nothing, _paramSchemaPattern = Nothing, _paramSchemaMaxItems = Nothing, _paramSchemaMinItems = Nothing, _paramSchemaUniqueItems = Nothing, _paramSchemaEnum = Nothing, _paramSchemaMultipleOf = Nothing}})}),Inline (Param {_paramName = "name", _paramDescription = Just "Updated name of the pet", _paramRequired = Just False, _paramSchema = ParamOther (ParamOtherSchema {_paramOtherSchemaIn = ParamFormData, _paramOtherSchemaAllowEmptyValue = Nothing, _paramOtherSchemaParamSchema = ParamSchema {_paramSchemaDefault = Nothing, _paramSchemaType = SwaggerString, _paramSchemaFormat = Nothing, _paramSchemaItems = Nothing, _paramSchemaMaximum = Nothing, _paramSchemaExclusiveMaximum = Nothing, _paramSchemaMinimum = Nothing, _paramSchemaExclusiveMinimum = Nothing, _paramSchemaMaxLength = Nothing, _paramSchemaMinLength = Nothing, _paramSchemaPattern = Nothing, _paramSchemaMaxItems = Nothing, _paramSchemaMinItems = Nothing, _paramSchemaUniqueItems = Nothing, _paramSchemaEnum = Nothing, _paramSchemaMultipleOf = Nothing}})}),Inline (Param {_paramName = "status", _paramDescription = Just "Updated status of the pet", _paramRequired = Just False, _paramSchema = ParamOther (ParamOtherSchema {_paramOtherSchemaIn = ParamFormData, _paramOtherSchemaAllowEmptyValue = Nothing, _paramOtherSchemaParamSchema = ParamSchema {_paramSchemaDefault = Nothing, _paramSchemaType = SwaggerString, _paramSchemaFormat = Nothing, _paramSchemaItems = Nothing, _paramSchemaMaximum = Nothing, _paramSchemaExclusiveMaximum = Nothing, _paramSchemaMinimum = Nothing, _paramSchemaExclusiveMinimum = Nothing, _paramSchemaMaxLength = Nothing, _paramSchemaMinLength = Nothing, _paramSchemaPattern = Nothing, _paramSchemaMaxItems = Nothing, _paramSchemaMinItems = Nothing, _paramSchemaUniqueItems = Nothing, _paramSchemaEnum = Nothing, _paramSchemaMultipleOf = Nothing}})})], _operationResponses = Responses {_responsesDefault = Nothing, _responsesResponses = fromList [(405,Inline (Response {_responseDescription = "Invalid input", _responseSchema = Nothing, _responseHeaders = fromList [], _responseExamples = Nothing}))]}, _operationSchemes = Nothing, _operationDeprecated = Nothing, _operationSecurity = [SecurityRequirement {getSecurityRequirement = fromList [("petstore_auth",["write:pets","read:pets"])]}]}), _pathItemDelete = Just (Operation {_operationTags = fromList ["pet"], _operationSummary = Just "Deletes a pet", _operationDescription = Just "", _operationExternalDocs = Nothing, _operationOperationId = Just "deletePet", _operationConsumes = Nothing, _operationProduces = Just (MimeList {getMimeList = [application/xml,application/json]}), _operationParameters = [Inline (Param {_paramName = "api_key", _paramDescription = Nothing, _paramRequired = Just False, _paramSchema = ParamOther (ParamOtherSchema {_paramOtherSchemaIn = ParamHeader, _paramOtherSchemaAllowEmptyValue = Nothing, _paramOtherSchemaParamSchema = ParamSchema {_paramSchemaDefault = Nothing, _paramSchemaType = SwaggerString, _paramSchemaFormat = Nothing, _paramSchemaItems = Nothing, _paramSchemaMaximum = Nothing, _paramSchemaExclusiveMaximum = Nothing, _paramSchemaMinimum = Nothing, _paramSchemaExclusiveMinimum = Nothing, _paramSchemaMaxLength = Nothing, _paramSchemaMinLength = Nothing, _paramSchemaPattern = Nothing, _paramSchemaMaxItems = Nothing, _paramSchemaMinItems = Nothing, _paramSchemaUniqueItems = Nothing, _paramSchemaEnum = Nothing, _paramSchemaMultipleOf = Nothing}})}),Inline (Param {_paramName = "petId", _paramDescription = Just "Pet id to delete", _paramRequired = Just True, _paramSchema = ParamOther (ParamOtherSchema {_paramOtherSchemaIn = ParamPath, _paramOtherSchemaAllowEmptyValue = Nothing, _paramOtherSchemaParamSchema = ParamSchema {_paramSchemaDefault = Nothing, _paramSchemaType = SwaggerInteger, _paramSchemaFormat = Just "int64", _paramSchemaItems = Nothing, _paramSchemaMaximum = Nothing, _paramSchemaExclusiveMaximum = Nothing, _paramSchemaMinimum = Nothing, _paramSchemaExclusiveMinimum = Nothing, _paramSchemaMaxLength = Nothing, _paramSchemaMinLength = Nothing, _paramSchemaPattern = Nothing, _paramSchemaMaxItems = Nothing, _paramSchemaMinItems = Nothing, _paramSchemaUniqueItems = Nothing, _paramSchemaEnum = Nothing, _paramSchemaMultipleOf = Nothing}})})], _operationResponses = Responses {_responsesDefault = Nothing, _responsesResponses = fromList [(400,Inline (Response {_responseDescription = "Invalid ID supplied", _responseSchema = Nothing, _responseHeaders = fromList [], _responseExamples = Nothing})),(404,Inline (Response {_responseDescription = "Pet not found", _responseSchema = Nothing, _responseHeaders = fromList [], _responseExamples = Nothing}))]}, _operationSchemes = Nothing, _operationDeprecated = Nothing, _operationSecurity = [SecurityRequirement {getSecurityRequirement = fromList [("petstore_auth",["write:pets","read:pets"])]}]}), _pathItemOptions = Nothing, _pathItemHead = Nothing, _pathItemPatch = Nothing, _pathItemParameters = []})
  --   ,("/user/createWithArray",PathItem {_pathItemGet = Nothing, _pathItemPut = Nothing, _pathItemPost = Just (Operation {_operationTags = fromList ["user"], _operationSummary = Just "Creates list of users with given input array", _operationDescription = Just "", _operationExternalDocs = Nothing, _operationOperationId = Just "createUsersWithArrayInput", _operationConsumes = Nothing, _operationProduces = Just (MimeList {getMimeList = [application/xml,application/json]}), _operationParameters = [Inline (Param {_paramName = "body", _paramDescription = Just "List of user object", _paramRequired = Just True, _paramSchema = ParamBody (Inline (Schema {_schemaTitle = Nothing, _schemaDescription = Nothing, _schemaRequired = [], _schemaAllOf = Nothing, _schemaProperties = fromList [], _schemaAdditionalProperties = Nothing, _schemaDiscriminator = Nothing, _schemaReadOnly = Nothing, _schemaXml = Nothing, _schemaExternalDocs = Nothing, _schemaExample = Nothing, _schemaMaxProperties = Nothing, _schemaMinProperties = Nothing, _schemaParamSchema = ParamSchema {_paramSchemaDefault = Nothing, _paramSchemaType = SwaggerArray, _paramSchemaFormat = Nothing, _paramSchemaItems = Just (SwaggerItemsObject (Ref (Reference {getReference = "User"}))), _paramSchemaMaximum = Nothing, _paramSchemaExclusiveMaximum = Nothing, _paramSchemaMinimum = Nothing, _paramSchemaExclusiveMinimum = Nothing, _paramSchemaMaxLength = Nothing, _paramSchemaMinLength = Nothing, _paramSchemaPattern = Nothing, _paramSchemaMaxItems = Nothing, _paramSchemaMinItems = Nothing, _paramSchemaUniqueItems = Nothing, _paramSchemaEnum = Nothing, _paramSchemaMultipleOf = Nothing}}))})], _operationResponses = Responses {_responsesDefault = Just (Inline (Response {_responseDescription = "successful operation", _responseSchema = Nothing, _responseHeaders = fromList [], _responseExamples = Nothing})), _responsesResponses = fromList []}, _operationSchemes = Nothing, _operationDeprecated = Nothing, _operationSecurity = []}), _pathItemDelete = Nothing, _pathItemOptions = Nothing, _pathItemHead = Nothing, _pathItemPatch = Nothing, _pathItemParameters = []})
  --   ,("/pet",PathItem {_pathItemGet = Nothing, _pathItemPut = Just (Operation {_operationTags = fromList ["pet"], _operationSummary = Just "Update an existing pet", _operationDescription = Just "", _operationExternalDocs = Nothing, _operationOperationId = Just "updatePet", _operationConsumes = Just (MimeList {getMimeList = [application/json,application/xml]}), _operationProduces = Just (MimeList {getMimeList = [application/xml,application/json]}), _operationParameters = [Inline (Param {_paramName = "body", _paramDescription = Just "Pet object that needs to be added to the store", _paramRequired = Just True, _paramSchema = ParamBody (Ref (Reference {getReference = "Pet"}))})], _operationResponses = Responses {_responsesDefault = Nothing, _responsesResponses = fromList [(400,Inline (Response {_responseDescription = "Invalid ID supplied", _responseSchema = Nothing, _responseHeaders = fromList [], _responseExamples = Nothing})),(404,Inline (Response {_responseDescription = "Pet not found", _responseSchema = Nothing, _responseHeaders = fromList [], _responseExamples = Nothing})),(405,Inline (Response {_responseDescription = "Validation exception", _responseSchema = Nothing, _responseHeaders = fromList [], _responseExamples = Nothing}))]}, _operationSchemes = Nothing, _operationDeprecated = Nothing, _operationSecurity = [SecurityRequirement {getSecurityRequirement = fromList [("petstore_auth",["write:pets","read:pets"])]}]}), _pathItemPost = Just (Operation {_operationTags = fromList ["pet"], _operationSummary = Just "Add a new pet to the store", _operationDescription = Just "", _operationExternalDocs = Nothing, _operationOperationId = Just "addPet", _operationConsumes = Just (MimeList {getMimeList = [application/json,application/xml]}), _operationProduces = Just (MimeList {getMimeList = [application/xml,application/json]}), _operationParameters = [Inline (Param {_paramName = "body", _paramDescription = Just "Pet object that needs to be added to the store", _paramRequired = Just True, _paramSchema = ParamBody (Ref (Reference {getReference = "Pet"}))})], _operationResponses = Responses {_responsesDefault = Nothing, _responsesResponses = fromList [(405,Inline (Response {_responseDescription = "Invalid input", _responseSchema = Nothing, _responseHeaders = fromList [], _responseExamples = Nothing}))]}, _operationSchemes = Nothing, _operationDeprecated = Nothing, _operationSecurity = [SecurityRequirement {getSecurityRequirement = fromList [("petstore_auth",["write:pets","read:pets"])]}]}), _pathItemDelete = Nothing, _pathItemOptions = Nothing, _pathItemHead = Nothing, _pathItemPatch = Nothing, _pathItemParameters = []})
  --   ,("/user/createWithList",PathItem {_pathItemGet = Nothing, _pathItemPut = Nothing, _pathItemPost = Just (Operation {_operationTags = fromList ["user"], _operationSummary = Just "Creates list of users with given input array", _operationDescription = Just "", _operationExternalDocs = Nothing, _operationOperationId = Just "createUsersWithListInput", _operationConsumes = Nothing, _operationProduces = Just (MimeList {getMimeList = [application/xml,application/json]}), _operationParameters = [Inline (Param {_paramName = "body", _paramDescription = Just "List of user object", _paramRequired = Just True, _paramSchema = ParamBody (Inline (Schema {_schemaTitle = Nothing, _schemaDescription = Nothing, _schemaRequired = [], _schemaAllOf = Nothing, _schemaProperties = fromList [], _schemaAdditionalProperties = Nothing, _schemaDiscriminator = Nothing, _schemaReadOnly = Nothing, _schemaXml = Nothing, _schemaExternalDocs = Nothing, _schemaExample = Nothing, _schemaMaxProperties = Nothing, _schemaMinProperties = Nothing, _schemaParamSchema = ParamSchema {_paramSchemaDefault = Nothing, _paramSchemaType = SwaggerArray, _paramSchemaFormat = Nothing, _paramSchemaItems = Just (SwaggerItemsObject (Ref (Reference {getReference = "User"}))), _paramSchemaMaximum = Nothing, _paramSchemaExclusiveMaximum = Nothing, _paramSchemaMinimum = Nothing, _paramSchemaExclusiveMinimum = Nothing, _paramSchemaMaxLength = Nothing, _paramSchemaMinLength = Nothing, _paramSchemaPattern = Nothing, _paramSchemaMaxItems = Nothing, _paramSchemaMinItems = Nothing, _paramSchemaUniqueItems = Nothing, _paramSchemaEnum = Nothing, _paramSchemaMultipleOf = Nothing}}))})], _operationResponses = Responses {_responsesDefault = Just (Inline (Response {_responseDescription = "successful operation", _responseSchema = Nothing, _responseHeaders = fromList [], _responseExamples = Nothing})), _responsesResponses = fromList []}, _operationSchemes = Nothing, _operationDeprecated = Nothing, _operationSecurity = []}), _pathItemDelete = Nothing, _pathItemOptions = Nothing, _pathItemHead = Nothing, _pathItemPatch = Nothing, _pathItemParameters = []})
  --   ,("/user/{username}",PathItem {_pathItemGet = Just (Operation {_operationTags = fromList ["user"], _operationSummary = Just "Get user by user name", _operationDescription = Just "", _operationExternalDocs = Nothing, _operationOperationId = Just "getUserByName", _operationConsumes = Nothing, _operationProduces = Just (MimeList {getMimeList = [application/xml,application/json]}), _operationParameters = [Inline (Param {_paramName = "username", _paramDescription = Just "The name that needs to be fetched. Use user1 for testing. ", _paramRequired = Just True, _paramSchema = ParamOther (ParamOtherSchema {_paramOtherSchemaIn = ParamPath, _paramOtherSchemaAllowEmptyValue = Nothing, _paramOtherSchemaParamSchema = ParamSchema {_paramSchemaDefault = Nothing, _paramSchemaType = SwaggerString, _paramSchemaFormat = Nothing, _paramSchemaItems = Nothing, _paramSchemaMaximum = Nothing, _paramSchemaExclusiveMaximum = Nothing, _paramSchemaMinimum = Nothing, _paramSchemaExclusiveMinimum = Nothing, _paramSchemaMaxLength = Nothing, _paramSchemaMinLength = Nothing, _paramSchemaPattern = Nothing, _paramSchemaMaxItems = Nothing, _paramSchemaMinItems = Nothing, _paramSchemaUniqueItems = Nothing, _paramSchemaEnum = Nothing, _paramSchemaMultipleOf = Nothing}})})], _operationResponses = Responses {_responsesDefault = Nothing, _responsesResponses = fromList [(400,Inline (Response {_responseDescription = "Invalid username supplied", _responseSchema = Nothing, _responseHeaders = fromList [], _responseExamples = Nothing})),(404,Inline (Response {_responseDescription = "User not found", _responseSchema = Nothing, _responseHeaders = fromList [], _responseExamples = Nothing})),(200,Inline (Response {_responseDescription = "successful operation", _responseSchema = Just (Ref (Reference {getReference = "User"})), _responseHeaders = fromList [], _responseExamples = Nothing}))]}, _operationSchemes = Nothing, _operationDeprecated = Nothing, _operationSecurity = []}), _pathItemPut = Just (Operation {_operationTags = fromList ["user"], _operationSummary = Just "Updated user", _operationDescription = Just "This can only be done by the logged in user.", _operationExternalDocs = Nothing, _operationOperationId = Just "updateUser", _operationConsumes = Nothing, _operationProduces = Just (MimeList {getMimeList = [application/xml,application/json]}), _operationParameters = [Inline (Param {_paramName = "username", _paramDescription = Just "name that need to be updated", _paramRequired = Just True, _paramSchema = ParamOther (ParamOtherSchema {_paramOtherSchemaIn = ParamPath, _paramOtherSchemaAllowEmptyValue = Nothing, _paramOtherSchemaParamSchema = ParamSchema {_paramSchemaDefault = Nothing, _paramSchemaType = SwaggerString, _paramSchemaFormat = Nothing, _paramSchemaItems = Nothing, _paramSchemaMaximum = Nothing, _paramSchemaExclusiveMaximum = Nothing, _paramSchemaMinimum = Nothing, _paramSchemaExclusiveMinimum = Nothing, _paramSchemaMaxLength = Nothing, _paramSchemaMinLength = Nothing, _paramSchemaPattern = Nothing, _paramSchemaMaxItems = Nothing, _paramSchemaMinItems = Nothing, _paramSchemaUniqueItems = Nothing, _paramSchemaEnum = Nothing, _paramSchemaMultipleOf = Nothing}})}),Inline (Param {_paramName = "body", _paramDescription = Just "Updated user object", _paramRequired = Just True, _paramSchema = ParamBody (Ref (Reference {getReference = "User"}))})], _operationResponses = Responses {_responsesDefault = Nothing, _responsesResponses = fromList [(400,Inline (Response {_responseDescription = "Invalid user supplied", _responseSchema = Nothing, _responseHeaders = fromList [], _responseExamples = Nothing})),(404,Inline (Response {_responseDescription = "User not found", _responseSchema = Nothing, _responseHeaders = fromList [], _responseExamples = Nothing}))]}, _operationSchemes = Nothing, _operationDeprecated = Nothing, _operationSecurity = []}), _pathItemPost = Nothing, _pathItemDelete = Just (Operation {_operationTags = fromList ["user"], _operationSummary = Just "Delete user", _operationDescription = Just "This can only be done by the logged in user.", _operationExternalDocs = Nothing, _operationOperationId = Just "deleteUser", _operationConsumes = Nothing, _operationProduces = Just (MimeList {getMimeList = [application/xml,application/json]}), _operationParameters = [Inline (Param {_paramName = "username", _paramDescription = Just "The name that needs to be deleted", _paramRequired = Just True, _paramSchema = ParamOther (ParamOtherSchema {_paramOtherSchemaIn = ParamPath, _paramOtherSchemaAllowEmptyValue = Nothing, _paramOtherSchemaParamSchema = ParamSchema {_paramSchemaDefault = Nothing, _paramSchemaType = SwaggerString, _paramSchemaFormat = Nothing, _paramSchemaItems = Nothing, _paramSchemaMaximum = Nothing, _paramSchemaExclusiveMaximum = Nothing, _paramSchemaMinimum = Nothing, _paramSchemaExclusiveMinimum = Nothing, _paramSchemaMaxLength = Nothing, _paramSchemaMinLength = Nothing, _paramSchemaPattern = Nothing, _paramSchemaMaxItems = Nothing, _paramSchemaMinItems = Nothing, _paramSchemaUniqueItems = Nothing, _paramSchemaEnum = Nothing, _paramSchemaMultipleOf = Nothing}})})], _operationResponses = Responses {_responsesDefault = Nothing, _responsesResponses = fromList [(400,Inline (Response {_responseDescription = "Invalid username supplied", _responseSchema = Nothing, _responseHeaders = fromList [], _responseExamples = Nothing})),(404,Inline (Response {_responseDescription = "User not found", _responseSchema = Nothing, _responseHeaders = fromList [], _responseExamples = Nothing}))]}, _operationSchemes = Nothing, _operationDeprecated = Nothing, _operationSecurity = []}), _pathItemOptions = Nothing, _pathItemHead = Nothing, _pathItemPatch = Nothing, _pathItemParameters = []})], 

parseHaskellSrcContract :: IO ()
parseHaskellSrcContract = do
  parseResult <- parseFile "webapi-swagger/sampleFiles/contract.hs"
  case parseResult of
    ParseOk hModule -> 
      case hModule of
        Module srcLoc (Just _) langPragmas imports declarations -> putStrLn $ show declarations
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
  fromMaybeSV = fromJustNote "Expected a list with 4 elements for WebApi instance! "

fromParamVec :: Vector 3 String
fromParamVec = fromJustNote "Expected a list with 3 elements for WebApi instance!" $ SV.fromList ["FromParam", "FormParam", "EdiStr"]

generateContractBody :: String -> [ContractDetails] -> [Decl SrcSpanInfo]
generateContractBody contractName contractDetails = 
  [emptyDataDeclaration contractName] ++ flip fmap contractDetails (\cDetail -> routeDeclaration (routeName cDetail) (routePath cDetail) ) ++ 
    [webApiInstance contractName (fmap (\ctDetail -> (routeName ctDetail , fmap show (Map.keys $ methodData ctDetail))) contractDetails ) ] ++
    (fmap (\(topVec, innerVecList) -> apiInstanceDeclaration topVec innerVecList ) $ DL.concat $ fmap (constructVectorForRoute contractName) contractDetails)
    
    
 where
  constructVectorForRoute :: String -> ContractDetails -> [(Vector 4 String, [Vector 4 String])] 
  constructVectorForRoute contractName ctrDetails = 
    let currentRouteName = routeName ctrDetails
    in Map.foldlWithKey' (routeDetailToVector contractName currentRouteName) [] (methodData ctrDetails)
  routeDetailToVector :: String -> String -> [(Vector 4 String, [Vector 4 String])] -> StdMethod -> ApiTypeDetails -> [(Vector 4 String, [Vector 4 String])]
  routeDetailToVector contractName routeName accValue currentMethod apiDetails = 
    let topLevelVector = fromMaybeSV $ SV.fromList ["ApiContract", contractName, (show currentMethod), routeName]
        respType = Just $ apiOut apiDetails
        errType = apiErr apiDetails
        formParamType = formParam apiDetails
        queryParamType = queryParam apiDetails
        fileParamType = fileParam apiDetails
        headerParamType = headerIn apiDetails
        instanceVectorList = catMaybes $ fmap (\(typeInfo, typeLabel) -> fmap (\tInfo -> fromMaybeSV $ SV.fromList [typeLabel, show currentMethod, routeName, tInfo] ) typeInfo) $ DL.zip (respType:errType:formParamType:queryParamType:fileParamType:headerParamType:[]) ["ApiOut", "ApiErr","FormParam", "QueryParam", "FileParam", "HeaderIn"]
    in (topLevelVector, instanceVectorList):accValue
  fromMaybeSV = fromJustNote "Expected a list with 4 elements for WebApi instance! "


-- printHaskellModule :: IO()
-- printHaskellModule = 
--   let hModule = Module noSrcSpan (Just $ ModuleHead noSrcSpan (ModuleName noSrcSpan "Contract") Nothing Nothing)
--         (fmap languageExtension ["TypeFamilies", "MultiParamTypeClasses", "DeriveGeneric", "TypeOperators", "DataKinds", "TypeSynonymInstances", "FlexibleInstances"])
--         (fmap (moduleImport (False, "")) [ "WebApi",  "Data.Aeson",  "Data.ByteString",  "Data.Text as T",  "GHC.Generics"])
--         [
--         emptyDataDeclaration "EDITranslatorApi",
--         dataDeclaration (NewType noSrcSpan) "EdiStr" [("ediStr", "ByteString")] ["Show", "Generic"],
--         dataDeclaration (NewType noSrcSpan) "CharacterSet" [("characterSet", "ByteString")] ["Show", "Generic"],
--         dataDeclaration (DataType noSrcSpan) "EdiJsonWithDelims" [("ediJson", "ByteString"), ("segmentDelimiter", "Char"), ("elementDelimiter", "Char")] ["Show", "Generic"],
--         routeDeclaration "EdiToJsonR" ["convert", "toJson"],
--         routeDeclaration "EdiFromJsonR" ["convert", "fromJson"],
--         routeDeclaration "PathAfterThat" ["convert", "fromJson", "nextPath", "pathAfterThat"],
--         routeDeclaration "YaPath" ["convert", "fromJson", "somePath", "anotherPath", "yaPathHere"],
--         apiInstanceDeclaration instanceTopVec instanceTypeVec, 
--         fromParamInstanceDecl fromParamVec,
--         webApiInstance "EDITranslatorApi" [("EdiToJsonR", ["POST", "PUT", "GET"]), ("EdiFromJsonR", ["GET", "POST"])]
--         ]
--   in writeFile "webapi-swagger/sampleFiles/codeGen.hs" $ prettyPrint hModule



type InnerRecords = [(String, String)]
type DerivingClass = String  

#if MIN_VERSION_haskell_src_exts(1,20,0)
#else
dataDeclaration :: (DataOrNew SrcSpanInfo) -> String -> InnerRecords -> [DerivingClass] -> Decl SrcSpanInfo
dataDeclaration dataOrNew dataName innerRecords derivingList = 
  DataDecl noSrcSpan  
    dataOrNew 
    Nothing 
    (declarationHead dataName)
    (constructorDeclaration dataName innerRecords)
    (Just $ derivingDecl  derivingList)
#endif


declarationHead :: String -> DeclHead SrcSpanInfo
declarationHead declHeadName = (DHead noSrcSpan (Ident noSrcSpan declHeadName) )

constructorDeclaration :: String -> InnerRecords -> [QualConDecl SrcSpanInfo]
constructorDeclaration constructorName innerRecords = 
  [QualConDecl noSrcSpan Nothing Nothing (RecDecl noSrcSpan (nameDecl constructorName) (fmap fieldDecl innerRecords) )] 


stringLiteral :: String -> Exp SrcSpanInfo
stringLiteral str = (Lit noSrcSpan (Language.Haskell.Exts.String noSrcSpan str str))

variableName :: String -> Exp SrcSpanInfo
variableName name = (Var noSrcSpan (UnQual noSrcSpan (nameDecl name) ) )

nameDecl :: String -> Name SrcSpanInfo
nameDecl = Ident noSrcSpan 

fieldDecl :: (String, String) -> FieldDecl SrcSpanInfo
fieldDecl (fieldName, fieldType) = 
  FieldDecl noSrcSpan [nameDecl fieldName] (TyCon noSrcSpan (UnQual noSrcSpan (nameDecl fieldType)))

derivingDecl :: [String] -> Deriving SrcSpanInfo
#if MIN_VERSION_haskell_src_exts(1,20,0)
derivingDecl derivingList = Deriving noSrcSpan Nothing $ fmap iRule derivingList
#else
derivingDecl derivingList = Deriving noSrcSpan $ fmap iRule derivingList
#endif
 where 
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


sumTypeDeclaration :: String -> [String] -> [DerivingClass] -> Decl SrcSpanInfo
sumTypeDeclaration dataName listOfComponents derivingList = 
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
  sumTypeConstructor = 
    fmap (\construcorVal -> QualConDecl noSrcSpan Nothing Nothing 
      (ConDecl noSrcSpan 
        (nameDecl construcorVal) [] ) )

languageExtension :: String -> ModulePragma SrcSpanInfo
languageExtension langExtName = LanguagePragma noSrcSpan [nameDecl langExtName]


-- Modules imported as *NOT qualified* by default for now
moduleImport :: (Bool, String) -> String -> ImportDecl SrcSpanInfo
moduleImport (isQualified, qualifiedName) moduleName  = ImportDecl {importAnn = noSrcSpan, importModule = ModuleName noSrcSpan moduleName, importQualified = False, importSrc = False, importSafe = False, importPkg = Nothing, importAs = Nothing, importSpecs = Nothing}


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
        (unPromotedUnQualSymDecl ":/")
        (processPathComponent lastElem)
      )
    currentRoute:remainingRoute -> 
      (TyInfix noSrcSpan 
        (processPathComponent currentRoute)
        (unPromotedUnQualSymDecl ":/")
        (recursiveTypeForRoute remainingRoute)
      )   
 where 
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


-- Instances for ToJSON and FromJSON
instanceDeclForJSON :: String -> [Decl SrcSpanInfo]
instanceDeclForJSON dataTypeName = [jsonInstance "ToJSON", jsonInstance "FromJSON"]
 where 
  jsonInstance jsonDirection = 
    InstDecl noSrcSpan Nothing 
      (IRule noSrcSpan Nothing Nothing 
        (IHApp noSrcSpan 
          (instanceHead jsonDirection) 
          (typeConstructor dataTypeName)
        )
      ) Nothing

queryParamInstanceIRule :: String -> String -> InstRule SrcSpanInfo
queryParamInstanceIRule paramDirection sumTypeName = 
  IRule noSrcSpan Nothing Nothing 
    (IHApp noSrcSpan 
      (IHApp noSrcSpan 
        (instanceHead paramDirection)
        (TyPromoted noSrcSpan (PromotedCon noSrcSpan True (UnQual noSrcSpan (nameDecl "QueryParam") ) ) )) 
      (typeConstructor sumTypeName) )

-- The ToParam 'QueryParam instance for Sum Type
toParamQueryParamInstance :: String -> Decl SrcSpanInfo
toParamQueryParamInstance sumTypeName = 
  InstDecl noSrcSpan Nothing
  (queryParamInstanceIRule "ToParam" sumTypeName)
  ( Just [InsDecl noSrcSpan 
          (FunBind noSrcSpan 
            [Match noSrcSpan 
              (nameDecl "toParam") 
              [PWildCard noSrcSpan , PVar noSrcSpan (nameDecl "pfx"),PVar noSrcSpan (nameDecl (fmap Char.toLower sumTypeName) )] 
              (UnGuardedRhs noSrcSpan 
                (List noSrcSpan 
                  [Tuple noSrcSpan 
                    Boxed 
                    [Var noSrcSpan 
                      (UnQual noSrcSpan 
                        (nameDecl "pfx")
                      ),
                    InfixApp noSrcSpan 
                      (dataConstructor "Just")
                      (QVarOp noSrcSpan (UnQual noSrcSpan (Symbol noSrcSpan "$"))) 
                      (App noSrcSpan 
                        (variableName "encodeParam")
                        (variableName (fmap Char.toLower sumTypeName) )
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
    (PLit noSrcSpan (Signless noSrcSpan) (Language.Haskell.Exts.String noSrcSpan caseMatchOnStr caseMatchOnStr ) ) 
    (UnGuardedRhs noSrcSpan (App noSrcSpan (dataConstructor "Just") (dataConstructor resultOfCaseMatch) )) 
    Nothing

-- the EncodeParam instance for Sum Type
encodeParamSumTypeInstance :: String -> [(String, String)] -> Decl SrcSpanInfo
encodeParamSumTypeInstance sumTypeName caseOptions =
  InstDecl noSrcSpan Nothing
  (IRule noSrcSpan Nothing Nothing (IHApp noSrcSpan (instanceHead "EncodeParam") (typeConstructor sumTypeName) ))
   (Just [InsDecl noSrcSpan 
    (FunBind noSrcSpan 
      [Match noSrcSpan 
        (nameDecl "encodeParam") 
        [PVar noSrcSpan (nameDecl (fmap Char.toLower sumTypeName) )] 
        (UnGuardedRhs noSrcSpan 
          (Case noSrcSpan 
            (variableName (fmap Char.toLower sumTypeName) )
            (fmap encodeCaseStatementOption caseOptions) )) 
        Nothing
      ])])


-- The DecodeParam Instance for Sum Type
decodeParamSumTypeInstance :: String -> [(String, String)] -> Decl SrcSpanInfo
decodeParamSumTypeInstance sumTypeName caseOptions = 
  InstDecl noSrcSpan Nothing
    (IRule noSrcSpan Nothing Nothing (IHApp noSrcSpan (instanceHead "DecodeParam") (typeConstructor sumTypeName) ))
    (Just [InsDecl noSrcSpan 
      (FunBind noSrcSpan 
        [Match noSrcSpan (nameDecl "decodeParam") 
          [PVar noSrcSpan (nameDecl (fmap Char.toLower sumTypeName) )] 
          (UnGuardedRhs noSrcSpan 
            (Case noSrcSpan 
              (variableName  (fmap Char.toLower sumTypeName)) 
              ((fmap decodeCaseStatementOption caseOptions) ++ [Alt noSrcSpan (PWildCard noSrcSpan) (UnGuardedRhs noSrcSpan (dataConstructor "Nothing") ) Nothing] ) )) 
          Nothing
        ])])

-- The FromParam 'QueryParam instance for Sum Type
fromParamQueryParamInstance :: String -> Decl SrcSpanInfo
fromParamQueryParamInstance sumTypeName = 
  InstDecl noSrcSpan Nothing
  (queryParamInstanceIRule "FromParam" sumTypeName)
  (Just 
    [InsDecl noSrcSpan 
      (FunBind noSrcSpan 
        [Match noSrcSpan 
          (nameDecl "fromParam") 
          (fmap patternVariable ["pt","key","kvs"])
          (UnGuardedRhs noSrcSpan 
            (Case noSrcSpan 
              (App noSrcSpan 
                (App noSrcSpan 
                  (App noSrcSpan 
                    (variableName "lookupParam")                    
                    (variableName "pt") ) 
                  (variableName "key") )
                (variableName"kvs")
              ) 
              [Alt noSrcSpan 
                (PApp noSrcSpan 
                  (UnQual noSrcSpan 
                    (nameDecl "Just")
                  ) 
                  [PParen noSrcSpan 
                    (PApp noSrcSpan 
                      (UnQual noSrcSpan 
                        (nameDecl "Just")
                      ) [patternVariable "par"]
                    )
                  ]
                ) 
                (UnGuardedRhs noSrcSpan 
                  (Do noSrcSpan 
                    [Qualifier noSrcSpan 
                      (Case noSrcSpan 
                        (App noSrcSpan 
                          (variableName "decodeParam")
                          (variableName "par")
                        ) 
                        [Alt noSrcSpan 
                          (PApp noSrcSpan 
                            (UnQual noSrcSpan (nameDecl "Just")) 
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
                                      (variableName "key")
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
                            (variableName "key")
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
routeDeclaration routeName routePathComponents = 
  case routePathComponents of
    (PathComp pathElem):[] -> 
      TypeDecl noSrcSpan
        (declarationHead routeName)
        (TyApp noSrcSpan
          (typeConstructor "Static")
          (recursiveTypeForRoute routePathComponents) )
    _ -> 
      TypeDecl noSrcSpan  
        (declarationHead routeName)
        (recursiveTypeForRoute routePathComponents)


webApiInstance :: String -> [(String, [String])] -> Decl SrcSpanInfo
webApiInstance mainTypeName routeAndMethods =
  InstDecl noSrcSpan Nothing 
    (IRule noSrcSpan Nothing Nothing 
      (IHApp noSrcSpan 
        (instanceHead "WebApi")
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
          (typeConstructor "Route") 
          (TyPromoted noSrcSpan 
            (PromotedList noSrcSpan True 
              (fmap typeConstructor listOfMethods)               
            )
          )
        ) 
        (typeConstructor rName)

---------------------------------------------------------------------------------------
-- Support multiple versions of GHC (Use ifndef )
-- for LTS 9.0 -> 1.18.2

#if MIN_VERSION_haskell_src_exts(1,20,0)
-- for haskell-src-exts 1.20.x
dataDeclaration :: Decl SrcSpanInfo
dataDeclaration = 
    DataDecl noSrcSpan  
      (NewType noSrcSpan) 
      Nothing 
      (DHead noSrcSpan (Ident noSrcSpan "CharacterSet") )
      [QualConDecl noSrcSpan Nothing Nothing (RecDecl noSrcSpan (Ident noSrcSpan "CharacterSet") [FieldDecl noSrcSpan [Ident noSrcSpan "characterSet"] (TyCon noSrcSpan (UnQual noSrcSpan (Ident noSrcSpan "ByteString")))])] 
      [Deriving noSrcSpan Nothing [IRule noSrcSpan Nothing Nothing (IHCon noSrcSpan (UnQual noSrcSpan (Ident noSrcSpan "Show"))),IRule noSrcSpan Nothing Nothing (IHCon noSrcSpan (UnQual noSrcSpan (Ident noSrcSpan "Generic")))]]
#endif




-- PathItem {
--   _pathItemGet = 
--     Just (Operation 
--           {_operationTags = fromList ["user"], 
--            _operationSummary = Just "Logs out current logged in user session", 
--            _operationDescription = Just "", 
--            _operationExternalDocs = Nothing, 
--            _operationOperationId = Just "logoutUser", 
--            _operationConsumes = Nothing, 
--            _operationProduces = Just (MimeList {getMimeList = [application/xml,application/json]}), 
--            _operationParameters = [], 
--            _operationResponses = 
--               Responses {
--                 _responsesDefault = Just (Inline (
--                   Response {
--                     _responseDescription = "successful operation", 
--                     _responseSchema = Nothing, 
--                     _responseHeaders = fromList [], 
--                     _responseExamples = Nothing})), 
--                 _responsesResponses = fromList []}, 
--            _operationSchemes = Nothing, 
--            _operationDeprecated = Nothing, 
--            _operationSecurity = []}), 
  
--   _pathItemPut = Nothing, 
--   _pathItemPost = Nothing, 
--   _pathItemDelete = Nothing, 
--   _pathItemOptions = Nothing, 
--   _pathItemHead = Nothing, 
--   _pathItemPatch = Nothing, 
--   _pathItemParameters = []
--   }




-- Swagger {
--   _swaggerInfo = 
--     Info {_infoTitle = "Swagger Petstore", _infoDescription = Just "This is a sample server Petstore server.  You can find out more about     Swagger at [http://swagger.io](http://swagger.io) or on [irc.freenode.net, #swagger](http://swagger.io/irc/).      For this sample, you can use the api key `special-key` to test the authorization     filters.", _infoTermsOfService = Just "http://swagger.io/terms/", _infoContact = Just (Contact {_contactName = Nothing, _contactUrl = Nothing, _contactEmail = Just "apiteam@swagger.io"}), _infoLicense = Just (License {_licenseName = "Apache 2.0", _licenseUrl = Just (URL {getUrl = "http://www.apache.org/licenses/LICENSE-2.0.html"})}), _infoVersion = "1.0.0"}, 
  
--   _swaggerHost = Just (Host {_hostName = "petstore.swagger.io", _hostPort = Nothing}), 
--   _swaggerBasePath = Just "/v2", 
--   _swaggerSchemes = Just [Https,Http], 
--   _swaggerConsumes = MimeList {getMimeList = []}, 
--   _swaggerProduces = MimeList {getMimeList = []}, 
--   _swaggerPaths = fromList [("/user/logout",PathItem {_pathItemGet = Just (Operation {_operationTags = fromList ["user"], _operationSummary = Just "Logs out current logged in user session", _operationDescription = Just "", _operationExternalDocs = Nothing, _operationOperationId = Just "logoutUser", _operationConsumes = Nothing, _operationProduces = Just (MimeList {getMimeList = [application/xml,application/json]}), _operationParameters = [], _operationResponses = Responses {_responsesDefault = Just (Inline (Response {_responseDescription = "successful operation", _responseSchema = Nothing, _responseHeaders = fromList [], _responseExamples = Nothing})), _responsesResponses = fromList []}, _operationSchemes = Nothing, _operationDeprecated = Nothing, _operationSecurity = []}), _pathItemPut = Nothing, _pathItemPost = Nothing, _pathItemDelete = Nothing, _pathItemOptions = Nothing, _pathItemHead = Nothing, _pathItemPatch = Nothing, _pathItemParameters = []})
--   ,("/pet/findByStatus",PathItem {_pathItemGet = Just (Operation {_operationTags = fromList ["pet"], _operationSummary = Just "Finds Pets by status", _operationDescription = Just "Multiple status values can be provided with comma separated strings", _operationExternalDocs = Nothing, _operationOperationId = Just "findPetsByStatus", _operationConsumes = Nothing, _operationProduces = Just (MimeList {getMimeList = [application/xml,application/json]}), _operationParameters = [Inline (Param {_paramName = "status", _paramDescription = Just "Status values that need to be considered for filter", _paramRequired = Just True, _paramSchema = ParamOther (ParamOtherSchema {_paramOtherSchemaIn = ParamQuery, _paramOtherSchemaAllowEmptyValue = Nothing, _paramOtherSchemaParamSchema = ParamSchema {_paramSchemaDefault = Nothing, _paramSchemaType = SwaggerArray, _paramSchemaFormat = Nothing, _paramSchemaItems = Just (SwaggerItemsPrimitive (Just CollectionMulti) (ParamSchema {_paramSchemaDefault = Just (String "available"), _paramSchemaType = SwaggerString, _paramSchemaFormat = Nothing, _paramSchemaItems = Nothing, _paramSchemaMaximum = Nothing, _paramSchemaExclusiveMaximum = Nothing, _paramSchemaMinimum = Nothing, _paramSchemaExclusiveMinimum = Nothing, _paramSchemaMaxLength = Nothing, _paramSchemaMinLength = Nothing, _paramSchemaPattern = Nothing, _paramSchemaMaxItems = Nothing, _paramSchemaMinItems = Nothing, _paramSchemaUniqueItems = Nothing, _paramSchemaEnum = Just [String "available",String "pending",String "sold"], _paramSchemaMultipleOf = Nothing})), _paramSchemaMaximum = Nothing, _paramSchemaExclusiveMaximum = Nothing, _paramSchemaMinimum = Nothing, _paramSchemaExclusiveMinimum = Nothing, _paramSchemaMaxLength = Nothing, _paramSchemaMinLength = Nothing, _paramSchemaPattern = Nothing, _paramSchemaMaxItems = Nothing, _paramSchemaMinItems = Nothing, _paramSchemaUniqueItems = Nothing, _paramSchemaEnum = Nothing, _paramSchemaMultipleOf = Nothing}})})], _operationResponses = Responses {_responsesDefault = Nothing, _responsesResponses = fromList [(400,Inline (Response {_responseDescription = "Invalid status value", _responseSchema = Nothing, _responseHeaders = fromList [], _responseExamples = Nothing})),(200,Inline (Response {_responseDescription = "successful operation", _responseSchema = Just (Inline (Schema {_schemaTitle = Nothing, _schemaDescription = Nothing, _schemaRequired = [], _schemaAllOf = Nothing, _schemaProperties = fromList [], _schemaAdditionalProperties = Nothing, _schemaDiscriminator = Nothing, _schemaReadOnly = Nothing, _schemaXml = Nothing, _schemaExternalDocs = Nothing, _schemaExample = Nothing, _schemaMaxProperties = Nothing, _schemaMinProperties = Nothing, _schemaParamSchema = ParamSchema {_paramSchemaDefault = Nothing, _paramSchemaType = SwaggerArray, _paramSchemaFormat = Nothing, _paramSchemaItems = Just (SwaggerItemsObject (Ref (Reference {getReference = "Pet"}))), _paramSchemaMaximum = Nothing, _paramSchemaExclusiveMaximum = Nothing, _paramSchemaMinimum = Nothing, _paramSchemaExclusiveMinimum = Nothing, _paramSchemaMaxLength = Nothing, _paramSchemaMinLength = Nothing, _paramSchemaPattern = Nothing, _paramSchemaMaxItems = Nothing, _paramSchemaMinItems = Nothing, _paramSchemaUniqueItems = Nothing, _paramSchemaEnum = Nothing, _paramSchemaMultipleOf = Nothing}})), _responseHeaders = fromList [], _responseExamples = Nothing}))]}, _operationSchemes = Nothing, _operationDeprecated = Nothing, _operationSecurity = [SecurityRequirement {getSecurityRequirement = fromList [("petstore_auth",["write:pets","read:pets"])]}]}), _pathItemPut = Nothing, _pathItemPost = Nothing, _pathItemDelete = Nothing, _pathItemOptions = Nothing, _pathItemHead = Nothing, _pathItemPatch = Nothing, _pathItemParameters = []})
--   ,("/user/login",PathItem {_pathItemGet = Just (Operation {_operationTags = fromList ["user"], _operationSummary = Just "Logs user into the system", _operationDescription = Just "", _operationExternalDocs = Nothing, _operationOperationId = Just "loginUser", _operationConsumes = Nothing, _operationProduces = Just (MimeList {getMimeList = [application/xml,application/json]}), _operationParameters = [Inline (Param {_paramName = "username", _paramDescription = Just "The user name for login", _paramRequired = Just True, _paramSchema = ParamOther (ParamOtherSchema {_paramOtherSchemaIn = ParamQuery, _paramOtherSchemaAllowEmptyValue = Nothing, _paramOtherSchemaParamSchema = ParamSchema {_paramSchemaDefault = Nothing, _paramSchemaType = SwaggerString, _paramSchemaFormat = Nothing, _paramSchemaItems = Nothing, _paramSchemaMaximum = Nothing, _paramSchemaExclusiveMaximum = Nothing, _paramSchemaMinimum = Nothing, _paramSchemaExclusiveMinimum = Nothing, _paramSchemaMaxLength = Nothing, _paramSchemaMinLength = Nothing, _paramSchemaPattern = Nothing, _paramSchemaMaxItems = Nothing, _paramSchemaMinItems = Nothing, _paramSchemaUniqueItems = Nothing, _paramSchemaEnum = Nothing, _paramSchemaMultipleOf = Nothing}})}),Inline (Param {_paramName = "password", _paramDescription = Just "The password for login in clear text", _paramRequired = Just True, _paramSchema = ParamOther (ParamOtherSchema {_paramOtherSchemaIn = ParamQuery, _paramOtherSchemaAllowEmptyValue = Nothing, _paramOtherSchemaParamSchema = ParamSchema {_paramSchemaDefault = Nothing, _paramSchemaType = SwaggerString, _paramSchemaFormat = Nothing, _paramSchemaItems = Nothing, _paramSchemaMaximum = Nothing, _paramSchemaExclusiveMaximum = Nothing, _paramSchemaMinimum = Nothing, _paramSchemaExclusiveMinimum = Nothing, _paramSchemaMaxLength = Nothing, _paramSchemaMinLength = Nothing, _paramSchemaPattern = Nothing, _paramSchemaMaxItems = Nothing, _paramSchemaMinItems = Nothing, _paramSchemaUniqueItems = Nothing, _paramSchemaEnum = Nothing, _paramSchemaMultipleOf = Nothing}})})], _operationResponses = Responses {_responsesDefault = Nothing, _responsesResponses = fromList [(400,Inline (Response {_responseDescription = "Invalid username/password supplied", _responseSchema = Nothing, _responseHeaders = fromList [], _responseExamples = Nothing})),(200,Inline (Response {_responseDescription = "successful operation", _responseSchema = Just (Inline (Schema {_schemaTitle = Nothing, _schemaDescription = Nothing, _schemaRequired = [], _schemaAllOf = Nothing, _schemaProperties = fromList [], _schemaAdditionalProperties = Nothing, _schemaDiscriminator = Nothing, _schemaReadOnly = Nothing, _schemaXml = Nothing, _schemaExternalDocs = Nothing, _schemaExample = Nothing, _schemaMaxProperties = Nothing, _schemaMinProperties = Nothing, _schemaParamSchema = ParamSchema {_paramSchemaDefault = Nothing, _paramSchemaType = SwaggerString, _paramSchemaFormat = Nothing, _paramSchemaItems = Nothing, _paramSchemaMaximum = Nothing, _paramSchemaExclusiveMaximum = Nothing, _paramSchemaMinimum = Nothing, _paramSchemaExclusiveMinimum = Nothing, _paramSchemaMaxLength = Nothing, _paramSchemaMinLength = Nothing, _paramSchemaPattern = Nothing, _paramSchemaMaxItems = Nothing, _paramSchemaMinItems = Nothing, _paramSchemaUniqueItems = Nothing, _paramSchemaEnum = Nothing, _paramSchemaMultipleOf = Nothing}})), _responseHeaders = fromList [("X-Rate-Limit",Header {_headerDescription = Just "calls per hour allowed by the user", _headerParamSchema = ParamSchema {_paramSchemaDefault = Nothing, _paramSchemaType = SwaggerInteger, _paramSchemaFormat = Just "int32", _paramSchemaItems = Nothing, _paramSchemaMaximum = Nothing, _paramSchemaExclusiveMaximum = Nothing, _paramSchemaMinimum = Nothing, _paramSchemaExclusiveMinimum = Nothing, _paramSchemaMaxLength = Nothing, _paramSchemaMinLength = Nothing, _paramSchemaPattern = Nothing, _paramSchemaMaxItems = Nothing, _paramSchemaMinItems = Nothing, _paramSchemaUniqueItems = Nothing, _paramSchemaEnum = Nothing, _paramSchemaMultipleOf = Nothing}})
--   ,("X-Expires-After",Header {_headerDescription = Just "date in UTC when token expires", _headerParamSchema = ParamSchema {_paramSchemaDefault = Nothing, _paramSchemaType = SwaggerString, _paramSchemaFormat = Just "date-time", _paramSchemaItems = Nothing, _paramSchemaMaximum = Nothing, _paramSchemaExclusiveMaximum = Nothing, _paramSchemaMinimum = Nothing, _paramSchemaExclusiveMinimum = Nothing, _paramSchemaMaxLength = Nothing, _paramSchemaMinLength = Nothing, _paramSchemaPattern = Nothing, _paramSchemaMaxItems = Nothing, _paramSchemaMinItems = Nothing, _paramSchemaUniqueItems = Nothing, _paramSchemaEnum = Nothing, _paramSchemaMultipleOf = Nothing}})], _responseExamples = Nothing}))]}, _operationSchemes = Nothing, _operationDeprecated = Nothing, _operationSecurity = []}), _pathItemPut = Nothing, _pathItemPost = Nothing, _pathItemDelete = Nothing, _pathItemOptions = Nothing, _pathItemHead = Nothing, _pathItemPatch = Nothing, _pathItemParameters = []})
--   ,("/user",PathItem {_pathItemGet = Nothing, _pathItemPut = Nothing, _pathItemPost = Just (Operation {_operationTags = fromList ["user"], _operationSummary = Just "Create user", _operationDescription = Just "This can only be done by the logged in user.", _operationExternalDocs = Nothing, _operationOperationId = Just "createUser", _operationConsumes = Nothing, _operationProduces = Just (MimeList {getMimeList = [application/xml,application/json]}), _operationParameters = [Inline (Param {_paramName = "body", _paramDescription = Just "Created user object", _paramRequired = Just True, _paramSchema = ParamBody (Ref (Reference {getReference = "User"}))})], _operationResponses = Responses {_responsesDefault = Just (Inline (Response {_responseDescription = "successful operation", _responseSchema = Nothing, _responseHeaders = fromList [], _responseExamples = Nothing})), _responsesResponses = fromList []}, _operationSchemes = Nothing, _operationDeprecated = Nothing, _operationSecurity = []}), _pathItemDelete = Nothing, _pathItemOptions = Nothing, _pathItemHead = Nothing, _pathItemPatch = Nothing, _pathItemParameters = []})
--   ,("/pet/findByTags",PathItem {_pathItemGet = Just (Operation {_operationTags = fromList ["pet"], _operationSummary = Just "Finds Pets by tags", _operationDescription = Just "Muliple tags can be provided with comma separated strings. Use         tag1, tag2, tag3 for testing.", _operationExternalDocs = Nothing, _operationOperationId = Just "findPetsByTags", _operationConsumes = Nothing, _operationProduces = Just (MimeList {getMimeList = [application/xml,application/json]}), _operationParameters = [Inline (Param {_paramName = "tags", _paramDescription = Just "Tags to filter by", _paramRequired = Just True, _paramSchema = ParamOther (ParamOtherSchema {_paramOtherSchemaIn = ParamQuery, _paramOtherSchemaAllowEmptyValue = Nothing, _paramOtherSchemaParamSchema = ParamSchema {_paramSchemaDefault = Nothing, _paramSchemaType = SwaggerArray, _paramSchemaFormat = Nothing, _paramSchemaItems = Just (SwaggerItemsPrimitive (Just CollectionMulti) (ParamSchema {_paramSchemaDefault = Nothing, _paramSchemaType = SwaggerString, _paramSchemaFormat = Nothing, _paramSchemaItems = Nothing, _paramSchemaMaximum = Nothing, _paramSchemaExclusiveMaximum = Nothing, _paramSchemaMinimum = Nothing, _paramSchemaExclusiveMinimum = Nothing, _paramSchemaMaxLength = Nothing, _paramSchemaMinLength = Nothing, _paramSchemaPattern = Nothing, _paramSchemaMaxItems = Nothing, _paramSchemaMinItems = Nothing, _paramSchemaUniqueItems = Nothing, _paramSchemaEnum = Nothing, _paramSchemaMultipleOf = Nothing})), _paramSchemaMaximum = Nothing, _paramSchemaExclusiveMaximum = Nothing, _paramSchemaMinimum = Nothing, _paramSchemaExclusiveMinimum = Nothing, _paramSchemaMaxLength = Nothing, _paramSchemaMinLength = Nothing, _paramSchemaPattern = Nothing, _paramSchemaMaxItems = Nothing, _paramSchemaMinItems = Nothing, _paramSchemaUniqueItems = Nothing, _paramSchemaEnum = Nothing, _paramSchemaMultipleOf = Nothing}})})], _operationResponses = Responses {_responsesDefault = Nothing, _responsesResponses = fromList [(400,Inline (Response {_responseDescription = "Invalid tag value", _responseSchema = Nothing, _responseHeaders = fromList [], _responseExamples = Nothing})),(200,Inline (Response {_responseDescription = "successful operation", _responseSchema = Just (Inline (Schema {_schemaTitle = Nothing, _schemaDescription = Nothing, _schemaRequired = [], _schemaAllOf = Nothing, _schemaProperties = fromList [], _schemaAdditionalProperties = Nothing, _schemaDiscriminator = Nothing, _schemaReadOnly = Nothing, _schemaXml = Nothing, _schemaExternalDocs = Nothing, _schemaExample = Nothing, _schemaMaxProperties = Nothing, _schemaMinProperties = Nothing, _schemaParamSchema = ParamSchema {_paramSchemaDefault = Nothing, _paramSchemaType = SwaggerArray, _paramSchemaFormat = Nothing, _paramSchemaItems = Just (SwaggerItemsObject (Ref (Reference {getReference = "Pet"}))), _paramSchemaMaximum = Nothing, _paramSchemaExclusiveMaximum = Nothing, _paramSchemaMinimum = Nothing, _paramSchemaExclusiveMinimum = Nothing, _paramSchemaMaxLength = Nothing, _paramSchemaMinLength = Nothing, _paramSchemaPattern = Nothing, _paramSchemaMaxItems = Nothing, _paramSchemaMinItems = Nothing, _paramSchemaUniqueItems = Nothing, _paramSchemaEnum = Nothing, _paramSchemaMultipleOf = Nothing}})), _responseHeaders = fromList [], _responseExamples = Nothing}))]}, _operationSchemes = Nothing, _operationDeprecated = Just True, _operationSecurity = [SecurityRequirement {getSecurityRequirement = fromList [("petstore_auth",["write:pets","read:pets"])]}]}), _pathItemPut = Nothing, _pathItemPost = Nothing, _pathItemDelete = Nothing, _pathItemOptions = Nothing, _pathItemHead = Nothing, _pathItemPatch = Nothing, _pathItemParameters = []})
  
--   ,("/store/order",PathItem {_pathItemGet = Nothing, _pathItemPut = Nothing, _pathItemPost = Just (Operation {_operationTags = fromList ["store"], _operationSummary = Just "Place an order for a pet", _operationDescription = Just "", _operationExternalDocs = Nothing, _operationOperationId = Just "placeOrder", _operationConsumes = Nothing, _operationProduces = Just (MimeList {getMimeList = [application/xml,application/json]}), _operationParameters = [Inline (Param {_paramName = "body", _paramDescription = Just "order placed for purchasing the pet", _paramRequired = Just True, _paramSchema = ParamBody (Ref (Reference {getReference = "Order"}))})], _operationResponses = Responses {_responsesDefault = Nothing, _responsesResponses = fromList [(400,Inline (Response {_responseDescription = "Invalid Order", _responseSchema = Nothing, _responseHeaders = fromList [], _responseExamples = Nothing})),(200,Inline (Response {_responseDescription = "successful operation", _responseSchema = Just (Ref (Reference {getReference = "Order"})), _responseHeaders = fromList [], _responseExamples = Nothing}))]}, _operationSchemes = Nothing, _operationDeprecated = Nothing, _operationSecurity = []}), _pathItemDelete = Nothing, _pathItemOptions = Nothing, _pathItemHead = Nothing, _pathItemPatch = Nothing, _pathItemParameters = []})
--   ,("/pet/{petId}/uploadImage",PathItem {_pathItemGet = Nothing, _pathItemPut = Nothing, _pathItemPost = Just (Operation {_operationTags = fromList ["pet"], _operationSummary = Just "uploads an image", _operationDescription = Just "", _operationExternalDocs = Nothing, _operationOperationId = Just "uploadFile", _operationConsumes = Just (MimeList {getMimeList = [multipart/form-data]}), _operationProduces = Just (MimeList {getMimeList = [application/json]}), _operationParameters = [Inline (Param {_paramName = "petId", _paramDescription = Just "ID of pet to update", _paramRequired = Just True, _paramSchema = ParamOther (ParamOtherSchema {_paramOtherSchemaIn = ParamPath, _paramOtherSchemaAllowEmptyValue = Nothing, _paramOtherSchemaParamSchema = ParamSchema {_paramSchemaDefault = Nothing, _paramSchemaType = SwaggerInteger, _paramSchemaFormat = Just "int64", _paramSchemaItems = Nothing, _paramSchemaMaximum = Nothing, _paramSchemaExclusiveMaximum = Nothing, _paramSchemaMinimum = Nothing, _paramSchemaExclusiveMinimum = Nothing, _paramSchemaMaxLength = Nothing, _paramSchemaMinLength = Nothing, _paramSchemaPattern = Nothing, _paramSchemaMaxItems = Nothing, _paramSchemaMinItems = Nothing, _paramSchemaUniqueItems = Nothing, _paramSchemaEnum = Nothing, _paramSchemaMultipleOf = Nothing}})}),Inline (Param {_paramName = "additionalMetadata", _paramDescription = Just "Additional data to pass to server", _paramRequired = Just False, _paramSchema = ParamOther (ParamOtherSchema {_paramOtherSchemaIn = ParamFormData, _paramOtherSchemaAllowEmptyValue = Nothing, _paramOtherSchemaParamSchema = ParamSchema {_paramSchemaDefault = Nothing, _paramSchemaType = SwaggerString, _paramSchemaFormat = Nothing, _paramSchemaItems = Nothing, _paramSchemaMaximum = Nothing, _paramSchemaExclusiveMaximum = Nothing, _paramSchemaMinimum = Nothing, _paramSchemaExclusiveMinimum = Nothing, _paramSchemaMaxLength = Nothing, _paramSchemaMinLength = Nothing, _paramSchemaPattern = Nothing, _paramSchemaMaxItems = Nothing, _paramSchemaMinItems = Nothing, _paramSchemaUniqueItems = Nothing, _paramSchemaEnum = Nothing, _paramSchemaMultipleOf = Nothing}})}),Inline (Param {_paramName = "file", _paramDescription = Just "file to upload", _paramRequired = Just False, _paramSchema = ParamOther (ParamOtherSchema {_paramOtherSchemaIn = ParamFormData, _paramOtherSchemaAllowEmptyValue = Nothing, _paramOtherSchemaParamSchema = ParamSchema {_paramSchemaDefault = Nothing, _paramSchemaType = SwaggerFile, _paramSchemaFormat = Nothing, _paramSchemaItems = Nothing, _paramSchemaMaximum = Nothing, _paramSchemaExclusiveMaximum = Nothing, _paramSchemaMinimum = Nothing, _paramSchemaExclusiveMinimum = Nothing, _paramSchemaMaxLength = Nothing, _paramSchemaMinLength = Nothing, _paramSchemaPattern = Nothing, _paramSchemaMaxItems = Nothing, _paramSchemaMinItems = Nothing, _paramSchemaUniqueItems = Nothing, _paramSchemaEnum = Nothing, _paramSchemaMultipleOf = Nothing}})})], _operationResponses = Responses {_responsesDefault = Nothing, _responsesResponses = fromList [(200,Inline (Response {_responseDescription = "successful operation", _responseSchema = Just (Ref (Reference {getReference = "ApiResponse"})), _responseHeaders = fromList [], _responseExamples = Nothing}))]}, _operationSchemes = Nothing, _operationDeprecated = Nothing, _operationSecurity = [SecurityRequirement {getSecurityRequirement = fromList [("petstore_auth",["write:pets","read:pets"])]}]}), _pathItemDelete = Nothing, _pathItemOptions = Nothing, _pathItemHead = Nothing, _pathItemPatch = Nothing, _pathItemParameters = []})
--   ,("/store/order/{orderId}",PathItem {_pathItemGet = Just (Operation {_operationTags = fromList ["store"], _operationSummary = Just "Find purchase order by ID", _operationDescription = Just "For valid response try integer IDs with value >= 1 and <= 10.         Other values will generated exceptions", _operationExternalDocs = Nothing, _operationOperationId = Just "getOrderById", _operationConsumes = Nothing, _operationProduces = Just (MimeList {getMimeList = [application/xml,application/json]}), _operationParameters = [Inline (Param {_paramName = "orderId", _paramDescription = Just "ID of pet that needs to be fetched", _paramRequired = Just True, _paramSchema = ParamOther (ParamOtherSchema {_paramOtherSchemaIn = ParamPath, _paramOtherSchemaAllowEmptyValue = Nothing, _paramOtherSchemaParamSchema = ParamSchema {_paramSchemaDefault = Nothing, _paramSchemaType = SwaggerInteger, _paramSchemaFormat = Just "int64", _paramSchemaItems = Nothing, _paramSchemaMaximum = Just 10.0, _paramSchemaExclusiveMaximum = Nothing, _paramSchemaMinimum = Just 1.0, _paramSchemaExclusiveMinimum = Nothing, _paramSchemaMaxLength = Nothing, _paramSchemaMinLength = Nothing, _paramSchemaPattern = Nothing, _paramSchemaMaxItems = Nothing, _paramSchemaMinItems = Nothing, _paramSchemaUniqueItems = Nothing, _paramSchemaEnum = Nothing, _paramSchemaMultipleOf = Nothing}})})], _operationResponses = Responses {_responsesDefault = Nothing, _responsesResponses = fromList [(400,Inline (Response {_responseDescription = "Invalid ID supplied", _responseSchema = Nothing, _responseHeaders = fromList [], _responseExamples = Nothing})),(404,Inline (Response {_responseDescription = "Order not found", _responseSchema = Nothing, _responseHeaders = fromList [], _responseExamples = Nothing})),(200,Inline (Response {_responseDescription = "successful operation", _responseSchema = Just (Ref (Reference {getReference = "Order"})), _responseHeaders = fromList [], _responseExamples = Nothing}))]}, _operationSchemes = Nothing, _operationDeprecated = Nothing, _operationSecurity = []}), _pathItemPut = Nothing, _pathItemPost = Nothing, _pathItemDelete = Just (Operation {_operationTags = fromList ["store"], _operationSummary = Just "Delete purchase order by ID", _operationDescription = Just "For valid response try integer IDs with positive integer value.         Negative or non-integer values will generate API errors", _operationExternalDocs = Nothing, _operationOperationId = Just "deleteOrder", _operationConsumes = Nothing, _operationProduces = Just (MimeList {getMimeList = [application/xml,application/json]}), _operationParameters = [Inline (Param {_paramName = "orderId", _paramDescription = Just "ID of the order that needs to be deleted", _paramRequired = Just True, _paramSchema = ParamOther (ParamOtherSchema {_paramOtherSchemaIn = ParamPath, _paramOtherSchemaAllowEmptyValue = Nothing, _paramOtherSchemaParamSchema = ParamSchema {_paramSchemaDefault = Nothing, _paramSchemaType = SwaggerInteger, _paramSchemaFormat = Just "int64", _paramSchemaItems = Nothing, _paramSchemaMaximum = Nothing, _paramSchemaExclusiveMaximum = Nothing, _paramSchemaMinimum = Just 1.0, _paramSchemaExclusiveMinimum = Nothing, _paramSchemaMaxLength = Nothing, _paramSchemaMinLength = Nothing, _paramSchemaPattern = Nothing, _paramSchemaMaxItems = Nothing, _paramSchemaMinItems = Nothing, _paramSchemaUniqueItems = Nothing, _paramSchemaEnum = Nothing, _paramSchemaMultipleOf = Nothing}})})], _operationResponses = Responses {_responsesDefault = Nothing, _responsesResponses = fromList [(400,Inline (Response {_responseDescription = "Invalid ID supplied", _responseSchema = Nothing, _responseHeaders = fromList [], _responseExamples = Nothing})),(404,Inline (Response {_responseDescription = "Order not found", _responseSchema = Nothing, _responseHeaders = fromList [], _responseExamples = Nothing}))]}, _operationSchemes = Nothing, _operationDeprecated = Nothing, _operationSecurity = []}), _pathItemOptions = Nothing, _pathItemHead = Nothing, _pathItemPatch = Nothing, _pathItemParameters = []})
--   ,("/store/inventory",PathItem {_pathItemGet = Just (Operation {_operationTags = fromList ["store"], _operationSummary = Just "Returns pet inventories by status", _operationDescription = Just "Returns a map of status codes to quantities", _operationExternalDocs = Nothing, _operationOperationId = Just "getInventory", _operationConsumes = Nothing, _operationProduces = Just (MimeList {getMimeList = [application/json]}), _operationParameters = [], _operationResponses = Responses {_responsesDefault = Nothing, _responsesResponses = fromList [(200,Inline (Response {_responseDescription = "successful operation", _responseSchema = Just (Inline (Schema {_schemaTitle = Nothing, _schemaDescription = Nothing, _schemaRequired = [], _schemaAllOf = Nothing, _schemaProperties = fromList [], _schemaAdditionalProperties = Just (Inline (Schema {_schemaTitle = Nothing, _schemaDescription = Nothing, _schemaRequired = [], _schemaAllOf = Nothing, _schemaProperties = fromList [], _schemaAdditionalProperties = Nothing, _schemaDiscriminator = Nothing, _schemaReadOnly = Nothing, _schemaXml = Nothing, _schemaExternalDocs = Nothing, _schemaExample = Nothing, _schemaMaxProperties = Nothing, _schemaMinProperties = Nothing, _schemaParamSchema = ParamSchema {_paramSchemaDefault = Nothing, _paramSchemaType = SwaggerInteger, _paramSchemaFormat = Just "int32", _paramSchemaItems = Nothing, _paramSchemaMaximum = Nothing, _paramSchemaExclusiveMaximum = Nothing, _paramSchemaMinimum = Nothing, _paramSchemaExclusiveMinimum = Nothing, _paramSchemaMaxLength = Nothing, _paramSchemaMinLength = Nothing, _paramSchemaPattern = Nothing, _paramSchemaMaxItems = Nothing, _paramSchemaMinItems = Nothing, _paramSchemaUniqueItems = Nothing, _paramSchemaEnum = Nothing, _paramSchemaMultipleOf = Nothing}})), _schemaDiscriminator = Nothing, _schemaReadOnly = Nothing, _schemaXml = Nothing, _schemaExternalDocs = Nothing, _schemaExample = Nothing, _schemaMaxProperties = Nothing, _schemaMinProperties = Nothing, _schemaParamSchema = ParamSchema {_paramSchemaDefault = Nothing, _paramSchemaType = SwaggerObject, _paramSchemaFormat = Nothing, _paramSchemaItems = Nothing, _paramSchemaMaximum = Nothing, _paramSchemaExclusiveMaximum = Nothing, _paramSchemaMinimum = Nothing, _paramSchemaExclusiveMinimum = Nothing, _paramSchemaMaxLength = Nothing, _paramSchemaMinLength = Nothing, _paramSchemaPattern = Nothing, _paramSchemaMaxItems = Nothing, _paramSchemaMinItems = Nothing, _paramSchemaUniqueItems = Nothing, _paramSchemaEnum = Nothing, _paramSchemaMultipleOf = Nothing}})), _responseHeaders = fromList [], _responseExamples = Nothing}))]}, _operationSchemes = Nothing, _operationDeprecated = Nothing, _operationSecurity = [SecurityRequirement {getSecurityRequirement = fromList [("api_key",[])]}]}), _pathItemPut = Nothing, _pathItemPost = Nothing, _pathItemDelete = Nothing, _pathItemOptions = Nothing, _pathItemHead = Nothing, _pathItemPatch = Nothing, _pathItemParameters = []})
--   ,("/pet/{petId}",PathItem {_pathItemGet = Just (Operation {_operationTags = fromList ["pet"], _operationSummary = Just "Find pet by ID", _operationDescription = Just "Returns a single pet", _operationExternalDocs = Nothing, _operationOperationId = Just "getPetById", _operationConsumes = Nothing, _operationProduces = Just (MimeList {getMimeList = [application/xml,application/json]}), _operationParameters = [Inline (Param {_paramName = "petId", _paramDescription = Just "ID of pet to return", _paramRequired = Just True, _paramSchema = ParamOther (ParamOtherSchema {_paramOtherSchemaIn = ParamPath, _paramOtherSchemaAllowEmptyValue = Nothing, _paramOtherSchemaParamSchema = ParamSchema {_paramSchemaDefault = Nothing, _paramSchemaType = SwaggerInteger, _paramSchemaFormat = Just "int64", _paramSchemaItems = Nothing, _paramSchemaMaximum = Nothing, _paramSchemaExclusiveMaximum = Nothing, _paramSchemaMinimum = Nothing, _paramSchemaExclusiveMinimum = Nothing, _paramSchemaMaxLength = Nothing, _paramSchemaMinLength = Nothing, _paramSchemaPattern = Nothing, _paramSchemaMaxItems = Nothing, _paramSchemaMinItems = Nothing, _paramSchemaUniqueItems = Nothing, _paramSchemaEnum = Nothing, _paramSchemaMultipleOf = Nothing}})})], _operationResponses = Responses {_responsesDefault = Nothing, _responsesResponses = fromList [(400,Inline (Response {_responseDescription = "Invalid ID supplied", _responseSchema = Nothing, _responseHeaders = fromList [], _responseExamples = Nothing})),(404,Inline (Response {_responseDescription = "Pet not found", _responseSchema = Nothing, _responseHeaders = fromList [], _responseExamples = Nothing})),(200,Inline (Response {_responseDescription = "successful operation", _responseSchema = Just (Ref (Reference {getReference = "Pet"})), _responseHeaders = fromList [], _responseExamples = Nothing}))]}, _operationSchemes = Nothing, _operationDeprecated = Nothing, _operationSecurity = [SecurityRequirement {getSecurityRequirement = fromList [("api_key",[])]}]}), _pathItemPut = Nothing, _pathItemPost = Just (Operation {_operationTags = fromList ["pet"], _operationSummary = Just "Updates a pet in the store with form data", _operationDescription = Just "", _operationExternalDocs = Nothing, _operationOperationId = Just "updatePetWithForm", _operationConsumes = Just (MimeList {getMimeList = [application/x-www-form-urlencoded]}), _operationProduces = Just (MimeList {getMimeList = [application/xml,application/json]}), _operationParameters = [Inline (Param {_paramName = "petId", _paramDescription = Just "ID of pet that needs to be updated", _paramRequired = Just True, _paramSchema = ParamOther (ParamOtherSchema {_paramOtherSchemaIn = ParamPath, _paramOtherSchemaAllowEmptyValue = Nothing, _paramOtherSchemaParamSchema = ParamSchema {_paramSchemaDefault = Nothing, _paramSchemaType = SwaggerInteger, _paramSchemaFormat = Just "int64", _paramSchemaItems = Nothing, _paramSchemaMaximum = Nothing, _paramSchemaExclusiveMaximum = Nothing, _paramSchemaMinimum = Nothing, _paramSchemaExclusiveMinimum = Nothing, _paramSchemaMaxLength = Nothing, _paramSchemaMinLength = Nothing, _paramSchemaPattern = Nothing, _paramSchemaMaxItems = Nothing, _paramSchemaMinItems = Nothing, _paramSchemaUniqueItems = Nothing, _paramSchemaEnum = Nothing, _paramSchemaMultipleOf = Nothing}})}),Inline (Param {_paramName = "name", _paramDescription = Just "Updated name of the pet", _paramRequired = Just False, _paramSchema = ParamOther (ParamOtherSchema {_paramOtherSchemaIn = ParamFormData, _paramOtherSchemaAllowEmptyValue = Nothing, _paramOtherSchemaParamSchema = ParamSchema {_paramSchemaDefault = Nothing, _paramSchemaType = SwaggerString, _paramSchemaFormat = Nothing, _paramSchemaItems = Nothing, _paramSchemaMaximum = Nothing, _paramSchemaExclusiveMaximum = Nothing, _paramSchemaMinimum = Nothing, _paramSchemaExclusiveMinimum = Nothing, _paramSchemaMaxLength = Nothing, _paramSchemaMinLength = Nothing, _paramSchemaPattern = Nothing, _paramSchemaMaxItems = Nothing, _paramSchemaMinItems = Nothing, _paramSchemaUniqueItems = Nothing, _paramSchemaEnum = Nothing, _paramSchemaMultipleOf = Nothing}})}),Inline (Param {_paramName = "status", _paramDescription = Just "Updated status of the pet", _paramRequired = Just False, _paramSchema = ParamOther (ParamOtherSchema {_paramOtherSchemaIn = ParamFormData, _paramOtherSchemaAllowEmptyValue = Nothing, _paramOtherSchemaParamSchema = ParamSchema {_paramSchemaDefault = Nothing, _paramSchemaType = SwaggerString, _paramSchemaFormat = Nothing, _paramSchemaItems = Nothing, _paramSchemaMaximum = Nothing, _paramSchemaExclusiveMaximum = Nothing, _paramSchemaMinimum = Nothing, _paramSchemaExclusiveMinimum = Nothing, _paramSchemaMaxLength = Nothing, _paramSchemaMinLength = Nothing, _paramSchemaPattern = Nothing, _paramSchemaMaxItems = Nothing, _paramSchemaMinItems = Nothing, _paramSchemaUniqueItems = Nothing, _paramSchemaEnum = Nothing, _paramSchemaMultipleOf = Nothing}})})], _operationResponses = Responses {_responsesDefault = Nothing, _responsesResponses = fromList [(405,Inline (Response {_responseDescription = "Invalid input", _responseSchema = Nothing, _responseHeaders = fromList [], _responseExamples = Nothing}))]}, _operationSchemes = Nothing, _operationDeprecated = Nothing, _operationSecurity = [SecurityRequirement {getSecurityRequirement = fromList [("petstore_auth",["write:pets","read:pets"])]}]}), _pathItemDelete = Just (Operation {_operationTags = fromList ["pet"], _operationSummary = Just "Deletes a pet", _operationDescription = Just "", _operationExternalDocs = Nothing, _operationOperationId = Just "deletePet", _operationConsumes = Nothing, _operationProduces = Just (MimeList {getMimeList = [application/xml,application/json]}), _operationParameters = [Inline (Param {_paramName = "api_key", _paramDescription = Nothing, _paramRequired = Just False, _paramSchema = ParamOther (ParamOtherSchema {_paramOtherSchemaIn = ParamHeader, _paramOtherSchemaAllowEmptyValue = Nothing, _paramOtherSchemaParamSchema = ParamSchema {_paramSchemaDefault = Nothing, _paramSchemaType = SwaggerString, _paramSchemaFormat = Nothing, _paramSchemaItems = Nothing, _paramSchemaMaximum = Nothing, _paramSchemaExclusiveMaximum = Nothing, _paramSchemaMinimum = Nothing, _paramSchemaExclusiveMinimum = Nothing, _paramSchemaMaxLength = Nothing, _paramSchemaMinLength = Nothing, _paramSchemaPattern = Nothing, _paramSchemaMaxItems = Nothing, _paramSchemaMinItems = Nothing, _paramSchemaUniqueItems = Nothing, _paramSchemaEnum = Nothing, _paramSchemaMultipleOf = Nothing}})}),Inline (Param {_paramName = "petId", _paramDescription = Just "Pet id to delete", _paramRequired = Just True, _paramSchema = ParamOther (ParamOtherSchema {_paramOtherSchemaIn = ParamPath, _paramOtherSchemaAllowEmptyValue = Nothing, _paramOtherSchemaParamSchema = ParamSchema {_paramSchemaDefault = Nothing, _paramSchemaType = SwaggerInteger, _paramSchemaFormat = Just "int64", _paramSchemaItems = Nothing, _paramSchemaMaximum = Nothing, _paramSchemaExclusiveMaximum = Nothing, _paramSchemaMinimum = Nothing, _paramSchemaExclusiveMinimum = Nothing, _paramSchemaMaxLength = Nothing, _paramSchemaMinLength = Nothing, _paramSchemaPattern = Nothing, _paramSchemaMaxItems = Nothing, _paramSchemaMinItems = Nothing, _paramSchemaUniqueItems = Nothing, _paramSchemaEnum = Nothing, _paramSchemaMultipleOf = Nothing}})})], _operationResponses = Responses {_responsesDefault = Nothing, _responsesResponses = fromList [(400,Inline (Response {_responseDescription = "Invalid ID supplied", _responseSchema = Nothing, _responseHeaders = fromList [], _responseExamples = Nothing})),(404,Inline (Response {_responseDescription = "Pet not found", _responseSchema = Nothing, _responseHeaders = fromList [], _responseExamples = Nothing}))]}, _operationSchemes = Nothing, _operationDeprecated = Nothing, _operationSecurity = [SecurityRequirement {getSecurityRequirement = fromList [("petstore_auth",["write:pets","read:pets"])]}]}), _pathItemOptions = Nothing, _pathItemHead = Nothing, _pathItemPatch = Nothing, _pathItemParameters = []})
--   ,("/user/createWithArray",PathItem {_pathItemGet = Nothing, _pathItemPut = Nothing, _pathItemPost = Just (Operation {_operationTags = fromList ["user"], _operationSummary = Just "Creates list of users with given input array", _operationDescription = Just "", _operationExternalDocs = Nothing, _operationOperationId = Just "createUsersWithArrayInput", _operationConsumes = Nothing, _operationProduces = Just (MimeList {getMimeList = [application/xml,application/json]}), _operationParameters = [Inline (Param {_paramName = "body", _paramDescription = Just "List of user object", _paramRequired = Just True, _paramSchema = ParamBody (Inline (Schema {_schemaTitle = Nothing, _schemaDescription = Nothing, _schemaRequired = [], _schemaAllOf = Nothing, _schemaProperties = fromList [], _schemaAdditionalProperties = Nothing, _schemaDiscriminator = Nothing, _schemaReadOnly = Nothing, _schemaXml = Nothing, _schemaExternalDocs = Nothing, _schemaExample = Nothing, _schemaMaxProperties = Nothing, _schemaMinProperties = Nothing, _schemaParamSchema = ParamSchema {_paramSchemaDefault = Nothing, _paramSchemaType = SwaggerArray, _paramSchemaFormat = Nothing, _paramSchemaItems = Just (SwaggerItemsObject (Ref (Reference {getReference = "User"}))), _paramSchemaMaximum = Nothing, _paramSchemaExclusiveMaximum = Nothing, _paramSchemaMinimum = Nothing, _paramSchemaExclusiveMinimum = Nothing, _paramSchemaMaxLength = Nothing, _paramSchemaMinLength = Nothing, _paramSchemaPattern = Nothing, _paramSchemaMaxItems = Nothing, _paramSchemaMinItems = Nothing, _paramSchemaUniqueItems = Nothing, _paramSchemaEnum = Nothing, _paramSchemaMultipleOf = Nothing}}))})], _operationResponses = Responses {_responsesDefault = Just (Inline (Response {_responseDescription = "successful operation", _responseSchema = Nothing, _responseHeaders = fromList [], _responseExamples = Nothing})), _responsesResponses = fromList []}, _operationSchemes = Nothing, _operationDeprecated = Nothing, _operationSecurity = []}), _pathItemDelete = Nothing, _pathItemOptions = Nothing, _pathItemHead = Nothing, _pathItemPatch = Nothing, _pathItemParameters = []})
--   ,("/pet",PathItem {_pathItemGet = Nothing, _pathItemPut = Just (Operation {_operationTags = fromList ["pet"], _operationSummary = Just "Update an existing pet", _operationDescription = Just "", _operationExternalDocs = Nothing, _operationOperationId = Just "updatePet", _operationConsumes = Just (MimeList {getMimeList = [application/json,application/xml]}), _operationProduces = Just (MimeList {getMimeList = [application/xml,application/json]}), _operationParameters = [Inline (Param {_paramName = "body", _paramDescription = Just "Pet object that needs to be added to the store", _paramRequired = Just True, _paramSchema = ParamBody (Ref (Reference {getReference = "Pet"}))})], _operationResponses = Responses {_responsesDefault = Nothing, _responsesResponses = fromList [(400,Inline (Response {_responseDescription = "Invalid ID supplied", _responseSchema = Nothing, _responseHeaders = fromList [], _responseExamples = Nothing})),(404,Inline (Response {_responseDescription = "Pet not found", _responseSchema = Nothing, _responseHeaders = fromList [], _responseExamples = Nothing})),(405,Inline (Response {_responseDescription = "Validation exception", _responseSchema = Nothing, _responseHeaders = fromList [], _responseExamples = Nothing}))]}, _operationSchemes = Nothing, _operationDeprecated = Nothing, _operationSecurity = [SecurityRequirement {getSecurityRequirement = fromList [("petstore_auth",["write:pets","read:pets"])]}]}), _pathItemPost = Just (Operation {_operationTags = fromList ["pet"], _operationSummary = Just "Add a new pet to the store", _operationDescription = Just "", _operationExternalDocs = Nothing, _operationOperationId = Just "addPet", _operationConsumes = Just (MimeList {getMimeList = [application/json,application/xml]}), _operationProduces = Just (MimeList {getMimeList = [application/xml,application/json]}), _operationParameters = [Inline (Param {_paramName = "body", _paramDescription = Just "Pet object that needs to be added to the store", _paramRequired = Just True, _paramSchema = ParamBody (Ref (Reference {getReference = "Pet"}))})], _operationResponses = Responses {_responsesDefault = Nothing, _responsesResponses = fromList [(405,Inline (Response {_responseDescription = "Invalid input", _responseSchema = Nothing, _responseHeaders = fromList [], _responseExamples = Nothing}))]}, _operationSchemes = Nothing, _operationDeprecated = Nothing, _operationSecurity = [SecurityRequirement {getSecurityRequirement = fromList [("petstore_auth",["write:pets","read:pets"])]}]}), _pathItemDelete = Nothing, _pathItemOptions = Nothing, _pathItemHead = Nothing, _pathItemPatch = Nothing, _pathItemParameters = []})
--   ,("/user/createWithList",PathItem {_pathItemGet = Nothing, _pathItemPut = Nothing, _pathItemPost = Just (Operation {_operationTags = fromList ["user"], _operationSummary = Just "Creates list of users with given input array", _operationDescription = Just "", _operationExternalDocs = Nothing, _operationOperationId = Just "createUsersWithListInput", _operationConsumes = Nothing, _operationProduces = Just (MimeList {getMimeList = [application/xml,application/json]}), _operationParameters = [Inline (Param {_paramName = "body", _paramDescription = Just "List of user object", _paramRequired = Just True, _paramSchema = ParamBody (Inline (Schema {_schemaTitle = Nothing, _schemaDescription = Nothing, _schemaRequired = [], _schemaAllOf = Nothing, _schemaProperties = fromList [], _schemaAdditionalProperties = Nothing, _schemaDiscriminator = Nothing, _schemaReadOnly = Nothing, _schemaXml = Nothing, _schemaExternalDocs = Nothing, _schemaExample = Nothing, _schemaMaxProperties = Nothing, _schemaMinProperties = Nothing, _schemaParamSchema = ParamSchema {_paramSchemaDefault = Nothing, _paramSchemaType = SwaggerArray, _paramSchemaFormat = Nothing, _paramSchemaItems = Just (SwaggerItemsObject (Ref (Reference {getReference = "User"}))), _paramSchemaMaximum = Nothing, _paramSchemaExclusiveMaximum = Nothing, _paramSchemaMinimum = Nothing, _paramSchemaExclusiveMinimum = Nothing, _paramSchemaMaxLength = Nothing, _paramSchemaMinLength = Nothing, _paramSchemaPattern = Nothing, _paramSchemaMaxItems = Nothing, _paramSchemaMinItems = Nothing, _paramSchemaUniqueItems = Nothing, _paramSchemaEnum = Nothing, _paramSchemaMultipleOf = Nothing}}))})], _operationResponses = Responses {_responsesDefault = Just (Inline (Response {_responseDescription = "successful operation", _responseSchema = Nothing, _responseHeaders = fromList [], _responseExamples = Nothing})), _responsesResponses = fromList []}, _operationSchemes = Nothing, _operationDeprecated = Nothing, _operationSecurity = []}), _pathItemDelete = Nothing, _pathItemOptions = Nothing, _pathItemHead = Nothing, _pathItemPatch = Nothing, _pathItemParameters = []})
--   ,("/user/{username}",PathItem {_pathItemGet = Just (Operation {_operationTags = fromList ["user"], _operationSummary = Just "Get user by user name", _operationDescription = Just "", _operationExternalDocs = Nothing, _operationOperationId = Just "getUserByName", _operationConsumes = Nothing, _operationProduces = Just (MimeList {getMimeList = [application/xml,application/json]}), _operationParameters = [Inline (Param {_paramName = "username", _paramDescription = Just "The name that needs to be fetched. Use user1 for testing. ", _paramRequired = Just True, _paramSchema = ParamOther (ParamOtherSchema {_paramOtherSchemaIn = ParamPath, _paramOtherSchemaAllowEmptyValue = Nothing, _paramOtherSchemaParamSchema = ParamSchema {_paramSchemaDefault = Nothing, _paramSchemaType = SwaggerString, _paramSchemaFormat = Nothing, _paramSchemaItems = Nothing, _paramSchemaMaximum = Nothing, _paramSchemaExclusiveMaximum = Nothing, _paramSchemaMinimum = Nothing, _paramSchemaExclusiveMinimum = Nothing, _paramSchemaMaxLength = Nothing, _paramSchemaMinLength = Nothing, _paramSchemaPattern = Nothing, _paramSchemaMaxItems = Nothing, _paramSchemaMinItems = Nothing, _paramSchemaUniqueItems = Nothing, _paramSchemaEnum = Nothing, _paramSchemaMultipleOf = Nothing}})})], _operationResponses = Responses {_responsesDefault = Nothing, _responsesResponses = fromList [(400,Inline (Response {_responseDescription = "Invalid username supplied", _responseSchema = Nothing, _responseHeaders = fromList [], _responseExamples = Nothing})),(404,Inline (Response {_responseDescription = "User not found", _responseSchema = Nothing, _responseHeaders = fromList [], _responseExamples = Nothing})),(200,Inline (Response {_responseDescription = "successful operation", _responseSchema = Just (Ref (Reference {getReference = "User"})), _responseHeaders = fromList [], _responseExamples = Nothing}))]}, _operationSchemes = Nothing, _operationDeprecated = Nothing, _operationSecurity = []}), _pathItemPut = Just (Operation {_operationTags = fromList ["user"], _operationSummary = Just "Updated user", _operationDescription = Just "This can only be done by the logged in user.", _operationExternalDocs = Nothing, _operationOperationId = Just "updateUser", _operationConsumes = Nothing, _operationProduces = Just (MimeList {getMimeList = [application/xml,application/json]}), _operationParameters = [Inline (Param {_paramName = "username", _paramDescription = Just "name that need to be updated", _paramRequired = Just True, _paramSchema = ParamOther (ParamOtherSchema {_paramOtherSchemaIn = ParamPath, _paramOtherSchemaAllowEmptyValue = Nothing, _paramOtherSchemaParamSchema = ParamSchema {_paramSchemaDefault = Nothing, _paramSchemaType = SwaggerString, _paramSchemaFormat = Nothing, _paramSchemaItems = Nothing, _paramSchemaMaximum = Nothing, _paramSchemaExclusiveMaximum = Nothing, _paramSchemaMinimum = Nothing, _paramSchemaExclusiveMinimum = Nothing, _paramSchemaMaxLength = Nothing, _paramSchemaMinLength = Nothing, _paramSchemaPattern = Nothing, _paramSchemaMaxItems = Nothing, _paramSchemaMinItems = Nothing, _paramSchemaUniqueItems = Nothing, _paramSchemaEnum = Nothing, _paramSchemaMultipleOf = Nothing}})}),Inline (Param {_paramName = "body", _paramDescription = Just "Updated user object", _paramRequired = Just True, _paramSchema = ParamBody (Ref (Reference {getReference = "User"}))})], _operationResponses = Responses {_responsesDefault = Nothing, _responsesResponses = fromList [(400,Inline (Response {_responseDescription = "Invalid user supplied", _responseSchema = Nothing, _responseHeaders = fromList [], _responseExamples = Nothing})),(404,Inline (Response {_responseDescription = "User not found", _responseSchema = Nothing, _responseHeaders = fromList [], _responseExamples = Nothing}))]}, _operationSchemes = Nothing, _operationDeprecated = Nothing, _operationSecurity = []}), _pathItemPost = Nothing, _pathItemDelete = Just (Operation {_operationTags = fromList ["user"], _operationSummary = Just "Delete user", _operationDescription = Just "This can only be done by the logged in user.", _operationExternalDocs = Nothing, _operationOperationId = Just "deleteUser", _operationConsumes = Nothing, _operationProduces = Just (MimeList {getMimeList = [application/xml,application/json]}), _operationParameters = [Inline (Param {_paramName = "username", _paramDescription = Just "The name that needs to be deleted", _paramRequired = Just True, _paramSchema = ParamOther (ParamOtherSchema {_paramOtherSchemaIn = ParamPath, _paramOtherSchemaAllowEmptyValue = Nothing, _paramOtherSchemaParamSchema = ParamSchema {_paramSchemaDefault = Nothing, _paramSchemaType = SwaggerString, _paramSchemaFormat = Nothing, _paramSchemaItems = Nothing, _paramSchemaMaximum = Nothing, _paramSchemaExclusiveMaximum = Nothing, _paramSchemaMinimum = Nothing, _paramSchemaExclusiveMinimum = Nothing, _paramSchemaMaxLength = Nothing, _paramSchemaMinLength = Nothing, _paramSchemaPattern = Nothing, _paramSchemaMaxItems = Nothing, _paramSchemaMinItems = Nothing, _paramSchemaUniqueItems = Nothing, _paramSchemaEnum = Nothing, _paramSchemaMultipleOf = Nothing}})})], _operationResponses = Responses {_responsesDefault = Nothing, _responsesResponses = fromList [(400,Inline (Response {_responseDescription = "Invalid username supplied", _responseSchema = Nothing, _responseHeaders = fromList [], _responseExamples = Nothing})),(404,Inline (Response {_responseDescription = "User not found", _responseSchema = Nothing, _responseHeaders = fromList [], _responseExamples = Nothing}))]}, _operationSchemes = Nothing, _operationDeprecated = Nothing, _operationSecurity = []}), _pathItemOptions = Nothing, _pathItemHead = Nothing, _pathItemPatch = Nothing, _pathItemParameters = []})], 
  -- _swaggerDefinitions = fromList 
  --   [("Tag",Schema {_schemaTitle = Nothing, _schemaDescription = Nothing, _schemaRequired = [], _schemaAllOf = Nothing, _schemaProperties = fromList [("name",Inline (Schema {_schemaTitle = Nothing, _schemaDescription = Nothing, _schemaRequired = [], _schemaAllOf = Nothing, _schemaProperties = fromList [], _schemaAdditionalProperties = Nothing, _schemaDiscriminator = Nothing, _schemaReadOnly = Nothing, _schemaXml = Nothing, _schemaExternalDocs = Nothing, _schemaExample = Nothing, _schemaMaxProperties = Nothing, _schemaMinProperties = Nothing, _schemaParamSchema = ParamSchema {_paramSchemaDefault = Nothing, _paramSchemaType = SwaggerString, _paramSchemaFormat = Nothing, _paramSchemaItems = Nothing, _paramSchemaMaximum = Nothing, _paramSchemaExclusiveMaximum = Nothing, _paramSchemaMinimum = Nothing, _paramSchemaExclusiveMinimum = Nothing, _paramSchemaMaxLength = Nothing, _paramSchemaMinLength = Nothing, _paramSchemaPattern = Nothing, _paramSchemaMaxItems = Nothing, _paramSchemaMinItems = Nothing, _paramSchemaUniqueItems = Nothing, _paramSchemaEnum = Nothing, _paramSchemaMultipleOf = Nothing}}))
  --   ,("id",Inline (Schema {_schemaTitle = Nothing, _schemaDescription = Nothing, _schemaRequired = [], _schemaAllOf = Nothing, _schemaProperties = fromList [], _schemaAdditionalProperties = Nothing, _schemaDiscriminator = Nothing, _schemaReadOnly = Nothing, _schemaXml = Nothing, _schemaExternalDocs = Nothing, _schemaExample = Nothing, _schemaMaxProperties = Nothing, _schemaMinProperties = Nothing, _schemaParamSchema = ParamSchema {_paramSchemaDefault = Nothing, _paramSchemaType = SwaggerInteger, _paramSchemaFormat = Just "int64", _paramSchemaItems = Nothing, _paramSchemaMaximum = Nothing, _paramSchemaExclusiveMaximum = Nothing, _paramSchemaMinimum = Nothing, _paramSchemaExclusiveMinimum = Nothing, _paramSchemaMaxLength = Nothing, _paramSchemaMinLength = Nothing, _paramSchemaPattern = Nothing, _paramSchemaMaxItems = Nothing, _paramSchemaMinItems = Nothing, _paramSchemaUniqueItems = Nothing, _paramSchemaEnum = Nothing, _paramSchemaMultipleOf = Nothing}}))], _schemaAdditionalProperties = Nothing, _schemaDiscriminator = Nothing, _schemaReadOnly = Nothing, _schemaXml = Just (Xml {_xmlName = Just "Tag", _xmlNamespace = Nothing, _xmlPrefix = Nothing, _xmlAttribute = Nothing, _xmlWrapped = Nothing}), _schemaExternalDocs = Nothing, _schemaExample = Nothing, _schemaMaxProperties = Nothing, _schemaMinProperties = Nothing, _schemaParamSchema = ParamSchema {_paramSchemaDefault = Nothing, _paramSchemaType = SwaggerObject, _paramSchemaFormat = Nothing, _paramSchemaItems = Nothing, _paramSchemaMaximum = Nothing, _paramSchemaExclusiveMaximum = Nothing, _paramSchemaMinimum = Nothing, _paramSchemaExclusiveMinimum = Nothing, _paramSchemaMaxLength = Nothing, _paramSchemaMinLength = Nothing, _paramSchemaPattern = Nothing, _paramSchemaMaxItems = Nothing, _paramSchemaMinItems = Nothing, _paramSchemaUniqueItems = Nothing, _paramSchemaEnum = Nothing, _paramSchemaMultipleOf = Nothing}})
  --   ,("Category",Schema {_schemaTitle = Nothing, _schemaDescription = Nothing, _schemaRequired = [], _schemaAllOf = Nothing, _schemaProperties = fromList [("name",Inline (Schema {_schemaTitle = Nothing, _schemaDescription = Nothing, _schemaRequired = [], _schemaAllOf = Nothing, _schemaProperties = fromList [], _schemaAdditionalProperties = Nothing, _schemaDiscriminator = Nothing, _schemaReadOnly = Nothing, _schemaXml = Nothing, _schemaExternalDocs = Nothing, _schemaExample = Nothing, _schemaMaxProperties = Nothing, _schemaMinProperties = Nothing, _schemaParamSchema = ParamSchema {_paramSchemaDefault = Nothing, _paramSchemaType = SwaggerString, _paramSchemaFormat = Nothing, _paramSchemaItems = Nothing, _paramSchemaMaximum = Nothing, _paramSchemaExclusiveMaximum = Nothing, _paramSchemaMinimum = Nothing, _paramSchemaExclusiveMinimum = Nothing, _paramSchemaMaxLength = Nothing, _paramSchemaMinLength = Nothing, _paramSchemaPattern = Nothing, _paramSchemaMaxItems = Nothing, _paramSchemaMinItems = Nothing, _paramSchemaUniqueItems = Nothing, _paramSchemaEnum = Nothing, _paramSchemaMultipleOf = Nothing}}))
  --   ,("id",Inline (Schema {_schemaTitle = Nothing, _schemaDescription = Nothing, _schemaRequired = [], _schemaAllOf = Nothing, _schemaProperties = fromList [], _schemaAdditionalProperties = Nothing, _schemaDiscriminator = Nothing, _schemaReadOnly = Nothing, _schemaXml = Nothing, _schemaExternalDocs = Nothing, _schemaExample = Nothing, _schemaMaxProperties = Nothing, _schemaMinProperties = Nothing, _schemaParamSchema = ParamSchema {_paramSchemaDefault = Nothing, _paramSchemaType = SwaggerInteger, _paramSchemaFormat = Just "int64", _paramSchemaItems = Nothing, _paramSchemaMaximum = Nothing, _paramSchemaExclusiveMaximum = Nothing, _paramSchemaMinimum = Nothing, _paramSchemaExclusiveMinimum = Nothing, _paramSchemaMaxLength = Nothing, _paramSchemaMinLength = Nothing, _paramSchemaPattern = Nothing, _paramSchemaMaxItems = Nothing, _paramSchemaMinItems = Nothing, _paramSchemaUniqueItems = Nothing, _paramSchemaEnum = Nothing, _paramSchemaMultipleOf = Nothing}}))], _schemaAdditionalProperties = Nothing, _schemaDiscriminator = Nothing, _schemaReadOnly = Nothing, _schemaXml = Just (Xml {_xmlName = Just "Category", _xmlNamespace = Nothing, _xmlPrefix = Nothing, _xmlAttribute = Nothing, _xmlWrapped = Nothing}), _schemaExternalDocs = Nothing, _schemaExample = Nothing, _schemaMaxProperties = Nothing, _schemaMinProperties = Nothing, _schemaParamSchema = ParamSchema {_paramSchemaDefault = Nothing, _paramSchemaType = SwaggerObject, _paramSchemaFormat = Nothing, _paramSchemaItems = Nothing, _paramSchemaMaximum = Nothing, _paramSchemaExclusiveMaximum = Nothing, _paramSchemaMinimum = Nothing, _paramSchemaExclusiveMinimum = Nothing, _paramSchemaMaxLength = Nothing, _paramSchemaMinLength = Nothing, _paramSchemaPattern = Nothing, _paramSchemaMaxItems = Nothing, _paramSchemaMinItems = Nothing, _paramSchemaUniqueItems = Nothing, _paramSchemaEnum = Nothing, _paramSchemaMultipleOf = Nothing}})
  --   ,("User",Schema {_schemaTitle = Nothing, _schemaDescription = Nothing, _schemaRequired = [], _schemaAllOf = Nothing, _schemaProperties = fromList [("email",Inline (Schema {_schemaTitle = Nothing, _schemaDescription = Nothing, _schemaRequired = [], _schemaAllOf = Nothing, _schemaProperties = fromList [], _schemaAdditionalProperties = Nothing, _schemaDiscriminator = Nothing, _schemaReadOnly = Nothing, _schemaXml = Nothing, _schemaExternalDocs = Nothing, _schemaExample = Nothing, _schemaMaxProperties = Nothing, _schemaMinProperties = Nothing, _schemaParamSchema = ParamSchema {_paramSchemaDefault = Nothing, _paramSchemaType = SwaggerString, _paramSchemaFormat = Nothing, _paramSchemaItems = Nothing, _paramSchemaMaximum = Nothing, _paramSchemaExclusiveMaximum = Nothing, _paramSchemaMinimum = Nothing, _paramSchemaExclusiveMinimum = Nothing, _paramSchemaMaxLength = Nothing, _paramSchemaMinLength = Nothing, _paramSchemaPattern = Nothing, _paramSchemaMaxItems = Nothing, _paramSchemaMinItems = Nothing, _paramSchemaUniqueItems = Nothing, _paramSchemaEnum = Nothing, _paramSchemaMultipleOf = Nothing}}))
  --   ,("phone",Inline (Schema {_schemaTitle = Nothing, _schemaDescription = Nothing, _schemaRequired = [], _schemaAllOf = Nothing, _schemaProperties = fromList [], _schemaAdditionalProperties = Nothing, _schemaDiscriminator = Nothing, _schemaReadOnly = Nothing, _schemaXml = Nothing, _schemaExternalDocs = Nothing, _schemaExample = Nothing, _schemaMaxProperties = Nothing, _schemaMinProperties = Nothing, _schemaParamSchema = ParamSchema {_paramSchemaDefault = Nothing, _paramSchemaType = SwaggerString, _paramSchemaFormat = Nothing, _paramSchemaItems = Nothing, _paramSchemaMaximum = Nothing, _paramSchemaExclusiveMaximum = Nothing, _paramSchemaMinimum = Nothing, _paramSchemaExclusiveMinimum = Nothing, _paramSchemaMaxLength = Nothing, _paramSchemaMinLength = Nothing, _paramSchemaPattern = Nothing, _paramSchemaMaxItems = Nothing, _paramSchemaMinItems = Nothing, _paramSchemaUniqueItems = Nothing, _paramSchemaEnum = Nothing, _paramSchemaMultipleOf = Nothing}}))
  --   ,("userStatus",Inline (Schema {_schemaTitle = Nothing, _schemaDescription = Just "User Status", _schemaRequired = [], _schemaAllOf = Nothing, _schemaProperties = fromList [], _schemaAdditionalProperties = Nothing, _schemaDiscriminator = Nothing, _schemaReadOnly = Nothing, _schemaXml = Nothing, _schemaExternalDocs = Nothing, _schemaExample = Nothing, _schemaMaxProperties = Nothing, _schemaMinProperties = Nothing, _schemaParamSchema = ParamSchema {_paramSchemaDefault = Nothing, _paramSchemaType = SwaggerInteger, _paramSchemaFormat = Just "int32", _paramSchemaItems = Nothing, _paramSchemaMaximum = Nothing, _paramSchemaExclusiveMaximum = Nothing, _paramSchemaMinimum = Nothing, _paramSchemaExclusiveMinimum = Nothing, _paramSchemaMaxLength = Nothing, _paramSchemaMinLength = Nothing, _paramSchemaPattern = Nothing, _paramSchemaMaxItems = Nothing, _paramSchemaMinItems = Nothing, _paramSchemaUniqueItems = Nothing, _paramSchemaEnum = Nothing, _paramSchemaMultipleOf = Nothing}}))
  --   ,("lastName",Inline (Schema 
  --                           _schemaTitle = Nothing, 
  --                           _schemaDescription = Nothing, 
  --                           _schemaRequired = [], 
  --                           _schemaAllOf = Nothing, 
  --                           _schemaProperties = fromList [], 
  --                           _schemaAdditionalProperties = Nothing, 
  --                           _schemaDiscriminator = Nothing, 
  --                           _schemaReadOnly = Nothing, 
  --                           _schemaXml = Nothing, 
  --                           _schemaExternalDocs = Nothing, 
  --                           _schemaExample = Nothing, 
  --                           _schemaMaxProperties = Nothing, 
  --                           _schemaMinProperties = Nothing, 
  --                           _schemaParamSchema = ParamSchema {
  --                               _paramSchemaDefault = Nothing, 
  --                               _paramSchemaType = SwaggerString, 
  --                               _paramSchemaFormat = Nothing, 
  --                               _paramSchemaItems = Nothing, 
  --                               _paramSchemaMaximum = Nothing, 
  --                               _paramSchemaExclusiveMaximum = Nothing, 
  --                               _paramSchemaMinimum = Nothing, 
  --                               _paramSchemaExclusiveMinimum = Nothing, 
  --                               _paramSchemaMaxLength = Nothing, 
  --                               _paramSchemaMinLength = Nothing, 
  --                               _paramSchemaPattern = Nothing, 
  --                               _paramSchemaMaxItems = Nothing, 
  --                               _paramSchemaMinItems = Nothing, _paramSchemaUniqueItems = Nothing, _paramSchemaEnum = Nothing, _paramSchemaMultipleOf = Nothing}}))
  --   ,("username",Inline (Schema {_schemaTitle = Nothing, _schemaDescription = Nothing, _schemaRequired = [], _schemaAllOf = Nothing, _schemaProperties = fromList [], _schemaAdditionalProperties = Nothing, _schemaDiscriminator = Nothing, _schemaReadOnly = Nothing, _schemaXml = Nothing, _schemaExternalDocs = Nothing, _schemaExample = Nothing, _schemaMaxProperties = Nothing, _schemaMinProperties = Nothing, _schemaParamSchema = ParamSchema {_paramSchemaDefault = Nothing, _paramSchemaType = SwaggerString, _paramSchemaFormat = Nothing, _paramSchemaItems = Nothing, _paramSchemaMaximum = Nothing, _paramSchemaExclusiveMaximum = Nothing, _paramSchemaMinimum = Nothing, _paramSchemaExclusiveMinimum = Nothing, _paramSchemaMaxLength = Nothing, _paramSchemaMinLength = Nothing, _paramSchemaPattern = Nothing, _paramSchemaMaxItems = Nothing, _paramSchemaMinItems = Nothing, _paramSchemaUniqueItems = Nothing, _paramSchemaEnum = Nothing, _paramSchemaMultipleOf = Nothing}}))
  --   ,("password",Inline (Schema {_schemaTitle = Nothing, _schemaDescription = Nothing, _schemaRequired = [], _schemaAllOf = Nothing, _schemaProperties = fromList [], _schemaAdditionalProperties = Nothing, _schemaDiscriminator = Nothing, _schemaReadOnly = Nothing, _schemaXml = Nothing, _schemaExternalDocs = Nothing, _schemaExample = Nothing, _schemaMaxProperties = Nothing, _schemaMinProperties = Nothing, _schemaParamSchema = ParamSchema {_paramSchemaDefault = Nothing, _paramSchemaType = SwaggerString, _paramSchemaFormat = Nothing, _paramSchemaItems = Nothing, _paramSchemaMaximum = Nothing, _paramSchemaExclusiveMaximum = Nothing, _paramSchemaMinimum = Nothing, _paramSchemaExclusiveMinimum = Nothing, _paramSchemaMaxLength = Nothing, _paramSchemaMinLength = Nothing, _paramSchemaPattern = Nothing, _paramSchemaMaxItems = Nothing, _paramSchemaMinItems = Nothing, _paramSchemaUniqueItems = Nothing, _paramSchemaEnum = Nothing, _paramSchemaMultipleOf = Nothing}}))
  --   ,("firstName",Inline (Schema {_schemaTitle = Nothing, _schemaDescription = Nothing, _schemaRequired = [], _schemaAllOf = Nothing, _schemaProperties = fromList [], _schemaAdditionalProperties = Nothing, _schemaDiscriminator = Nothing, _schemaReadOnly = Nothing, _schemaXml = Nothing, _schemaExternalDocs = Nothing, _schemaExample = Nothing, _schemaMaxProperties = Nothing, _schemaMinProperties = Nothing, _schemaParamSchema = ParamSchema {_paramSchemaDefault = Nothing, _paramSchemaType = SwaggerString, _paramSchemaFormat = Nothing, _paramSchemaItems = Nothing, _paramSchemaMaximum = Nothing, _paramSchemaExclusiveMaximum = Nothing, _paramSchemaMinimum = Nothing, _paramSchemaExclusiveMinimum = Nothing, _paramSchemaMaxLength = Nothing, _paramSchemaMinLength = Nothing, _paramSchemaPattern = Nothing, _paramSchemaMaxItems = Nothing, _paramSchemaMinItems = Nothing, _paramSchemaUniqueItems = Nothing, _paramSchemaEnum = Nothing, _paramSchemaMultipleOf = Nothing}}))
  --   ,("id",Inline (Schema {_schemaTitle = Nothing, _schemaDescription = Nothing, _schemaRequired = [], _schemaAllOf = Nothing, _schemaProperties = fromList [], _schemaAdditionalProperties = Nothing, _schemaDiscriminator = Nothing, _schemaReadOnly = Nothing, _schemaXml = Nothing, _schemaExternalDocs = Nothing, _schemaExample = Nothing, _schemaMaxProperties = Nothing, _schemaMinProperties = Nothing, _schemaParamSchema = ParamSchema {_paramSchemaDefault = Nothing, _paramSchemaType = SwaggerInteger, _paramSchemaFormat = Just "int64", _paramSchemaItems = Nothing, _paramSchemaMaximum = Nothing, _paramSchemaExclusiveMaximum = Nothing, _paramSchemaMinimum = Nothing, _paramSchemaExclusiveMinimum = Nothing, _paramSchemaMaxLength = Nothing, _paramSchemaMinLength = Nothing, _paramSchemaPattern = Nothing, _paramSchemaMaxItems = Nothing, _paramSchemaMinItems = Nothing, _paramSchemaUniqueItems = Nothing, _paramSchemaEnum = Nothing, _paramSchemaMultipleOf = Nothing}}))], _schemaAdditionalProperties = Nothing, _schemaDiscriminator = Nothing, _schemaReadOnly = Nothing, _schemaXml = Just (Xml {_xmlName = Just "User", _xmlNamespace = Nothing, _xmlPrefix = Nothing, _xmlAttribute = Nothing, _xmlWrapped = Nothing}), _schemaExternalDocs = Nothing, _schemaExample = Nothing, _schemaMaxProperties = Nothing, _schemaMinProperties = Nothing, _schemaParamSchema = ParamSchema {_paramSchemaDefault = Nothing, _paramSchemaType = SwaggerObject, _paramSchemaFormat = Nothing, _paramSchemaItems = Nothing, _paramSchemaMaximum = Nothing, _paramSchemaExclusiveMaximum = Nothing, _paramSchemaMinimum = Nothing, _paramSchemaExclusiveMinimum = Nothing, _paramSchemaMaxLength = Nothing, _paramSchemaMinLength = Nothing, _paramSchemaPattern = Nothing, _paramSchemaMaxItems = Nothing, _paramSchemaMinItems = Nothing, _paramSchemaUniqueItems = Nothing, _paramSchemaEnum = Nothing, _paramSchemaMultipleOf = Nothing}})
  --   ,("ApiResponse",Schema {_schemaTitle = Nothing, _schemaDescription = Nothing, _schemaRequired = [], _schemaAllOf = Nothing, _schemaProperties = fromList [("code",Inline (Schema {_schemaTitle = Nothing, _schemaDescription = Nothing, _schemaRequired = [], _schemaAllOf = Nothing, _schemaProperties = fromList [], _schemaAdditionalProperties = Nothing, _schemaDiscriminator = Nothing, _schemaReadOnly = Nothing, _schemaXml = Nothing, _schemaExternalDocs = Nothing, _schemaExample = Nothing, _schemaMaxProperties = Nothing, _schemaMinProperties = Nothing, _schemaParamSchema = ParamSchema {_paramSchemaDefault = Nothing, _paramSchemaType = SwaggerInteger, _paramSchemaFormat = Just "int32", _paramSchemaItems = Nothing, _paramSchemaMaximum = Nothing, _paramSchemaExclusiveMaximum = Nothing, _paramSchemaMinimum = Nothing, _paramSchemaExclusiveMinimum = Nothing, _paramSchemaMaxLength = Nothing, _paramSchemaMinLength = Nothing, _paramSchemaPattern = Nothing, _paramSchemaMaxItems = Nothing, _paramSchemaMinItems = Nothing, _paramSchemaUniqueItems = Nothing, _paramSchemaEnum = Nothing, _paramSchemaMultipleOf = Nothing}}))
  --   ,("type",Inline (Schema {_schemaTitle = Nothing, _schemaDescription = Nothing, _schemaRequired = [], _schemaAllOf = Nothing, _schemaProperties = fromList [], _schemaAdditionalProperties = Nothing, _schemaDiscriminator = Nothing, _schemaReadOnly = Nothing, _schemaXml = Nothing, _schemaExternalDocs = Nothing, _schemaExample = Nothing, _schemaMaxProperties = Nothing, _schemaMinProperties = Nothing, _schemaParamSchema = ParamSchema {_paramSchemaDefault = Nothing, _paramSchemaType = SwaggerString, _paramSchemaFormat = Nothing, _paramSchemaItems = Nothing, _paramSchemaMaximum = Nothing, _paramSchemaExclusiveMaximum = Nothing, _paramSchemaMinimum = Nothing, _paramSchemaExclusiveMinimum = Nothing, _paramSchemaMaxLength = Nothing, _paramSchemaMinLength = Nothing, _paramSchemaPattern = Nothing, _paramSchemaMaxItems = Nothing, _paramSchemaMinItems = Nothing, _paramSchemaUniqueItems = Nothing, _paramSchemaEnum = Nothing, _paramSchemaMultipleOf = Nothing}}))
  --   ,("message",Inline (Schema {_schemaTitle = Nothing, _schemaDescription = Nothing, _schemaRequired = [], _schemaAllOf = Nothing, _schemaProperties = fromList [], _schemaAdditionalProperties = Nothing, _schemaDiscriminator = Nothing, _schemaReadOnly = Nothing, _schemaXml = Nothing, _schemaExternalDocs = Nothing, _schemaExample = Nothing, _schemaMaxProperties = Nothing, _schemaMinProperties = Nothing, _schemaParamSchema = ParamSchema {_paramSchemaDefault = Nothing, _paramSchemaType = SwaggerString, _paramSchemaFormat = Nothing, _paramSchemaItems = Nothing, _paramSchemaMaximum = Nothing, _paramSchemaExclusiveMaximum = Nothing, _paramSchemaMinimum = Nothing, _paramSchemaExclusiveMinimum = Nothing, _paramSchemaMaxLength = Nothing, _paramSchemaMinLength = Nothing, _paramSchemaPattern = Nothing, _paramSchemaMaxItems = Nothing, _paramSchemaMinItems = Nothing, _paramSchemaUniqueItems = Nothing, _paramSchemaEnum = Nothing, _paramSchemaMultipleOf = Nothing}}))], _schemaAdditionalProperties = Nothing, _schemaDiscriminator = Nothing, _schemaReadOnly = Nothing, _schemaXml = Nothing, _schemaExternalDocs = Nothing, _schemaExample = Nothing, _schemaMaxProperties = Nothing, _schemaMinProperties = Nothing, _schemaParamSchema = ParamSchema {_paramSchemaDefault = Nothing, _paramSchemaType = SwaggerObject, _paramSchemaFormat = Nothing, _paramSchemaItems = Nothing, _paramSchemaMaximum = Nothing, _paramSchemaExclusiveMaximum = Nothing, _paramSchemaMinimum = Nothing, _paramSchemaExclusiveMinimum = Nothing, _paramSchemaMaxLength = Nothing, _paramSchemaMinLength = Nothing, _paramSchemaPattern = Nothing, _paramSchemaMaxItems = Nothing, _paramSchemaMinItems = Nothing, _paramSchemaUniqueItems = Nothing, _paramSchemaEnum = Nothing, _paramSchemaMultipleOf = Nothing}})
  --   ,("Order",Schema {_schemaTitle = Nothing, _schemaDescription = Nothing, _schemaRequired = [], _schemaAllOf = Nothing, _schemaProperties = fromList [("petId",Inline (Schema {_schemaTitle = Nothing, _schemaDescription = Nothing, _schemaRequired = [], _schemaAllOf = Nothing, _schemaProperties = fromList [], _schemaAdditionalProperties = Nothing, _schemaDiscriminator = Nothing, _schemaReadOnly = Nothing, _schemaXml = Nothing, _schemaExternalDocs = Nothing, _schemaExample = Nothing, _schemaMaxProperties = Nothing, _schemaMinProperties = Nothing, _schemaParamSchema = ParamSchema {_paramSchemaDefault = Nothing, _paramSchemaType = SwaggerInteger, _paramSchemaFormat = Just "int64", _paramSchemaItems = Nothing, _paramSchemaMaximum = Nothing, _paramSchemaExclusiveMaximum = Nothing, _paramSchemaMinimum = Nothing, _paramSchemaExclusiveMinimum = Nothing, _paramSchemaMaxLength = Nothing, _paramSchemaMinLength = Nothing, _paramSchemaPattern = Nothing, _paramSchemaMaxItems = Nothing, _paramSchemaMinItems = Nothing, _paramSchemaUniqueItems = Nothing, _paramSchemaEnum = Nothing, _paramSchemaMultipleOf = Nothing}}))
  --   ,("status",Inline (Schema {_schemaTitle = Nothing, _schemaDescription = Just "Order Status", _schemaRequired = [], _schemaAllOf = Nothing, _schemaProperties = fromList [], _schemaAdditionalProperties = Nothing, _schemaDiscriminator = Nothing, _schemaReadOnly = Nothing, _schemaXml = Nothing, _schemaExternalDocs = Nothing, _schemaExample = Nothing, _schemaMaxProperties = Nothing, _schemaMinProperties = Nothing, _schemaParamSchema = ParamSchema {_paramSchemaDefault = Nothing, _paramSchemaType = SwaggerString, _paramSchemaFormat = Nothing, _paramSchemaItems = Nothing, _paramSchemaMaximum = Nothing, _paramSchemaExclusiveMaximum = Nothing, _paramSchemaMinimum = Nothing, _paramSchemaExclusiveMinimum = Nothing, _paramSchemaMaxLength = Nothing, _paramSchemaMinLength = Nothing, _paramSchemaPattern = Nothing, _paramSchemaMaxItems = Nothing, _paramSchemaMinItems = Nothing, _paramSchemaUniqueItems = Nothing, _paramSchemaEnum = Just [String "placed",String "approved",String "delivered"], _paramSchemaMultipleOf = Nothing}}))
  --   ,("quantity",Inline (Schema {_schemaTitle = Nothing, _schemaDescription = Nothing, _schemaRequired = [], _schemaAllOf = Nothing, _schemaProperties = fromList [], _schemaAdditionalProperties = Nothing, _schemaDiscriminator = Nothing, _schemaReadOnly = Nothing, _schemaXml = Nothing, _schemaExternalDocs = Nothing, _schemaExample = Nothing, _schemaMaxProperties = Nothing, _schemaMinProperties = Nothing, _schemaParamSchema = ParamSchema {_paramSchemaDefault = Nothing, _paramSchemaType = SwaggerInteger, _paramSchemaFormat = Just "int32", _paramSchemaItems = Nothing, _paramSchemaMaximum = Nothing, _paramSchemaExclusiveMaximum = Nothing, _paramSchemaMinimum = Nothing, _paramSchemaExclusiveMinimum = Nothing, _paramSchemaMaxLength = Nothing, _paramSchemaMinLength = Nothing, _paramSchemaPattern = Nothing, _paramSchemaMaxItems = Nothing, _paramSchemaMinItems = Nothing, _paramSchemaUniqueItems = Nothing, _paramSchemaEnum = Nothing, _paramSchemaMultipleOf = Nothing}}))
  --   ,("id",Inline (Schema {_schemaTitle = Nothing, _schemaDescription = Nothing, _schemaRequired = [], _schemaAllOf = Nothing, _schemaProperties = fromList [], _schemaAdditionalProperties = Nothing, _schemaDiscriminator = Nothing, _schemaReadOnly = Nothing, _schemaXml = Nothing, _schemaExternalDocs = Nothing, _schemaExample = Nothing, _schemaMaxProperties = Nothing, _schemaMinProperties = Nothing, _schemaParamSchema = ParamSchema {_paramSchemaDefault = Nothing, _paramSchemaType = SwaggerInteger, _paramSchemaFormat = Just "int64", _paramSchemaItems = Nothing, _paramSchemaMaximum = Nothing, _paramSchemaExclusiveMaximum = Nothing, _paramSchemaMinimum = Nothing, _paramSchemaExclusiveMinimum = Nothing, _paramSchemaMaxLength = Nothing, _paramSchemaMinLength = Nothing, _paramSchemaPattern = Nothing, _paramSchemaMaxItems = Nothing, _paramSchemaMinItems = Nothing, _paramSchemaUniqueItems = Nothing, _paramSchemaEnum = Nothing, _paramSchemaMultipleOf = Nothing}}))
  --   ,("shipDate",Inline (Schema {_schemaTitle = Nothing, _schemaDescription = Nothing, _schemaRequired = [], _schemaAllOf = Nothing, _schemaProperties = fromList [], _schemaAdditionalProperties = Nothing, _schemaDiscriminator = Nothing, _schemaReadOnly = Nothing, _schemaXml = Nothing, _schemaExternalDocs = Nothing, _schemaExample = Nothing, _schemaMaxProperties = Nothing, _schemaMinProperties = Nothing, _schemaParamSchema = ParamSchema {_paramSchemaDefault = Nothing, _paramSchemaType = SwaggerString, _paramSchemaFormat = Just "date-time", _paramSchemaItems = Nothing, _paramSchemaMaximum = Nothing, _paramSchemaExclusiveMaximum = Nothing, _paramSchemaMinimum = Nothing, _paramSchemaExclusiveMinimum = Nothing, _paramSchemaMaxLength = Nothing, _paramSchemaMinLength = Nothing, _paramSchemaPattern = Nothing, _paramSchemaMaxItems = Nothing, _paramSchemaMinItems = Nothing, _paramSchemaUniqueItems = Nothing, _paramSchemaEnum = Nothing, _paramSchemaMultipleOf = Nothing}}))
  --   ,("complete",Inline (Schema {_schemaTitle = Nothing, _schemaDescription = Nothing, _schemaRequired = [], _schemaAllOf = Nothing, _schemaProperties = fromList [], _schemaAdditionalProperties = Nothing, _schemaDiscriminator = Nothing, _schemaReadOnly = Nothing, _schemaXml = Nothing, _schemaExternalDocs = Nothing, _schemaExample = Nothing, _schemaMaxProperties = Nothing, _schemaMinProperties = Nothing, _schemaParamSchema = ParamSchema {_paramSchemaDefault = Just (Bool False), _paramSchemaType = SwaggerBoolean, _paramSchemaFormat = Nothing, _paramSchemaItems = Nothing, _paramSchemaMaximum = Nothing, _paramSchemaExclusiveMaximum = Nothing, _paramSchemaMinimum = Nothing, _paramSchemaExclusiveMinimum = Nothing, _paramSchemaMaxLength = Nothing, _paramSchemaMinLength = Nothing, _paramSchemaPattern = Nothing, _paramSchemaMaxItems = Nothing, _paramSchemaMinItems = Nothing, _paramSchemaUniqueItems = Nothing, _paramSchemaEnum = Nothing, _paramSchemaMultipleOf = Nothing}}))], _schemaAdditionalProperties = Nothing, _schemaDiscriminator = Nothing, _schemaReadOnly = Nothing, _schemaXml = Just (Xml {_xmlName = Just "Order", _xmlNamespace = Nothing, _xmlPrefix = Nothing, _xmlAttribute = Nothing, _xmlWrapped = Nothing}), _schemaExternalDocs = Nothing, _schemaExample = Nothing, _schemaMaxProperties = Nothing, _schemaMinProperties = Nothing, _schemaParamSchema = ParamSchema {_paramSchemaDefault = Nothing, _paramSchemaType = SwaggerObject, _paramSchemaFormat = Nothing, _paramSchemaItems = Nothing, _paramSchemaMaximum = Nothing, _paramSchemaExclusiveMaximum = Nothing, _paramSchemaMinimum = Nothing, _paramSchemaExclusiveMinimum = Nothing, _paramSchemaMaxLength = Nothing, _paramSchemaMinLength = Nothing, _paramSchemaPattern = Nothing, _paramSchemaMaxItems = Nothing, _paramSchemaMinItems = Nothing, _paramSchemaUniqueItems = Nothing, _paramSchemaEnum = Nothing, _paramSchemaMultipleOf = Nothing}}),("Pet",Schema {_schemaTitle = Nothing, _schemaDescription = Nothing, _schemaRequired = ["name","photoUrls"], _schemaAllOf = Nothing, _schemaProperties = fromList [("photoUrls",Inline (Schema {_schemaTitle = Nothing, _schemaDescription = Nothing, _schemaRequired = [], _schemaAllOf = Nothing, _schemaProperties = fromList [], _schemaAdditionalProperties = Nothing, _schemaDiscriminator = Nothing, _schemaReadOnly = Nothing, _schemaXml = Just (Xml {_xmlName = Just "photoUrl", _xmlNamespace = Nothing, _xmlPrefix = Nothing, _xmlAttribute = Nothing, _xmlWrapped = Just True}), _schemaExternalDocs = Nothing, _schemaExample = Nothing, _schemaMaxProperties = Nothing, _schemaMinProperties = Nothing, _schemaParamSchema = ParamSchema {_paramSchemaDefault = Nothing, _paramSchemaType = SwaggerArray, _paramSchemaFormat = Nothing, _paramSchemaItems = Just (SwaggerItemsObject (Inline (Schema {_schemaTitle = Nothing, _schemaDescription = Nothing, _schemaRequired = [], _schemaAllOf = Nothing, _schemaProperties = fromList [], _schemaAdditionalProperties = Nothing, _schemaDiscriminator = Nothing, _schemaReadOnly = Nothing, _schemaXml = Nothing, _schemaExternalDocs = Nothing, _schemaExample = Nothing, _schemaMaxProperties = Nothing, _schemaMinProperties = Nothing, _schemaParamSchema = ParamSchema {_paramSchemaDefault = Nothing, _paramSchemaType = SwaggerString, _paramSchemaFormat = Nothing, _paramSchemaItems = Nothing, _paramSchemaMaximum = Nothing, _paramSchemaExclusiveMaximum = Nothing, _paramSchemaMinimum = Nothing, _paramSchemaExclusiveMinimum = Nothing, _paramSchemaMaxLength = Nothing, _paramSchemaMinLength = Nothing, _paramSchemaPattern = Nothing, _paramSchemaMaxItems = Nothing, _paramSchemaMinItems = Nothing, _paramSchemaUniqueItems = Nothing, _paramSchemaEnum = Nothing, _paramSchemaMultipleOf = Nothing}}))), _paramSchemaMaximum = Nothing, _paramSchemaExclusiveMaximum = Nothing, _paramSchemaMinimum = Nothing, _paramSchemaExclusiveMinimum = Nothing, _paramSchemaMaxLength = Nothing, _paramSchemaMinLength = Nothing, _paramSchemaPattern = Nothing, _paramSchemaMaxItems = Nothing, _paramSchemaMinItems = Nothing, _paramSchemaUniqueItems = Nothing, _paramSchemaEnum = Nothing, _paramSchemaMultipleOf = Nothing}})),("status",Inline (Schema {_schemaTitle = Nothing, _schemaDescription = Just "pet status in the store", _schemaRequired = [], _schemaAllOf = Nothing, _schemaProperties = fromList [], _schemaAdditionalProperties = Nothing, _schemaDiscriminator = Nothing, _schemaReadOnly = Nothing, _schemaXml = Nothing, _schemaExternalDocs = Nothing, _schemaExample = Nothing, _schemaMaxProperties = Nothing, _schemaMinProperties = Nothing, _schemaParamSchema = ParamSchema {_paramSchemaDefault = Nothing, _paramSchemaType = SwaggerString, _paramSchemaFormat = Nothing, _paramSchemaItems = Nothing, _paramSchemaMaximum = Nothing, _paramSchemaExclusiveMaximum = Nothing, _paramSchemaMinimum = Nothing, _paramSchemaExclusiveMinimum = Nothing, _paramSchemaMaxLength = Nothing, _paramSchemaMinLength = Nothing, _paramSchemaPattern = Nothing, _paramSchemaMaxItems = Nothing, _paramSchemaMinItems = Nothing, _paramSchemaUniqueItems = Nothing, _paramSchemaEnum = Just [String "available",String "pending",String "sold"], _paramSchemaMultipleOf = Nothing}})),("category",Ref (Reference {getReference = "Category"})),("name",Inline (Schema {_schemaTitle = Nothing, _schemaDescription = Nothing, _schemaRequired = [], _schemaAllOf = Nothing, _schemaProperties = fromList [], _schemaAdditionalProperties = Nothing, _schemaDiscriminator = Nothing, _schemaReadOnly = Nothing, _schemaXml = Nothing, _schemaExternalDocs = Nothing, _schemaExample = Just (String "doggie"), _schemaMaxProperties = Nothing, _schemaMinProperties = Nothing, _schemaParamSchema = ParamSchema {_paramSchemaDefault = Nothing, _paramSchemaType = SwaggerString, _paramSchemaFormat = Nothing, _paramSchemaItems = Nothing, _paramSchemaMaximum = Nothing, _paramSchemaExclusiveMaximum = Nothing, _paramSchemaMinimum = Nothing, _paramSchemaExclusiveMinimum = Nothing, _paramSchemaMaxLength = Nothing, _paramSchemaMinLength = Nothing, _paramSchemaPattern = Nothing, _paramSchemaMaxItems = Nothing, _paramSchemaMinItems = Nothing, _paramSchemaUniqueItems = Nothing, _paramSchemaEnum = Nothing, _paramSchemaMultipleOf = Nothing}})),("id",Inline (Schema {_schemaTitle = Nothing, _schemaDescription = Nothing, _schemaRequired = [], _schemaAllOf = Nothing, _schemaProperties = fromList [], _schemaAdditionalProperties = Nothing, _schemaDiscriminator = Nothing, _schemaReadOnly = Nothing, _schemaXml = Nothing, _schemaExternalDocs = Nothing, _schemaExample = Nothing, _schemaMaxProperties = Nothing, _schemaMinProperties = Nothing, _schemaParamSchema = ParamSchema {_paramSchemaDefault = Nothing, _paramSchemaType = SwaggerInteger, _paramSchemaFormat = Just "int64", _paramSchemaItems = Nothing, _paramSchemaMaximum = Nothing, _paramSchemaExclusiveMaximum = Nothing, _paramSchemaMinimum = Nothing, _paramSchemaExclusiveMinimum = Nothing, _paramSchemaMaxLength = Nothing, _paramSchemaMinLength = Nothing, _paramSchemaPattern = Nothing, _paramSchemaMaxItems = Nothing, _paramSchemaMinItems = Nothing, _paramSchemaUniqueItems = Nothing, _paramSchemaEnum = Nothing, _paramSchemaMultipleOf = Nothing}})),("tags",Inline (Schema {_schemaTitle = Nothing, _schemaDescription = Nothing, _schemaRequired = [], _schemaAllOf = Nothing, _schemaProperties = fromList [], _schemaAdditionalProperties = Nothing, _schemaDiscriminator = Nothing, _schemaReadOnly = Nothing, _schemaXml = Just (Xml {_xmlName = Just "tag", _xmlNamespace = Nothing, _xmlPrefix = Nothing, _xmlAttribute = Nothing, _xmlWrapped = Just True}), _schemaExternalDocs = Nothing, _schemaExample = Nothing, _schemaMaxProperties = Nothing, _schemaMinProperties = Nothing, _schemaParamSchema = ParamSchema {_paramSchemaDefault = Nothing, _paramSchemaType = SwaggerArray, _paramSchemaFormat = Nothing, _paramSchemaItems = Just (SwaggerItemsObject (Ref (Reference {getReference = "Tag"}))), _paramSchemaMaximum = Nothing, _paramSchemaExclusiveMaximum = Nothing, _paramSchemaMinimum = Nothing, _paramSchemaExclusiveMinimum = Nothing, _paramSchemaMaxLength = Nothing, _paramSchemaMinLength = Nothing, _paramSchemaPattern = Nothing, _paramSchemaMaxItems = Nothing, _paramSchemaMinItems = Nothing, _paramSchemaUniqueItems = Nothing, _paramSchemaEnum = Nothing, _paramSchemaMultipleOf = Nothing}}))], _schemaAdditionalProperties = Nothing, _schemaDiscriminator = Nothing, _schemaReadOnly = Nothing, _schemaXml = Just (Xml {_xmlName = Just "Pet", _xmlNamespace = Nothing, _xmlPrefix = Nothing, _xmlAttribute = Nothing, _xmlWrapped = Nothing}), _schemaExternalDocs = Nothing, _schemaExample = Nothing, _schemaMaxProperties = Nothing, _schemaMinProperties = Nothing, _schemaParamSchema = ParamSchema {_paramSchemaDefault = Nothing, _paramSchemaType = SwaggerObject, _paramSchemaFormat = Nothing, _paramSchemaItems = Nothing, _paramSchemaMaximum = Nothing, _paramSchemaExclusiveMaximum = Nothing, _paramSchemaMinimum = Nothing, _paramSchemaExclusiveMinimum = Nothing, _paramSchemaMaxLength = Nothing, _paramSchemaMinLength = Nothing, _paramSchemaPattern = Nothing, _paramSchemaMaxItems = Nothing, _paramSchemaMinItems = Nothing, _paramSchemaUniqueItems = Nothing, _paramSchemaEnum = Nothing, _paramSchemaMultipleOf = Nothing}})], 
  -- _swaggerParameters = fromList [], 
  -- _swaggerResponses = fromList [], 
  -- _swaggerSecurityDefinitions = fromList [("api_key",SecurityScheme {_securitySchemeType = SecuritySchemeApiKey (ApiKeyParams {_apiKeyName = "api_key", _apiKeyIn = ApiKeyHeader}), _securitySchemeDescription = Nothing}),("petstore_auth",SecurityScheme {_securitySchemeType = SecuritySchemeOAuth2 (OAuth2Params {_oauth2Flow = OAuth2Implicit "http://petstore.swagger.io/oauth/dialog", _oauth2Scopes = fromList [("write:pets","modify pets in your account"),("read:pets","read your pets")]}), _securitySchemeDescription = Nothing})], 
  -- _swaggerSecurity = [], 
  -- _swaggerTags = fromList [Tag {_tagName = "pet", _tagDescription = Just "Everything about your Pets", _tagExternalDocs = Just (ExternalDocs {_externalDocsDescription = Just "Find out more", _externalDocsUrl = URL {getUrl = "http://swagger.io"}})},Tag {_tagName = "store", _tagDescription = Just "Access to Petstore orders", _tagExternalDocs = Nothing},Tag {_tagName = "user", _tagDescription = Just "Operations about user", _tagExternalDocs = Just (ExternalDocs {_externalDocsDescription = Just "Find out more about our store", _externalDocsUrl = URL {getUrl = "http://swagger.io"}})}], 
  -- _swaggerExternalDocs = Just (ExternalDocs {_externalDocsDescription = Just "Find out more about Swagger", _externalDocsUrl = URL {getUrl = "http://swagger.io"}})}
