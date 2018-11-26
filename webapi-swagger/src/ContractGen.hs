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
import Language.Haskell.Exts as LHE hiding (OPTIONS)
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

import Data.Swagger hiding (get)
import Data.Swagger.Declare
-- import Data.Swagger.Lens
import Data.Swagger.Operation

import Debug.Trace as DT


runDefaultPathCodeGen :: IO ()
runDefaultPathCodeGen = runCodeGen "webapi-swagger/sampleFiles/swagger-petstore-ex.json" "webapi-swagger/src/"


runCodeGen :: FilePath -> FilePath -> IO () 
runCodeGen swaggerJsonInputFilePath contractOutputFolderPath = do
  newTypeCreationList <- execStateT (readSwaggerGenerateDefnModels swaggerJsonInputFilePath contractOutputFolderPath)  [] 
  createNewTypes newTypeCreationList
 where
  createNewTypes typeList = do
    let hTypes = DL.foldl' createType ([]::[Decl SrcSpanInfo]) typeList
    appendFile (contractOutputFolderPath ++ "Types.hs") $ DL.unlines $ fmap prettyPrint hTypes
      
  createType accValue typeInfo = 
    case typeInfo of
      ProductType newData -> do
        let toParamInstances =
              case (DL.isInfixOf "QueryParam" $ mName newData) of
                True -> [defaultToParamInstance (mName newData) "QueryParam"] 
                False -> 
                  case (DL.isInfixOf "FormParam" $ mName newData) of
                    True -> [defaultToParamInstance (mName newData) "FormParam"]
                    False -> []
        accValue ++ [dataDeclaration (DataType noSrcSpan) (mName newData) (mRecordTypes newData) ["Eq", "Show", "Generic"] ] ++ (jsonInstances (mName newData) ) ++ toParamInstances ++ [defaultToSchemaInstance (mName newData)]
      SumType tName tConstructors -> do
        let toParamEncodeParamQueryParamInstance = [toParamQueryParamInstance tName] ++ [encodeParamSumTypeInstance tName (DL.zip tConstructors ( (fmap . fmap) Char.toLower tConstructors) ) ]
        let fromParamDecodeParamQueryParamInstance = [fromParamQueryParamInstance tName] ++ [decodeParamSumTypeInstance tName (DL.zip ((fmap . fmap) Char.toLower tConstructors) tConstructors ) ]
        let toSchemaInstance = [toSchemaInstanceForSumType tName (DL.zip ((fmap . fmap) Char.toLower tConstructors) tConstructors ) ]
        accValue ++ ([sumTypeDeclaration tName tConstructors ["Eq", "Generic", "Ord"] ] ++ (instanceDeclForShow tName) ++ (instanceDeclForJSONForSumType tName) ++ toParamEncodeParamQueryParamInstance ++ fromParamDecodeParamQueryParamInstance ++ toSchemaInstance)



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
              (fmap (\modName -> moduleImport (modName,(False, Nothing)) ) [ "WebApi.Contract", "WebApi.Param", "Types", "Data.Int", "Data.Text"]) -- CommonTypes
              (generateContractBody "Petstore" contractDetailsFromPetstore)
      liftIO $ writeFile (contractOutputFolderPath ++ "Contract.hs") $ prettyPrint hContractModule
      let qualifiedImportsForTypes = [("Data.ByteString.Char8", (True, Just $ ModuleName noSrcSpan "ASCII"))]
      let hTypesModule = 
            Module noSrcSpan 
                (Just $ ModuleHead noSrcSpan (ModuleName noSrcSpan "Types") Nothing Nothing)
                (fmap languageExtension ["TypeFamilies", "MultiParamTypeClasses", "DeriveGeneric", "TypeOperators", "DataKinds", "TypeSynonymInstances", "FlexibleInstances", "DuplicateRecordFields", "OverloadedStrings"])
                (fmap (moduleImport) ( (DL.zip ["Data.Text","Data.Int","Data.Time.Clock", "GHC.Generics", "Data.Aeson", "WebApi.Param", "Data.Text.Encoding", "Data.Swagger.Schema"] (cycle [(False, Nothing)]) ) ++ qualifiedImportsForTypes ) ) --"GHC.Generics", "Data.Time.Calendar"
                (createDataDeclarations newData)
      liftIO $ writeFile (contractOutputFolderPath ++ "Types.hs") $ prettyPrint hTypesModule ++ "\n\n"
    
 where 
  createDataDeclarations :: [NewData] -> [Decl SrcSpanInfo]
  createDataDeclarations newDataList = DL.foldl' (\accValue newDataInfo -> 
      accValue ++ [(dataDeclaration (DataType noSrcSpan) (mName newDataInfo) (mRecordTypes newDataInfo) ["Eq", "Show", "Generic"])] ++ jsonInstances (mName newDataInfo) ++ [defaultToSchemaInstance (mName newDataInfo)] ) [] newDataList
 
-- TODO: This function assumes SwaggerObject to be the type and directly reads from schemaProperties. We need to also take additionalProperties into consideration.
generateSwaggerDefinitionData :: InsOrdHashMap Text Schema -> StateT [CreateNewType] IO [NewData]
generateSwaggerDefinitionData defDataHM = foldlWithKey' parseSwaggerDefinition (pure []) defDataHM
 where 
  parseSwaggerDefinition :: StateT [CreateNewType] IO [NewData] -> Text -> Schema -> StateT [CreateNewType] IO [NewData]
  parseSwaggerDefinition scAccValue modelName modelSchema = do
    accValue <- scAccValue
    let (schemaProperties::InsOrdHashMap Text (Referenced Schema) ) = _schemaProperties modelSchema
    let mandatoryFields = fmap toS (_schemaRequired modelSchema)
    recordNamesAndTypes <- foldlWithKey' (\scAccList innerRecord iRefSchema -> do 
            accList <- scAccList
            let innerRecordName = toS innerRecord
            let innerRecordTypeName = toS $ T.append (T.toTitle modelName) (T.toTitle innerRecord)
            innerRecordType <- case iRefSchema of
                    Ref referenceName -> pure $ toS $ getReference referenceName
                    Inline irSchema -> ((getTypeFromSwaggerType (Just innerRecordTypeName) (Just irSchema)) . _schemaParamSchema) irSchema
            let recordTypeWithMaybe = 
                  case (innerRecordName `DL.elem` mandatoryFields) of 
                    True -> innerRecordType
                    False -> "Maybe " ++ innerRecordType
            pure $ (innerRecordName, recordTypeWithMaybe):accList ) (pure []) schemaProperties
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
        let mContentTypes = 
              case apiOutType of
                "()" -> Just "'[PlainText]"
                "Text" -> Just "'[PlainText]"
                _ -> Nothing
        -- Group the Referenced Params by ParamLocation and then go through each group separately.
        let (formParamList, queryParamList, fileParamList, headerInList, bodyParamList) = DL.foldl' (groupParamTypes) ([], [], [], [], []) (_operationParameters operationData)
        mFormParamType <- getParamTypes (currentRouteName ++ show stdMethod) formParamList FormParam
        mQueryParamType <- getParamTypes (currentRouteName ++ show stdMethod) queryParamList QueryParam
        mFileParamType <- getParamTypes (currentRouteName ++ show stdMethod) fileParamList FileParam
        mHeaderInType <- getParamTypes (currentRouteName ++ show stdMethod) headerInList HeaderParam
        mReqBodyType <- getParamTypes (currentRouteName ++ show stdMethod) bodyParamList BodyParam
        let finalReqBodyType = flip fmap mReqBodyType (\reqBodyType -> 
              case (DL.isPrefixOf "[" reqBodyType) of
                True -> "'" ++ reqBodyType
                False -> "'[" ++ reqBodyType ++ "]" )
        pure $ Map.insert stdMethod (ApiTypeDetails apiOutType apiErrType mFormParamType mQueryParamType mFileParamType mHeaderInType finalReqBodyType mContentTypes) methodAcc
      Nothing -> pure methodAcc

  groupParamTypes :: ([Param], [Param], [Param], [Param], [Param]) -> Referenced Param -> ([Param], [Param], [Param], [Param], [Param])
  groupParamTypes (formParamList, queryParamList, fileParamList, headerInList, bodyParamList) refParam = 
    case refParam of
      Ref someRef -> error $ "Encountered a Referenced type at the first level of a reference param! Not sure what the use case of this is or how to process it! Debug Info : " ++ show refParam
      Inline param -> 
        case _paramSchema param of
          ParamBody _ -> (formParamList, queryParamList, fileParamList, headerInList, param:bodyParamList)
          ParamOther pOtherSchema -> 
            case _paramOtherSchemaIn pOtherSchema of 
              ParamQuery -> (formParamList, param:queryParamList, fileParamList, headerInList, bodyParamList) 
              ParamHeader -> (formParamList, queryParamList, fileParamList, param:headerInList, bodyParamList)
              ParamPath -> (formParamList, queryParamList, fileParamList, headerInList, bodyParamList) 
              ParamFormData ->
                case (_paramSchema param) of
                  -- ParamBody _ -> (formParamList, queryParamList, fileParamList, headerInList) -- this line seems to be unreachable code.
                  ParamOther pSchema -> 
                    case (_paramSchemaType $ _paramOtherSchemaParamSchema pSchema) of
                      SwaggerFile -> (formParamList, queryParamList, param:fileParamList, headerInList, bodyParamList) 
                      _ -> (param:formParamList, queryParamList, fileParamList, headerInList, bodyParamList) 


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
      _ -> -- TODO : Refactor handling of adding Maybes and adding to State into a single function and call from all places.
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
            typeListWithIsMandatory <- forM paramList (\param -> do
                    hType <- getParamTypeParam param (Just $ toS ( _paramName param) ) Nothing
                    pure (isMandatory param, hType) )
            let finalHaskellTypes = fmap (\(isMandatoryType, hType) -> (addMaybeToType isMandatoryType hType) ) typeListWithIsMandatory
            case finalHaskellTypes of
              [] -> pure Nothing
              x:[] -> pure $ Just x
              x:xs -> error "Handle case of multiple Header Params!"
          FileParam -> do
            typeList <- forM paramList (\param -> getParamTypeParam param (Just $ toS ( _paramName param) ) Nothing )
            case typeList of
              [] -> pure Nothing
              x:[] -> pure $ Just x
              x:xs -> error "Handle case of list of FileParam"
          BodyParam -> do
            listOfTypes <- forM paramList (\param -> getParamTypeParam param (Just $ toS (_paramName param)) Nothing  )
            case listOfTypes of
              [] -> error $ "Tried to Get Body Param type but got an empty list/string! Debug Info: " ++ show paramList
              x:[] -> pure $ Just x
              x:xs -> error $ "Encountered a list of Body Params. WebApi does not support this currently! Debug Info: " ++ show paramList
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
getTypeFromSwaggerType mParamNameOrRecordName mOuterSchema paramSchema = 
    case (_paramSchemaType paramSchema) of 
      SwaggerString -> 
        case _paramSchemaFormat paramSchema of
          Just "date" -> pure "Day"
          Just "date-time" -> pure "UTCTime"
          Just "password" -> error $ "Encountered SwaggerString with Format as `password`. This needs to be handled! Debug Info : " ++ show paramSchema
          Just "byte" -> pure "ByteString"
          Just "binary" -> error $ "Encountered SwaggerString with Format as `binary`. This needs to be handled! Debug Info: " ++ show paramSchema
          Nothing -> 
            case _paramSchemaEnum paramSchema of
              Nothing -> pure "Text"
              Just valueEnumList -> do
                let enumVals = fmap (\(Data.Aeson.String enumVal) -> toS $ T.toTitle enumVal ) valueEnumList
                let innerRecordTypeName = fromJustNote ("Expected a Param Name but got Nothing. Need Param Name to set the name for the new type we need to create. Debug Info: " ++ show paramSchema) mParamNameOrRecordName
                let haskellNewTypeInfo = SumType innerRecordTypeName enumVals
                currentState <- get
                let onlySumTypes = DL.filter (\newTypeObj -> 
                          case newTypeObj of 
                            SumType _ _-> True
                            _ -> False ) currentState
                let (createNewType, newOrExistingName) = DL.foldl' (checkIfSumTypeExists haskellNewTypeInfo) (False, innerRecordTypeName) onlySumTypes
                case createNewType of
                  True -> do
                    modify' (\existingState -> haskellNewTypeInfo:existingState)
                    pure innerRecordTypeName
                  False -> pure newOrExistingName
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
                                let paramName = fromJustNote ("Expected a Param Name but got Nothing. Need Param Name to set the name for the new type we need to create. Debug Info: " ++ show paramSchema) mParamNameOrRecordName
                                let titleCaseParamName = toS $ T.toTitle $ toS paramName
                                case _paramSchemaEnum innerParamSchema of
                                  Just enumVals -> do
                                    let enumValList::[String] = fmap (\(Data.Aeson.String val) -> toS $ T.toTitle val ) enumVals
                                    let haskellNewTypeInfo = SumType titleCaseParamName enumValList
                                    modify'(\existingState -> haskellNewTypeInfo:existingState)
                                    pure titleCaseParamName
                                  Nothing ->  getTypeFromSwaggerType Nothing Nothing innerParamSchema
                          case mCollectionFormat of
                            (Just CollectionMulti) -> pure $ "MultiSet " ++ typeName 
                            (Just CollectionTSV) -> pure $ "DelimitedCollection \"\t\" " ++ typeName
                            (Just CollectionSSV) -> pure $ "DelimitedCollection \" \"" ++ typeName
                            (Just CollectionPipes) -> pure $ "DelimitedCollection \"|\"" ++ typeName
                            -- Since CSV is the default, the below case takes care of (Just CSV) as well as Nothing
                            _ -> pure $ "DelimitedCollection \",\" " ++ typeName
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
  checkIfSumTypeExists (SumType newTypeName newTypeVals) (createType, newOrExistingTypeName) (SumType typeName tVals) = 
    case (newTypeVals == tVals) of 
      True -> (False, typeName)
      False -> 
        case (newTypeVals `DL.intersect` tVals) of
          [] -> (True, newTypeName)
          _ -> error $ "A new sum type would be created with elements already existing in other sum types! This needs to be handled! Debug Info: Type1 -> " ++ show (SumType newTypeName newTypeVals) ++ " Type2 (added to state first) -> " ++ show (SumType typeName tVals)



data ParamType = FormParam 
               | QueryParam
               | FileParam
               | HeaderParam
               | BodyParam
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
  , requestBody :: Maybe String
  , contentTypes :: Maybe String
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
   



parseHaskellSrcContract :: String -> IO ()
parseHaskellSrcContract pathToFile = do
  parseResult <- parseFile pathToFile 
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
        requestBodyType = requestBody apiDetails
        contentType = contentTypes apiDetails
        instanceVectorList = catMaybes $ fmap (\(typeInfo, typeLabel) -> fmap (\tInfo -> fromMaybeSV $ SV.fromList [typeLabel, show currentMethod, routeName, tInfo] ) typeInfo) $ DL.zip (respType:errType:formParamType:queryParamType:fileParamType:headerParamType:requestBodyType:contentType:[]) ["ApiOut", "ApiErr","FormParam", "QueryParam", "FileParam", "HeaderIn", "RequestBody", "ContentTypes"]
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
dataDeclaration :: (DataOrNew SrcSpanInfo) -> String -> InnerRecords -> [DerivingClass] -> Decl SrcSpanInfo
dataDeclaration dataOrNew dataName innerRecords derivingList = 
    DataDecl noSrcSpan  
      dataOrNew 
      Nothing 
      (declarationHead dataName)
      (constructorDeclaration dataName innerRecords)
      [derivingDecl  derivingList]
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
stringLiteral str = (Lit noSrcSpan (LHE.String noSrcSpan str str))

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
moduleImport :: (String, (Bool, Maybe (ModuleName SrcSpanInfo)) )-> ImportDecl SrcSpanInfo
moduleImport (moduleName, (isQualified, qualifiedName) ) = 
  ImportDecl {
              importAnn = noSrcSpan, 
              importModule = ModuleName noSrcSpan moduleName,
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


-- Show Instance for Enum Type 
instanceDeclForShow :: String -> [Decl SrcSpanInfo]
instanceDeclForShow dataTypeName =
  [InstDecl noSrcSpan Nothing 
    (IRule noSrcSpan Nothing Nothing 
      (IHApp noSrcSpan 
        (instanceHead "Show") 
        (typeConstructor dataTypeName)
      )
    )
    (Just 
      [InsDecl noSrcSpan 
        (FunBind noSrcSpan 
          [Match noSrcSpan (Ident noSrcSpan "show") 
            [PVar noSrcSpan (Ident noSrcSpan "st")] 
            (UnGuardedRhs noSrcSpan (InfixApp noSrcSpan (Var noSrcSpan (Qual noSrcSpan (ModuleName noSrcSpan "ASCII") (nameDecl "unpack"))) 
              (QVarOp noSrcSpan (unQualSymDecl "$") ) 
              (App noSrcSpan 
                (variableName "encodeParam")
                (variableName "st")
              ))) Nothing])]) ]

-- Instances for ToJSON and FromJSON For Sum Types
instanceDeclForJSONForSumType :: String -> [Decl SrcSpanInfo]
instanceDeclForJSONForSumType dataTypeName = [toJsonInstance, fromJsonInstance]
 where 
  toJsonInstance = 
    InstDecl noSrcSpan Nothing 
      (IRule noSrcSpan Nothing Nothing 
        (IHApp noSrcSpan 
          (instanceHead "ToJSON") 
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
          (instanceHead "FromJSON") 
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
                          (PApp noSrcSpan (UnQual noSrcSpan (nameDecl "Just")) [PVar noSrcSpan (nameDecl "x")]) 
                          (UnGuardedRhs noSrcSpan 
                            (App noSrcSpan (variableName "pure") (variableName "x") )) 
                          Nothing
                        ,Alt noSrcSpan 
                          (PApp noSrcSpan (UnQual noSrcSpan (nameDecl "Nothing")) []) 
                          (UnGuardedRhs noSrcSpan (App noSrcSpan (variableName "error") (stringLiteral "Failed while parsing Status value from JSON")))
                          Nothing ]
                      )))) (variableName"jsonVal") )) Nothing])])


jsonInstances :: String -> [Decl SrcSpanInfo]
jsonInstances dataTypeName = [jsonInstance "ToJSON", jsonInstance "FromJSON"]
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
    (PLit noSrcSpan (Signless noSrcSpan) (LHE.String noSrcSpan caseMatchOnStr caseMatchOnStr ) ) 
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


defaultToParamInstance :: String -> String -> Decl SrcSpanInfo
defaultToParamInstance dataTypeName paramType =
  InstDecl noSrcSpan Nothing 
    (IRule noSrcSpan Nothing Nothing 
      (IHApp noSrcSpan 
        (IHApp noSrcSpan 
          (instanceHead "ToParam") 
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

toSchemaInstanceForSumType :: String -> [(String, String)] -> Decl SrcSpanInfo
toSchemaInstanceForSumType typeName constructorValues = 
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
                    (Var noSrcSpan (Qual noSrcSpan (ModuleName noSrcSpan "Prelude") (nameDecl "id")))) 
                    (Paren noSrcSpan 
                      (Lambda noSrcSpan [PVar noSrcSpan (nameDecl "inputConst")] 
                        (Case noSrcSpan 
                          (Var noSrcSpan (UnQual noSrcSpan (nameDecl "inputConst"))) 
                          (fmap caseMatchStatement constructorValues)
                        )
                      )
                    )
                ) 
                (Var noSrcSpan (Qual noSrcSpan (ModuleName noSrcSpan "Prelude") (nameDecl "id")))
              ) 
              (dataConstructor "True")
          ) 
          (dataConstructor "False")
        )
      )
    )
  ) 
  Nothing)])
 where
  caseMatchStatement (lowerCaseCons, typeConstructor) = 
    (Alt noSrcSpan 
      (PLit noSrcSpan (Signless noSrcSpan) (LHE.String noSrcSpan typeConstructor typeConstructor)) 
      (UnGuardedRhs noSrcSpan (stringLiteral lowerCaseCons) ) Nothing)
    -- Alt noSrcSpan 
    --   (PLit noSrcSpan (Signless noSrcSpan) (LHE.String noSrcSpan "Pending" "Pending")) 
    --   (UnGuardedRhs noSrcSpan (stringLiteral "pending") ) Nothing,
    -- Alt noSrcSpan 
    --   (PLit noSrcSpan (Signless noSrcSpan) (LHE.String noSrcSpan "Sold" "Sold")) 
    --   (UnGuardedRhs noSrcSpan (stringLiteral "sold") ) Nothing
    -- ]
---------------------------------------------------------------------------------------
-- Support multiple versions of GHC (Use ifndef )
-- for LTS 9.0 -> 1.18.2






