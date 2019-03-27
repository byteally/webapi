{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes #-}




module ContractGen where

import Data.Aeson 
import Data.Text as T
import qualified Data.ByteString.Lazy as BSL
import Data.HashMap.Strict.InsOrd as HMSIns
import Language.Haskell.Exts as LHE hiding (OPTIONS)
import Data.Vector.Sized as SV hiding ((++), foldM, forM, mapM)
import Safe
import Data.Finite.Internal
import Network.HTTP.Types.Method
import Data.Maybe
import Data.List.Split as DLS (splitOn)
import qualified Data.List as DL
import qualified Data.Map.Lazy as Map
import qualified Data.Char as Char
import Control.Monad
import Control.Monad.Trans.State.Strict
import Control.Monad.IO.Class
import System.Directory
import Data.String.Interpolate
import Data.Swagger hiding (get, paramSchema)
import Data.Yaml (decodeEither')
import Control.Applicative ((<|>))


runDefaultPathCodeGen :: IO ()
runDefaultPathCodeGen = runCodeGen "sampleFiles/swagger-petstore-noXml.json" "/Users/kahlil/projects/ByteAlly/tmp/" "swagger-gen-proj"


runCodeGen :: FilePath -> FilePath -> String -> IO () 
runCodeGen swaggerJsonInputFilePath outputPath projectName = do
  let projectFolderGenPath = outputPath ++ projectName ++ "/"
  createDirectoryIfMissing True (projectFolderGenPath ++ "src/")
  newTypeCreationList <- execStateT (readSwaggerGenerateDefnModels swaggerJsonInputFilePath projectFolderGenPath projectName)  [] 
  createNewTypes newTypeCreationList projectFolderGenPath
  writeFile (projectFolderGenPath ++ "src/CommonTypes.hs") commonTypesModuleContent
  
 where
  createNewTypes ::[CreateNewType] -> FilePath -> IO ()
  createNewTypes typeList genPath = do
    let hTypes = DL.foldl' createType ([]::[Decl SrcSpanInfo]) typeList
    appendFile (genPath ++ "src/Types.hs") $ 
      (DL.unlines $ fmap prettyPrint hTypes )
      
  createType :: [Decl SrcSpanInfo] -> CreateNewType -> [Decl SrcSpanInfo]
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
        let (modifiedRecords, dataDecl) = dataDeclaration (DataType noSrcSpan) (mName newData) (mRecordTypes newData) ["Eq", "Show", "Generic"] 
        let jsonInsts = jsonInstances (mName newData) modifiedRecords
        accValue ++ [dataDecl] ++ jsonInsts ++ toParamInstances ++ [defaultToSchemaInstance (mName newData)]
      SumType tName tConstructors ogConstructors -> do
        let toParamEncodeParamQueryParamInstance = [toParamQueryParamInstance tName] ++ [encodeParamSumTypeInstance tName (DL.zip tConstructors ogConstructors ) ]
        let fromParamDecodeParamQueryParamInstance = [fromParamQueryParamInstance tName] ++ [decodeParamSumTypeInstance tName (DL.zip ogConstructors tConstructors ) ]
        let toSchemaInstances = toSchemaInstanceForSumType tName (DL.zip ogConstructors tConstructors ) 
        accValue ++ 
          ([sumTypeDeclaration tName tConstructors ["Eq", "Generic", "Ord"] ] 
            ++ (instanceDeclForShow tName) 
            ++ (instanceDeclForJSONForSumType tName) 
            ++ toParamEncodeParamQueryParamInstance 
            ++ fromParamDecodeParamQueryParamInstance 
            ++ toSchemaInstances)
      TypeAlias _ _ -> error $ "Encountered TypeAlias in State contents (while generating new data types). "
        ++ "\nThis may not be an error. We need to verify if it's a valid case for State to contain type aliases."
        ++ "\nThe type to be created is  : " ++ (show typeInfo)

  commonTypesModuleContent :: String
  commonTypesModuleContent = [i|
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module CommonTypes where

import Data.ByteString as BS
import qualified Data.ByteString.Char8 as ASCII
import qualified Data.HashMap.Lazy as HM
import WebApi.Param
import qualified Data.Vector as V

import qualified Data.Swagger as SW
import Data.Swagger.Internal.Schema as SW
import Data.Swagger.ParamSchema
import Control.Lens hiding (List)
import Data.Swagger.Internal hiding (Tag, CollectionFormat)

import Data.Text
import Data.Aeson
import GHC.Generics

data CollectionFormat = CSV | SSV | TSV | Pipes 

--change name to CollectionFormat
-- remove all qualifiers (WebApi.)
newtype Collection (format :: CollectionFormat) (t :: *)  = Collection { getCollection :: V.Vector t}
    deriving (Eq, Show)
-- write ToParam instance for Collection


instance (EncodeParam (Collection format t) ) => ToParam 'QueryParam (Collection format t) where
  toParam _ pfx val = [(pfx, Just $ encodeParam val)]

instance (EncodeParam t) => EncodeParam (Collection 'CSV t) where
  encodeParam (Collection innerVector) =  BS.intercalate "," $ fmap (\\singleVal -> encodeParam singleVal ) (V.toList innerVector)


instance (EncodeParam t) => EncodeParam (Collection 'SSV t) where
  encodeParam (Collection innerVector) =  BS.intercalate " " $ fmap (\\singleVal -> encodeParam singleVal ) (V.toList innerVector)

instance (EncodeParam t) => EncodeParam (Collection 'TSV t) where
  encodeParam (Collection innerVector) =  BS.intercalate "\\t" $ fmap (\\singleVal -> encodeParam singleVal ) (V.toList innerVector)

instance (EncodeParam t) => EncodeParam (Collection 'Pipes t) where
  encodeParam (Collection innerVector) =  BS.intercalate "|" $ fmap (\\singleVal -> encodeParam singleVal ) (V.toList innerVector)


instance (DecodeParam (Collection format t) ) => FromParam 'QueryParam (Collection format t) where
  fromParam pt key kvs = case lookupParam pt key kvs of
    Just (Just par) -> case decodeParam par of
          Just v -> Validation $ Right v
          _      -> Validation $ Left [ParseErr key "Unable to cast to Collection"]
    _ ->  Validation $ Left [NotFound key]


instance (DecodeParam t) => DecodeParam (Collection 'CSV t) where
  decodeParam str = 
    case sequenceA $ fmap decodeParam (ASCII.split ',' str) of
      Just parsedList -> Just $ Collection $ V.fromList $ parsedList
      Nothing -> Nothing
    

instance (DecodeParam t) => DecodeParam (Collection 'SSV t) where
  decodeParam str = 
    case sequenceA $ fmap decodeParam (ASCII.split ' ' str) of
      Just parsedList -> Just $ Collection $ V.fromList $ parsedList
      Nothing -> Nothing

    
instance (DecodeParam t) => DecodeParam (Collection 'TSV t) where
  decodeParam str = 
    case sequenceA $ fmap decodeParam (ASCII.split '\\t' str) of
      Just parsedList -> Just $ Collection $ V.fromList $ parsedList
      Nothing -> Nothing


instance (DecodeParam t) => DecodeParam (Collection 'Pipes t) where
  decodeParam str = 
    case sequenceA $ fmap decodeParam (ASCII.split '|' str) of
      Just parsedList -> Just $ Collection $ V.fromList $ parsedList
      Nothing -> Nothing


keyMapping :: HM.HashMap String String -> String -> String
keyMapping hMap k = 
  case HM.lookup k hMap of
    Just foundVal -> foundVal
    Nothing -> k


instance ToSchema (MultiSet Text) where
  declareNamedSchema = plain . paramSchemaToSchema


instance ToParamSchema (MultiSet Text) where
  toParamSchema _ = mempty
      & SW.type_ .~ SwaggerArray
      & SW.items ?~ SwaggerItemsPrimitive Nothing (mempty & SW.type_ .~ SwaggerString)  
      

data SwaggerNumber = IntegerFormat Integer | DoubleFormat Double
  deriving (Eq, Show, Generic)

instance ToJSON SwaggerNumber
instance FromJSON SwaggerNumber
instance ToSchema SwaggerNumber
instance ToParam 'QueryParam SwaggerNumber

instance DecodeParam SwaggerNumber where
  decodeParam bs =
    case decodeParam bs :: Maybe Integer of 
      Just x -> Just $ IntegerFormat x
      Nothing -> 
        case decodeParam bs :: Maybe Double of
          Just y -> Just $ DoubleFormat y
          Nothing -> error $ "Expected SwaggerNumber to coerce into either Integer or Double!"
            ++ "\\nInput param is : " ++ (show bs)
      |]

type StateConfig = StateT [CreateNewType] IO ()

readSwaggerGenerateDefnModels :: FilePath -> FilePath -> String -> StateConfig
readSwaggerGenerateDefnModels swaggerJsonInputFilePath contractOutputFolderPath projectName = do 
  swaggerJSONContents <- liftIO $ BSL.readFile swaggerJsonInputFilePath
  let decodedVal = eitherDecode swaggerJSONContents <|> either (Left . show) Right (decodeEither' (BSL.toStrict swaggerJSONContents))
  case decodedVal of
    Left errMsg -> error $ errMsg -- "Panic: not a valid JSON or yaml"
    Right (swaggerData :: Swagger) -> do
      (apiNameHs, contractDetails) <- getSwaggerData swaggerData
      let xmlImport = needsXmlImport contractDetails      
      newData <- generateSwaggerDefinitionData (_swaggerDefinitions swaggerData) 
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
              ((fmap (\modName -> moduleImport (modName,(False, Nothing)) ) contractImports) -- CommonTypes
                ++ fmap moduleImport qualifiedImportsForContract)
              (generateContractBody apiNameHs contractDetails)
      liftIO $ writeFile (contractOutputFolderPath ++ "src/Contract.hs") $ prettyPrint hContractModule
      let qualifiedImportsForTypes = 
            [("Data.ByteString.Char8", (True, Just $ ModuleName noSrcSpan "ASCII")), 
            ("Data.HashMap.Lazy", (True, Just $ ModuleName noSrcSpan "HM") ),
            ("Data.Swagger", (True, Just $ ModuleName noSrcSpan "SW") ) ]
      let hTypesModule = 
            Module noSrcSpan 
                (Just $ ModuleHead noSrcSpan (ModuleName noSrcSpan "Types") Nothing Nothing)
                (fmap languageExtension ["TypeFamilies", "MultiParamTypeClasses", "DeriveGeneric", "TypeOperators", "DataKinds", "TypeSynonymInstances", "FlexibleInstances", "DuplicateRecordFields", "OverloadedStrings"])
                (fmap moduleImport 
                  ( (DL.zip ["Data.Text",
                            "Data.Int",
                            "Data.Time.Clock", 
                            "GHC.Generics", 
                            "Data.Aeson", 
                            "WebApi.Param", 
                            "Data.Text.Encoding", 
                            "Data.Swagger.Schema", 
                            "CommonTypes",
                            "Control.Lens",
                            "Data.Swagger.Internal.Schema",
                            "Data.Swagger.ParamSchema",
                            -- TODO : This is kind of a hack!
                            "Data.Swagger.Internal hiding (Tag)"
                            ] (cycle [(False, Nothing)]) ) ++ qualifiedImportsForTypes ) ) --"GHC.Generics", "Data.Time.Calendar"
                (createDataDeclarations newData)
      liftIO $ do
        writeFile (contractOutputFolderPath ++ "src/Types.hs") $ prettyPrint hTypesModule ++ "\n\n"
        writeCabalAndProjectFiles contractOutputFolderPath projectName xmlImport
    
 where 
  createDataDeclarations :: [CreateNewType] -> [Decl SrcSpanInfo]
  createDataDeclarations newDataList = DL.foldl' (\accValue cNewTy -> 
    case cNewTy of
      ProductType newDataInfo -> 
        let (modifiedRecords, dataDecl) = dataDeclaration (DataType noSrcSpan) (mName newDataInfo) (mRecordTypes newDataInfo) ["Eq", "Show", "Generic"]
            jsonInsts = jsonInstances (mName newDataInfo) modifiedRecords
        in accValue ++ [dataDecl] ++ jsonInsts ++ [defaultToSchemaInstance (mName newDataInfo)] 
      TypeAlias tName alias -> (typeAliasForDecl tName alias):accValue 
      SumType _ _ _ -> error $ "Encountered a Sum Type creation while constructing initial types for Types.hs "
        ++ "\n The value is : " ++ (show cNewTy)   ) [] newDataList
      
  
  needsXmlImport :: [ContractDetails] -> Bool
  needsXmlImport = flip DL.foldl' False (\accBool cDetail -> 
              case accBool of
                True -> True
                False ->
                  let methodMap = methodData cDetail
                  in Map.foldl' (\innerAcc apiDetails -> hasXML apiDetails || innerAcc) accBool methodMap )
 
-- TODO: This function assumes SwaggerObject to be the type and directly reads from schemaProperties. We need to also take additionalProperties into consideration.
generateSwaggerDefinitionData :: InsOrdHashMap Text Schema -> StateT [CreateNewType] IO [CreateNewType]
generateSwaggerDefinitionData defDataHM = foldlWithKey' parseSwaggerDefinition (pure []) defDataHM
 where 
  parseSwaggerDefinition :: StateT [CreateNewType] IO [CreateNewType] -> Text -> Schema -> StateT [CreateNewType] IO [CreateNewType]
  parseSwaggerDefinition scAccValue modelName modelSchema = do 
    accValue <- scAccValue 
    let (schemaProperties::InsOrdHashMap Text (Referenced Schema) ) = _schemaProperties modelSchema
    case HMSIns.null schemaProperties of
      True -> do
        hsType <- getTypeFromSwaggerType (Just $ T.unpack modelName) (Just modelSchema) (_schemaParamSchema modelSchema)
        if hsType == (T.unpack modelName)
          -- If the name of the type returned is the same, it would mean that it's a sum type. 
          -- An alias is not necessary here as the sum type details would be stored in the State
          -- And the type will be generated later when the State value is read.
        then pure accValue
        else pure $ TypeAlias (T.unpack modelName) hsType:accValue
      False -> do
        let mandatoryFields = fmap T.unpack (_schemaRequired modelSchema)
        recordNamesAndTypes <- foldlWithKey' (\scAccList innerRecord iRefSchema -> do 
                accList <- scAccList
                let innerRecordName = T.unpack innerRecord
                let innerRecordTypeName = T.unpack $ T.append (T.toTitle modelName) (T.toTitle innerRecord)
                innerRecordType <- case iRefSchema of
                        Ref referenceName -> pure $ T.unpack $ getReference referenceName
                        Inline irSchema -> ((getTypeFromSwaggerType (Just innerRecordTypeName) (Just irSchema)) . _schemaParamSchema) irSchema
                let recordTypeWithMaybe = 
                      case (innerRecordName `DL.elem` mandatoryFields) of 
                        True -> innerRecordType
                        False -> "Maybe " ++ innerRecordType
                pure $ (innerRecordName, recordTypeWithMaybe):accList ) (pure []) schemaProperties
        let prodType = (ProductType $ NewData (T.unpack modelName) recordNamesAndTypes  )
        pure $ prodType:accValue



getSwaggerData :: Swagger -> StateT [CreateNewType] IO (String, [ContractDetails])
getSwaggerData swaggerData = do
 let apiNameFromSwagger = (_infoTitle . _swaggerInfo) swaggerData 
     validHsApiName = setValidConstructorId (T.unpack apiNameFromSwagger)
 contractDetailList <- HMSIns.foldlWithKey' (parseSwaggerPaths swaggerData) (pure []) (_swaggerPaths swaggerData)
 pure (validHsApiName, contractDetailList)

 where
  parseSwaggerPaths :: Swagger -> StateT [CreateNewType] IO [ContractDetails] -> FilePath -> PathItem -> StateT [CreateNewType] IO [ContractDetails]
  parseSwaggerPaths swaggerData contractDetailsList swFilePath swPathDetails = do
    let (refParamsHM:: InsOrdHashMap Text Param) = _swaggerParameters swaggerData
    cDetailsList <- contractDetailsList
    let currentRouteId = 
          case cDetailsList of 
              [] -> 1
              _ -> (routeId $ Prelude.head cDetailsList) + 1 -- TODO: Not being used anymore. Should be removed
        swaggerPath ::[SwPathComponent] = fmap constructSwPathComps $ DLS.splitOn "/" $ removeLeadingSlash swFilePath
        mainRouteName = setValidConstructorId $ (prettifyRouteName swaggerPath) ++ "R"

    -- TODO: Add a `Static` Type component at the start if the list has just one element of type PathComp
    finalPathWithParamTypes::[PathComponent] <- forM swaggerPath (\pathComponent -> 
        case pathComponent of 
          PathParamName pathParamName -> do
            (mParamNameList::[Maybe String]) <- mapM (getPathParamTypeFromOperation pathParamName refParamsHM) (getListOfPathOperations swPathDetails::[Maybe Operation])
            case (DL.nub . catMaybes) mParamNameList of
              [] -> error "TODO : Please report this as a bug. Need to handle the use of Common Params!"
              singleParamType:[] -> pure (PathParamType singleParamType)
              -- TODO : If the below case is encountered we need to handle it. (add separate Routes!)
              otherVal -> error $ "Expected only a single Param Type to be present in all Methods of this path."
                ++ "Instead got : " ++ show otherVal ++ " Path : " ++ swFilePath
          PathPiece staticPathCompStr -> pure (PathComp staticPathCompStr)
          )
         
    let currentRoutePath = finalPathWithParamTypes
        methodList = [GET, PUT, POST, PATCH, DELETE, OPTIONS, HEAD]
    currentMethodData <- Control.Monad.foldM (processPathItem mainRouteName swPathDetails swaggerData) (Map.empty) methodList
    let currentContractDetails = ContractDetails currentRouteId mainRouteName currentRoutePath currentMethodData
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

  getListOfPathOperations :: PathItem -> [Maybe Operation]
  getListOfPathOperations pathItem = [_pathItemGet pathItem, _pathItemPut pathItem, _pathItemPost pathItem, _pathItemDelete pathItem, _pathItemOptions pathItem, _pathItemHead pathItem, _pathItemPatch pathItem]
  
  getPathParamTypeFromOperation :: String -> InsOrdHashMap Text Param -> Maybe Operation -> StateT [CreateNewType] IO (Maybe String)
  getPathParamTypeFromOperation paramPathName refParamsHM mOperation = case mOperation of
    Just operation -> do 
      let paramList = _operationParameters operation
      mParamType <- foldM (\existingParamType refOrInlineParam -> 
        case refOrInlineParam of
          Ref (Reference pmText) -> 
            case HMSIns.lookup pmText refParamsHM of
              Just refParam -> 
                if (_paramName refParam) == T.pack paramPathName
                then 
                  case existingParamType of
                    Nothing -> do
                      pathParamType <- getParamTypeForPathParam refParam
                      pure $ Just pathParamType
                    Just _ -> error $ "Atleast two or more Params in the Params Ref HM match this param." 
                      ++ "This should be impossible. Please check the Swagger Spec!"
                      ++ "\nDebug Info (Path Param Name) : " ++ (show paramPathName)
                else pure existingParamType
              Nothing -> pure existingParamType
          Inline param -> case (_paramName param == T.pack paramPathName) of
            True -> do
              pathParamType <- getParamTypeForPathParam param
              pure $ Just pathParamType
            False -> pure existingParamType
        ) Nothing paramList 
      pure mParamType 
    Nothing -> pure $ Nothing

  getParamTypeForPathParam :: Param -> StateT [CreateNewType] IO String
  getParamTypeForPathParam param = 
    case (_paramSchema param) of
      ParamOther paramOtherSchema -> 
        case _paramOtherSchemaIn paramOtherSchema of
          ParamPath -> 
            getTypeFromSwaggerType Nothing Nothing (_paramOtherSchemaParamSchema paramOtherSchema) 
          _ -> error $ "Expected Path Param but got another Param Type. \nParam : " ++ (show param)
      ParamBody _ -> error $ "Param matched by name in the Ref Params HM. "
                  ++ "This means it should be a Path Param but it is a Body Param. "
                  ++ "This is theoretically impossible. Please check the Swagger Doc!"
                  ++ "\nDebug Info : (Path) Param ->  \n" ++ (show param)
  

  processPathItem :: String -> PathItem -> Swagger -> (Map.Map StdMethod ApiTypeDetails) ->  StdMethod -> StateT [CreateNewType] IO (Map.Map StdMethod ApiTypeDetails)
  processPathItem mainRouteName pathItem swaggerData methodDataAcc currentMethod =
    case currentMethod of
      GET -> (processOperation mainRouteName methodDataAcc swaggerData) GET $ _pathItemGet pathItem
      PUT -> (processOperation mainRouteName methodDataAcc swaggerData) PUT $ _pathItemPut pathItem
      POST -> (processOperation mainRouteName methodDataAcc swaggerData) POST $ _pathItemPost pathItem
      DELETE -> (processOperation mainRouteName methodDataAcc swaggerData) DELETE $ _pathItemDelete pathItem
      OPTIONS -> (processOperation mainRouteName methodDataAcc swaggerData) OPTIONS $ _pathItemOptions pathItem
      HEAD -> (processOperation mainRouteName methodDataAcc swaggerData) HEAD $ _pathItemHead pathItem
      PATCH -> (processOperation mainRouteName methodDataAcc swaggerData) PATCH $ _pathItemPatch pathItem
      _ -> pure $ Map.empty
  processOperation :: String -> Map.Map StdMethod ApiTypeDetails -> Swagger -> StdMethod -> Maybe Operation -> StateT [CreateNewType] IO (Map.Map StdMethod ApiTypeDetails)
  processOperation currentRouteName methodAcc swaggerData stdMethod mOperationData = 
    case mOperationData of
      Just operationData -> do
        let refParamsHM = _swaggerParameters swaggerData
        let apiResponses = _responsesResponses $ _operationResponses operationData
        (mApiOutType, apiErrType) <- getApiType (currentRouteName ++ show stdMethod) apiResponses swaggerData
        -- TODO: Case match on ApiOut and if `Nothing` then check for default responses in `_responsesDefault $ _operationResponses operationData`
        let apiOutType = fromMaybe "()" mApiOutType
        let addPlainText = 
              case apiOutType of
                "()" -> True
                "Text" -> True
                _ -> False
        -- Group the Referenced Params by ParamLocation and then go through each group separately.
        let (formParamList, queryParamList, fileParamList, headerInList, bodyParamList) = DL.foldl' (groupParamTypes refParamsHM) ([], [], [], [], []) (_operationParameters operationData)
        mFormParamType <- getParamTypes (currentRouteName ++ show stdMethod) formParamList FormParam
        mQueryParamType <- getParamTypes (currentRouteName ++ show stdMethod) queryParamList QueryParam
        mFileParamType <- getParamTypes (currentRouteName ++ show stdMethod) fileParamList FileParam
        mHeaderInType <- getParamTypes (currentRouteName ++ show stdMethod) headerInList HeaderParam
        mReqBodyType <- getParamTypes (currentRouteName ++ show stdMethod) bodyParamList BodyParam
        let (mContentTypes, xmlPresent) = getContentTypes (_operationProduces operationData) addPlainText
        let finalReqBodyType = flip fmap mReqBodyType (\reqBodyType -> 
              case (DL.isPrefixOf "[" reqBodyType) of
                True -> "'" ++ reqBodyType
                False -> "'[" ++ reqBodyType ++ "]" )
        pure $ Map.insert stdMethod (ApiTypeDetails apiOutType apiErrType mFormParamType mQueryParamType mFileParamType mHeaderInType finalReqBodyType mContentTypes xmlPresent) methodAcc
      Nothing -> pure methodAcc

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
  

  getApiType :: String -> InsOrdHashMap HttpStatusCode (Referenced Response) -> Swagger -> StateT [CreateNewType] IO (Maybe String, Maybe String)
  getApiType newTypeName responsesHM swaggerData = foldlWithKey' (\stateConfigWrappedTypes currentCode currentResponse -> do
        (apiOutType, apiErrType) <- stateConfigWrappedTypes
        case (currentCode >= 200 && currentCode < 300) of
          True -> do
            finalOutType <- do
                let newTypeNameConstructor = (newTypeName ++ "ApiOut")
                currentResponseType <- parseResponseContentGetType currentResponse swaggerData newTypeNameConstructor
                fOutType <- checkIfNewType apiOutType currentResponseType newTypeNameConstructor swaggerData
                pure $ Just fOutType
            pure (finalOutType, apiErrType)
          False -> do
            finalErrType <- do
                  let newTypeNameConstructor = (newTypeName ++ "ApiErr")
                  currentResponseType <- parseResponseContentGetType currentResponse swaggerData newTypeNameConstructor
                  fErrType <- checkIfNewType apiErrType currentResponseType newTypeNameConstructor swaggerData
                  pure $ Just fErrType
            pure (apiOutType, finalErrType)
    ) (pure (Nothing, Nothing)) responsesHM  
  parseResponseContentGetType :: Referenced Response -> Swagger -> String -> StateT [CreateNewType] IO String
  parseResponseContentGetType referencedResp swaggerData newTypeConsName = do
    let swResponses :: InsOrdHashMap Text Response = _swaggerResponses swaggerData
    -- let swDataDefns :: InsOrdHashMap Text Schema = _swaggerDefinitions swaggerData

    case referencedResp of
      Ref refText -> 
        case HMSIns.lookup (getReference refText) swResponses of
          Just refResponse -> 
            case _responseSchema refResponse of
              Just (Ref (Reference refSchema) ) -> pure $ T.unpack refSchema 
              Just (Inline inSchema) -> ( (getTypeFromSwaggerType (Just newTypeConsName) (Just inSchema) ) . _schemaParamSchema) inSchema
              Nothing -> pure "Text"
            -- TODO : Should we error out here or inform the user that Referenced Response not found in Responses HM? 
          Nothing -> pure "Text"
      Inline responseSchema -> 
        case (_responseSchema responseSchema) of
          Just (Ref refText) -> pure $ T.unpack $ getReference refText
          Just (Inline respSchema) -> ((getTypeFromSwaggerType (Just newTypeConsName) (Just respSchema) ) . _schemaParamSchema) respSchema
          Nothing -> pure "Text"
  getParamTypes :: String -> [Param] -> ParamType -> StateT [CreateNewType] IO (Maybe String)
  getParamTypes newTypeName paramList paramType = 
    case paramList of
      [] -> pure $ Nothing
      _ -> -- TODO : Refactor handling of adding Maybes and adding to State into a single function and call from all places.
        case paramType of
          FormParam -> do
            let paramNames = fmap (\param -> T.unpack $ _paramName param) paramList
            hTypesWithIsMandatory <- forM paramList (\param -> do 
              hType <- getParamTypeParam param (Just $ T.unpack ( _paramName param) ) Nothing
              pure (isMandatory param, hType) )
            let finalHaskellTypes = fmap (\(isMandatoryType, hType) -> (addMaybeToType isMandatoryType hType) ) hTypesWithIsMandatory
            let recordTypesInfo = DL.zip paramNames finalHaskellTypes
            let newDataTypeName = setValidConstructorId $ newTypeName ++ "FormParam"
            let formParamDataInfo = ProductType (NewData newDataTypeName recordTypesInfo)

            modify' (\existingState -> formParamDataInfo:existingState) 
            pure $ Just newDataTypeName
          QueryParam -> do
            let paramNames = fmap (\param -> T.unpack $ _paramName param) paramList
            hTypesWithIsMandatory <- forM paramList (\param -> do 
              hType <- getParamTypeParam param (Just $ T.unpack ( _paramName param) ) Nothing
              pure (isMandatory param, hType) )
            let finalHaskellTypes = fmap (\(isMandatoryType, hType) -> (addMaybeToType isMandatoryType hType) ) hTypesWithIsMandatory
            let recordTypesInfo = DL.zip paramNames finalHaskellTypes
            let newDataTypeName = setValidConstructorId $ newTypeName ++ "QueryParam"
            let queryParamDataInfo = ProductType (NewData newDataTypeName recordTypesInfo)
            modify' (\existingState -> queryParamDataInfo:existingState) 
            pure $ Just newDataTypeName
          HeaderParam -> do
            typeListWithIsMandatory <- forM paramList (\param -> do
                    hType <- getParamTypeParam param (Just $ T.unpack ( _paramName param) ) Nothing
                    pure (isMandatory param, hType) )
            let finalHaskellTypes = fmap (\(isMandatoryType, hType) -> (addMaybeToType isMandatoryType hType) ) typeListWithIsMandatory
            case finalHaskellTypes of
              [] -> pure Nothing
              x:[] -> pure $ Just x
              _ -> error $ "Encountered multiple Header Params! This is not yet handled!"
                ++ "\nDebug Info : " ++ (show paramList)
          FileParam -> do
            typeList <- forM paramList (\param -> getParamTypeParam param (Just $ T.unpack ( _paramName param) ) Nothing )
            case typeList of
              [] -> pure Nothing
              x:[] -> pure $ Just x
              _ -> error $ "Encountered list of FileParam. This is not yet handled! "
                ++ "\nDebug Info: " ++ (show paramList)
          BodyParam -> do
            listOfTypes <- forM paramList (\param -> getParamTypeParam param (Just $ T.unpack (_paramName param)) Nothing  )
            case listOfTypes of
              [] -> error $ "Tried to Get Body Param type but got an empty list/string! Debug Info: " ++ show paramList
              x:[] -> pure $ Just x
              _ -> error $ "Encountered a list of Body Params. WebApi does not support this currently! Debug Info: " ++ show paramList
  getParamTypeParam inputParam mParamName mOuterSchema =
    case _paramSchema inputParam of
      ParamBody refSchema -> 
        case refSchema of 
          Ref refType -> pure $ T.unpack (getReference refType)
          Inline rSchema -> getTypeFromSwaggerType mParamName (Just rSchema) (_schemaParamSchema rSchema)
      ParamOther pSchema -> getTypeFromSwaggerType mParamName mOuterSchema $ _paramOtherSchemaParamSchema pSchema

  isMandatory :: Param -> Bool
  isMandatory param = 
    case _paramRequired param of
      Just True -> True
      _ -> False
  addMaybeToType :: Bool -> String -> String
  addMaybeToType isNotNull haskellType = 
    case isNotNull of
      True -> haskellType
      False -> "Maybe " ++ haskellType 





checkIfNewType :: Maybe String -> String -> String -> Swagger -> (StateT [CreateNewType] IO String)
checkIfNewType existingType currentType newTypeName _ = 
  case existingType of 
    Just eType ->
      if (eType == currentType)
      then pure eType
      else do
        -- The following code handles the cases when there are 2 or more possible types for an ApiOut or ApiErr
        -- It creates/modifies a Sum Type with the possible response Types.
        let sumTypeInfo = SumType newTypeName [currentType, eType] [currentType, eType] -- Note : OgNames not really applicable here so putting Haskell names
        modify' (\existingState -> do
          let (stateList, isChanged) = DL.foldl' (addToStateSumType newTypeName currentType ) ([], False) existingState
          case isChanged of
            False -> sumTypeInfo:existingState
            True -> stateList)
        pure newTypeName
    Nothing -> pure currentType

 where
  getName :: CreateNewType -> String 
  getName cnType =
    case cnType of
      SumType consName _ _ -> consName
      ProductType (NewData consName _) -> consName
      TypeAlias consName _ -> consName

  addToStateSumType :: String -> String -> ([CreateNewType], Bool) -> CreateNewType -> ([CreateNewType], Bool)
  addToStateSumType newSumTypeName currentTypeStr (accList, isChanged) currentStType =
    case getName currentStType == newSumTypeName of
      True -> 
        case currentStType of
          SumType consName hsCons ogCons -> do
            let modSumType = SumType consName (currentTypeStr:hsCons) (currentTypeStr:ogCons)
            (modSumType:accList, True)
          ProductType _ -> (currentStType:accList, isChanged)
          TypeAlias _ _ -> (currentStType:accList, isChanged)
      False -> (currentStType:accList, isChanged)


getTypeFromSwaggerType :: Maybe String -> Maybe Schema ->  ParamSchema t -> StateT [CreateNewType] IO String 
getTypeFromSwaggerType mParamNameOrRecordName mOuterSchema paramSchema = 
    case (_paramSchemaType paramSchema) of 
      Just SwaggerString -> 
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
                let (enumVals, ogVals) = DL.unzip $ fmap (\(Data.Aeson.String enumVal) -> (T.unpack $ T.toTitle enumVal, T.unpack enumVal) ) valueEnumList
                let innerRecordTypeName = 
                      setValidConstructorId $ 
                      fromJustNote ("Expected a Param Name but got Nothing. "
                        ++ "Need Param Name to set the name for the new type we need to create."
                        ++ "\nDebug Info: " ++ show paramSchema) mParamNameOrRecordName
                let haskellNewTypeInfo = SumType innerRecordTypeName enumVals ogVals
                currentState <- get
                let onlySumTypes = DL.filter (\newTypeObj -> 
                          case newTypeObj of 
                            SumType _ _ _ -> True
                            _ -> False ) currentState
                let (createNewType, newOrExistingName) = DL.foldl' (checkIfSumTypeExists haskellNewTypeInfo) (True, innerRecordTypeName) onlySumTypes
                case createNewType of
                  True -> do
                    modify' (\existingState -> haskellNewTypeInfo:existingState)
                    pure innerRecordTypeName
                  False -> pure newOrExistingName
          _ -> pure "Text" -- error $ "Encountered SwaggerString with unknown Format! Debug Info: " ++ show paramSchema
      Just SwaggerNumber -> 
        case _paramSchemaFormat paramSchema of
          Just "float" -> pure "Float"
          Just "double" -> pure "Double"
          _ -> pure "SwaggerNumber"
      Just SwaggerInteger -> 
        case _paramSchemaFormat paramSchema of
          Just "int32" -> pure "Int32"
          Just "int64" -> pure "Int64"
          _ -> pure "Int"
      Just SwaggerBoolean -> pure "Bool"
      -- As per the pattern in `PetStore`, for SwaggerArray, we check the Param Schema Items field and look for a reference Name there.
      Just SwaggerArray -> case _paramSchemaItems paramSchema of
                        Just (SwaggerItemsObject obj) -> 
                          case obj of
                            Ref reference -> pure $ "[" ++ (T.unpack $ getReference reference) ++ "]"
                            Inline recursiveSchema -> do
                              hType <- ( ( (getTypeFromSwaggerType mParamNameOrRecordName (Just recursiveSchema) ) . _schemaParamSchema) recursiveSchema)
                              pure $ "[" ++ hType ++ "]"
                        Just (SwaggerItemsArray innerArray) -> checkIfArray $ flip Control.Monad.mapM innerArray (\singleElem -> do
                          case singleElem of
                            Ref ref -> pure $ T.unpack $ getReference ref
                            Inline innerSchema -> ((getTypeFromSwaggerType mParamNameOrRecordName (Just innerSchema) ) . _schemaParamSchema) innerSchema) 
                        Just (SwaggerItemsPrimitive mCollectionFormat innerParamSchema) -> do
                          typeName <- do
                                let paramName = fromJustNote ("Expected a Param Name but got Nothing. Need Param Name to set the name for the new type we need to create. Debug Info: " ++ show paramSchema) mParamNameOrRecordName
                                let titleCaseParamName = T.unpack $ T.toTitle $ T.pack paramName
                                case _paramSchemaEnum innerParamSchema of
                                  Just enumVals -> do
                                    let (enumValList, ogVals)::([String], [String]) = 
                                          DL.unzip $ fmap (\(Data.Aeson.String val) -> (T.unpack $ T.toTitle val, T.unpack val) ) enumVals
                                    let haskellNewTypeInfo = SumType titleCaseParamName enumValList ogVals
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
      Just SwaggerObject -> do
        let recordTypeName = 
              setValidConstructorId $ 
              fromJustNote ("Expected a Param Name but got Nothing. "
                ++ "Need Param Name to set the name for the new type we need to create."
                ++ "\nDebug Info: " ++ show paramSchema) mParamNameOrRecordName
        case mOuterSchema of
          Just outerSchema -> 
            case (HMSIns.toList $ _schemaProperties outerSchema) of
              [] -> 
                case (_schemaAdditionalProperties outerSchema) of
                  Just additionalProps -> 
                    case additionalProps of
                      AdditionalPropertiesSchema (Ref ref) -> pure $ "(HM.HashMap Text " ++ (T.unpack $ getReference ref) ++ ")"
                      AdditionalPropertiesSchema (Inline internalSchema) -> ((getTypeFromSwaggerType (Just recordTypeName) (Just internalSchema)) . _schemaParamSchema) internalSchema
                      AdditionalPropertiesAllowed _ -> error "TODO: unhandled case of additional props"
                      
                  Nothing -> 
                    case (_paramSchemaType . _schemaParamSchema) outerSchema of
                      Just SwaggerObject -> pure $ "(HM.HashMap Text Text)"
                      _ -> error $ "Type SwaggerObject but swaggerProperties and additionalProperties are both absent! "
                        ++ "Also, the paramSchema type in the ParamSchema is not an Object! Please check the JSON! "
                        ++ "Debug Info (Schema): " ++ show outerSchema
              propertyList -> do -- TODO: This needs to be changed when we encounter _schemaProperties in some swagger doc/schema.
                innerRecordsInfo <- forM propertyList (\(recordName, iRefSchema) -> do
                      innerRecordType <- case iRefSchema of
                          Ref refName -> pure $ T.unpack $ getReference refName
                          Inline irSchema -> ((getTypeFromSwaggerType (Just recordTypeName) (Just irSchema)) . _schemaParamSchema) irSchema 
                      pure (T.unpack recordName, innerRecordType) )
                let finalProductTypeInfo = ProductType $ NewData recordTypeName innerRecordsInfo
                modify' (\existingState -> finalProductTypeInfo:existingState)
                pure recordTypeName
          Nothing -> error $ "Expected outer schema to be present when trying to construct type of SwaggerObject. Debug Info (ParamSchema):  " ++ show paramSchema
      Just SwaggerFile -> pure "W.FileInfo" -- TODO 
      Just SwaggerNull -> pure "()"
      -- NOTE: what are types which have no type info?
      Nothing          -> pure "()"
      -- x -> ("Got Unexpected Primitive Value : " ++ show x)
 where 
  checkIfArray :: StateT [CreateNewType] IO [String] -> StateT [CreateNewType] IO String
  checkIfArray scStringList = do
    stringList <- scStringList 
    case DL.nub stringList of
      sameElem:[] -> pure $ "[" ++ sameElem ++ "]"
      x -> error $ "Got different types in the same list. Not sure how to proceed! Please check the swagger doc! " ++ show x
  checkIfSumTypeExists :: CreateNewType -> (Bool, String) -> CreateNewType -> (Bool, String)
  checkIfSumTypeExists (SumType newTypeName newTypeVals ogVals) (newTypeNeeded, newOrExistingTypeName) (SumType typeName tVals tOgVals) = 
    case newTypeNeeded of
      False -> (newTypeNeeded, newOrExistingTypeName)
      True -> 
        case (newTypeVals == tVals) of 
          True -> (False, typeName)
          False -> 
            case (newTypeVals `DL.intersect` tVals) of
              [] -> (True, newTypeName)
              _ -> error $ "A new sum type would be created with elements already existing in other sum types!"
                ++ "This needs to be handled! \nDebug Info: Type1 -> " ++ show (SumType newTypeName newTypeVals ogVals) 
                ++ "\nType2 (added to state first) -> " ++ show (SumType typeName tVals tOgVals)
  checkIfSumTypeExists newType _ existingType =
    error $ "PANIC : We already filtered for only Sum Types but encountered non-sum type constructor!"
      ++ "\nDebugInfo : New Type to be created is : " ++ (show newType)
      ++ "\nExisting type is : " ++ (show existingType)



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
  , hasXML :: Bool
  -- TODO: cookie in/out and header out need to be added when we encounter them
  } deriving (Eq, Show)

data NewData = NewData
  {
    mName :: String
  , mRecordTypes :: InnerRecords
  } deriving (Eq, Show)
--                                   hsNames  ogNames
data CreateNewType = SumType String [String] [String] | ProductType NewData | TypeAlias String String
  deriving (Eq, Show)

data PathComponent = PathComp String | PathParamType String
  deriving (Eq, Show)
   
data SwPathComponent = PathPiece String | PathParamName String
  deriving (Eq, Show)


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
  qualMethod :: StdMethod -> String
  qualMethod = ("W." ++) . show
    
  constructVectorForRoute :: String -> ContractDetails -> [(Vector 4 String, [Vector 4 String])] 
  constructVectorForRoute ctrtName ctrDetails = 
    let currentRouteName = routeName ctrDetails
    in Map.foldlWithKey' (routeDetailToVector ctrtName currentRouteName) [] (methodData ctrDetails)
  routeDetailToVector :: String -> String -> [(Vector 4 String, [Vector 4 String])] -> StdMethod -> ApiTypeDetails -> [(Vector 4 String, [Vector 4 String])]
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



type InnerRecords = [(String, String)]
type DerivingClass = String  
                      -- old name, new name
type ModifiedRecords = [(String, String)]


typeAliasForDecl :: String -> String -> Decl SrcSpanInfo
typeAliasForDecl typeNameStr typeAliasStr =
  TypeDecl noSrcSpan (DHead noSrcSpan (nameDecl typeNameStr)) (typeConstructor typeAliasStr)

#if MIN_VERSION_haskell_src_exts(1,20,0)
dataDeclaration :: (DataOrNew SrcSpanInfo) -> String -> InnerRecords -> [DerivingClass] -> (ModifiedRecords, Decl SrcSpanInfo)
dataDeclaration dataOrNew dataName innerRecords derivingList = 
  let (modRecords, constructorDecl) = constructorDeclaration dataName innerRecords
      decl =
        DataDecl noSrcSpan  
          dataOrNew 
          Nothing 
          (declarationHead dataName)
          constructorDecl
          [derivingDecl  derivingList]
  in (modRecords, decl)
#else
dataDeclaration :: (DataOrNew SrcSpanInfo) -> String -> InnerRecords -> [DerivingClass] -> (ModifiedRecords, Decl SrcSpanInfo)
dataDeclaration dataOrNew dataName innerRecords derivingList = 
  let (modRecords, constructorDecl) = constructorDeclaration dataName innerRecords
      decl =
        DataDecl noSrcSpan  
          dataOrNew 
          Nothing 
          (declarationHead dataName)
          constructorDecl
          (Just $ derivingDecl  derivingList)
  in (modRecords, decl)
#endif


declarationHead :: String -> DeclHead SrcSpanInfo
declarationHead declHeadName = (DHead noSrcSpan (Ident noSrcSpan declHeadName) )

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
      let lCaseNewVal = (Char.toLower $ DL.head newVal):(DL.tail newVal)
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
        (instanceHead "Show") 
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


data JsonDirection = ToJson | FromJson deriving (Eq)

instance Show JsonDirection where
  show ToJson = "ToJSON"
  show FromJson = "FromJSON" 

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
                    (QVarOp noSrcSpan (UnQual noSrcSpan (Symbol noSrcSpan "$"))) 
                    (RecUpdate noSrcSpan (variableName "defaultOptions")
                        [FieldUpdate noSrcSpan (UnQual noSrcSpan (Ident noSrcSpan "fieldLabelModifier")) 
                        (InfixApp noSrcSpan (variableName "keyMapping")
                            (QVarOp noSrcSpan (UnQual noSrcSpan (Symbol noSrcSpan "$"))) 
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
        (TyPromoted noSrcSpan (PromotedCon noSrcSpan True (UnQual noSrcSpan (nameDecl "QueryParam") ) ) )) 
      (typeConstructor sumTypeName) )

-- The ToParam 'QueryParam instance for Sum Type
toParamQueryParamInstance :: String -> Decl SrcSpanInfo
toParamQueryParamInstance sumTypeName = 
  let sumTypeVarName = (fmap Char.toLower sumTypeName) ++ "'"
  in InstDecl noSrcSpan Nothing
      (queryParamInstanceIRule "ToParam" sumTypeName)
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
                          (dataConstructor "Just")
                          (QVarOp noSrcSpan (UnQual noSrcSpan (Symbol noSrcSpan "$"))) 
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
    (UnGuardedRhs noSrcSpan (App noSrcSpan (dataConstructor "Just") (dataConstructor resultOfCaseMatch) )) 
    Nothing

-- the EncodeParam instance for Sum Type
encodeParamSumTypeInstance :: String -> [(String, String)] -> Decl SrcSpanInfo
encodeParamSumTypeInstance sumTypeName caseOptions =
  let sumTypeVarName = (fmap Char.toLower sumTypeName) ++ "'"
  in InstDecl noSrcSpan Nothing
      (IRule noSrcSpan Nothing Nothing (IHApp noSrcSpan (instanceHead "EncodeParam") (typeConstructor sumTypeName) ))
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
      (IRule noSrcSpan Nothing Nothing (IHApp noSrcSpan (instanceHead "DecodeParam") (typeConstructor sumTypeName) ))
      (Just [InsDecl noSrcSpan 
        (FunBind noSrcSpan 
          [Match noSrcSpan (nameDecl "decodeParam") 
            [PVar noSrcSpan (nameDecl sumTypeVarName)] 
            (UnGuardedRhs noSrcSpan 
              (Case noSrcSpan 
                (variableName sumTypeVarName) 
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
                  (UnQual noSrcSpan 
                    (nameDecl "Just")
                  ) 
                  [PParen noSrcSpan 
                    (PApp noSrcSpan 
                      (UnQual noSrcSpan 
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
                      (Var noSrcSpan (Qual noSrcSpan (ModuleName noSrcSpan "Prelude") (nameDecl "id")))) 
                      (Paren noSrcSpan 
                        (Lambda noSrcSpan [PVar noSrcSpan (nameDecl "inputConst")] 
                          (Case noSrcSpan 
                            (Var noSrcSpan (UnQual noSrcSpan (nameDecl "inputConst"))) 
                            (fmap caseMatchStatement constructorValues ++ errorCaseMatch)
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
              (typeConstructor "MultiSet") 
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
              (typeConstructor "MultiSet") 
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

haskellKeywords :: [String]
haskellKeywords = 
  ["as"
  ,"case"
  ,"of"
  ,"class"
  ,"data"
  ,"data family"
  ,"data instance"
  ,"default"
  ,"deriving"
  ,"deriving instance"
  ,"do"
  ,"forall"
  ,"foreign"
  ,"hiding"
  ,"if"
  ,"then"
  ,"else"
  ,"import"
  ,"infix"
  ,"infixl"
  ,"infixr"
  ,"instance"
  ,"let"
  ,"in"
  ,"mdo"
  ,"module"
  ,"newtype"
  ,"proc"
  ,"qualified"
  ,"rec"
  ,"type"
  ,"type family"
  ,"type instance"
  ,"where"]


writeCabalAndProjectFiles :: FilePath -> String -> Bool -> IO ()
writeCabalAndProjectFiles generationPath projectName needsWebapiXml = do
  writeFile (generationPath ++ projectName ++ ".cabal") (cabalFileContents needsWebapiXml)
  writeFile (generationPath ++ "LICENSE") licenseFileContents
  -- TODO : Once webapi-xml is pushed to GitHub, it needs to be added to the cabal.project file
  writeFile (generationPath ++ "cabal.project") cabalProjectFileContents


 where
  cabalFileContents :: Bool -> String
  cabalFileContents webapiXmlNeeded = [i|
name:           #{projectName}
version:        0.1.0.0
description:    Generated project with a contract accoriding to the provided Swagger doc.
homepage:       http://byteally.github.io/webapi/
bug-reports:    https://github.com/byteally/webapi/issues
author:         Magesh B
maintainer:     magesh85@gmail.com
copyright:      2018 Byteally
license:        BSD3
license-file:   LICENSE
build-type:     Simple
cabal-version:  >= 1.10

library
  exposed-modules:
      Contract
  other-modules:
      Types 
      CommonTypes
  hs-source-dirs:
      src
  build-depends:
      base >=4.7 && <5
    , text
    , swagger2
    , lens
    , aeson
    , bytestring
    , webapi
    , unordered-containers
    , webapi-contract
    , vector
    , time
    , multiset
    #{if webapiXmlNeeded then ", webapi-xml" else ""::String}
  default-language: Haskell2010
  ghc-options: -Wall
  |]

  cabalProjectFileContents :: String 
  cabalProjectFileContents = [i|
compiler : ghc
packages : .
source-repository-package
    type: git
    location: https://github.com/byteally/webapi
    tag: 8346e24255a72130ba4097a40aacd7897e0e70de
    subdir: webapi-contract

source-repository-package
    type: git
    location: https://github.com/byteally/webapi
    tag: 8346e24255a72130ba4097a40aacd7897e0e70de
    subdir: webapi

source-repository-package
    type: git
    location: https://github.com/byteally/webapi
    tag: 8346e24255a72130ba4097a40aacd7897e0e70de
    subdir: webapi-xml

source-repository-package
    type: git
    location: https://github.com/capital-match/bytestring-trie.git
    tag: 47526b2ec810239fe824c03c13cf1d81f0741b5c
|]


  licenseFileContents :: String
  licenseFileContents = [i|
Copyright Author name here (c) 2018

All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

    * Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

    * Neither the name of Author name here nor the names of other
      contributors may be used to endorse or promote products derived
      from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

|]



---------------------------------------------------------------------------------------
-- Support multiple versions of GHC (Use ifndef )
-- for LTS 9.0 -> 1.18.2






