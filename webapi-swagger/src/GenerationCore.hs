{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds             #-}


module GenerationCore where

import SwaggerGen as SG

-- import GHC.Generics
-- import qualified Data.HashMap.Strict as HM
-- import qualified Data.HashMap.Strict.InsOrd as OHM
-- import Data.Hashable
-- import qualified Data.Aeson as A
import qualified Data.Text as T
import qualified Data.List as DL
import Language.Haskell.Exts as LHE hiding (OPTIONS, Int, Tuple, Comma)
import Data.Maybe
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
import Data.Either
import Data.Text.Encoding
import qualified Network.HTTP.Media.MediaType as HTTP
import Dhall (auto, input)

-- import Debug.Trace

type ModuleTypes = HMS.HashMap SG.Module ([TypeDefinition], Imports)

type Imports = HMS.HashMap String String

----------- Functions that may be shifted into SwaggerGen  ---------------------------------------
getModuleFromProvenance :: Provenance -> SG.Module
getModuleFromProvenance (Provenance sgMod _ ) = sgMod

getModQualInfoModuleState :: ModuleState -> SG.Module -> (String, String)
getModQualInfoModuleState moduleStateHM sgModule =
  case HMS.lookup sgModule moduleStateHM of
    Just (HaskModule fullModNameList modQual) -> 
      (T.unpack $ T.intercalate "." fullModNameList, T.unpack modQual) 
    Nothing -> error $ "The impossible happened! Did not find mapping for Module in ModuleState!" 
                ++ "Debug Info: \nModule: " ++ (show sgModule) ++ "\nModuleState: " ++ (show moduleStateHM)


getModulePathAndQualInfo :: ModuleState -> SG.Module -> (String, String, String)
getModulePathAndQualInfo moduleStateHM sgModule =
  case HMS.lookup sgModule moduleStateHM of
    Just (HaskModule fullModNameList modQual) -> 
      let (innerTypesModuleName, modulePath) = 
              case fullModNameList of
                [] -> error $ "Encountered empty list in HaskModule! Expected Module Name components!"
                        ++ "ModuleState: " ++ (show moduleStateHM )
                modNameList -> 
                  let hsModName = (T.unpack $ DL.last modNameList) ++ ".hs"
                      relativePath = (T.unpack $ T.intercalate "/" (DL.init fullModNameList) ) ++ "/"
                  in (hsModName, relativePath)
      in (modulePath, innerTypesModuleName, T.unpack modQual) 
    Nothing -> error $ "The impossible happened! Did not find mapping for Module in ModuleState!" 
                ++ "Debug Info: \nModule: " ++ (show sgModule) ++ "\nModuleState: " ++ (show moduleStateHM)


getModuleFromTypeMeta :: TypeMeta -> SG.Module
getModuleFromTypeMeta (Definition (Provenance sgMod _) _) = sgMod

lookupTypeStateGetCustomHaskType :: TypeMeta -> TypeState -> CustomType 
lookupTypeStateGetCustomHaskType typeMeta typeStateHM = 
  case HMS.lookup typeMeta typeStateHM of
    Just typeDefn -> customHaskType typeDefn
    Nothing -> error $ "The impossible happened! Could not find TypeMeta in TypeState HM!"
                        ++ "\nTypeMeta: " ++ (show typeMeta)
                        ++ "\nTypeState: " ++ (show typeStateHM)

---------------------------------------------------------------------------------------------------------------------              


parseTypeStateIntoModuleTypes :: TypeState -> ModuleState -> ModuleTypes
parseTypeStateIntoModuleTypes tyState moduleStateHM = 
  HMS.foldlWithKey' parseTypeState HMS.empty tyState

 where
  parseTypeState :: ModuleTypes -> TypeMeta -> TypeDefinition -> ModuleTypes
  parseTypeState accModTypes typeMeta typeDefn = 
    let provenanceMod = getProvenanceModule typeMeta
        importList = importsForTypes (customHaskType typeDefn) 
        importsHM = constructImportsHMFromList importList
        finalImportHM = HMS.delete (fst $ getModQualInfoModuleState moduleStateHM provenanceMod) importsHM
        newModuleTypes = HMS.insertWith (\(newTypes, newImps) (oldTypes, oldImps) -> (newTypes ++ oldTypes, HMS.union newImps oldImps) ) 
                                provenanceMod ([typeDefn], finalImportHM) accModTypes
    in newModuleTypes

  importsForTypes :: CustomType -> [(String, String)]
  importsForTypes (CustomType _ dataConsList) = 
      DL.concat $ fmap (parseDataConsRef) dataConsList 
  importsForTypes (CustomNewType _ dConsName mRec ref) = 
    parseDataConsRef (DataConstructor dConsName [(mRec, ref)])

  parseDataConsRef :: DataConstructor -> [(String, String)]
  parseDataConsRef (DataConstructor _ recList) = 
    let importsList = fmap (\(_, ref) -> getRefImports ref moduleStateHM) recList
    in DL.concat importsList

runCodeGen :: FilePath -> FilePath -> FilePath -> String -> IO () 
runCodeGen swaggerJsonInputFilePath outputPath dhallFilePath projectName = do
  -- removeDirectoryRecursive outputPath
  let projectFolderGenPath = outputPath ++ projectName ++ "/"
  let projectSrcDir = (projectFolderGenPath ++ "src/")
  hasSrcDir <- doesPathExist projectSrcDir
  case hasSrcDir of
    True  -> removeDirectoryRecursive projectSrcDir
    False -> removeDirectoryRecursive outputPath
  createDirectoryIfMissing True projectSrcDir
  swaggerJSONContents <- liftIO $ BSL.readFile swaggerJsonInputFilePath
  let decodedVal = eitherDecode swaggerJSONContents <|> either (Left . show) Right (decodeEither' (BSL.toStrict swaggerJSONContents))
  case decodedVal of
    Left errMsg -> error $ errMsg -- "Panic: not a valid JSON or yaml"
    Right (swaggerData :: Swagger) -> do
      -- TODO: Read in the Config(s) from a file later
      -- let config = Config {instanceTemplates = []}
      instTemplates <- input auto (T.pack dhallFilePath) -- "/Users/kahlil/projects/ByteAlly/tmp/swaggerConfig.dhall"
              
      let config = Config {instanceTemplates = instTemplates}
      let instanceTemplateList = instanceTemplates config
      finalStateVal <- runSwaggerGenerator config (generateSwaggerState swaggerData)
      writeFile (projectSrcDir ++ "CommonTypes.hs") commonTypesModuleContent
      let tyState = typeState finalStateVal
      let routeStateHM = routeState finalStateVal
      let apiContractStateHM = apiContract finalStateVal
      let moduleStateHM = modules finalStateVal
      modulesWithQualNames <- generateModulesFromTypeState tyState projectSrcDir moduleStateHM instanceTemplateList
      generateContractFromContractState "SwaggerContract" apiContractStateHM routeStateHM projectFolderGenPath moduleStateHM tyState
      let moduleNamesOnly =  fst $ DL.unzip $ Set.toList modulesWithQualNames
      writeCabalAndProjectFiles projectFolderGenPath projectName False moduleNamesOnly

generateContractFromContractState :: String -> ContractState -> RouteState -> FilePath -> ModuleState -> TypeState -> IO ()
generateContractFromContractState contractName contractState routeStateHM genDirPath moduleStateHM typeStateHM = do
  let routeListUnGrouped = HMS.keys contractState
  let groupedRoutes = ( (DL.groupBy (\(route, _) (route2, _) -> route == route2 ) ) . DL.sort ) routeListUnGrouped 
  let unParsedRoutePaths = fmap selectFirstRouteFromList groupedRoutes
  let refRoutePaths = fmap lookupRouteInRouteState unParsedRoutePaths
  let routeNames = fmap (constructRouteName) refRoutePaths

  let moduleQualNames = fmap (\(route, method) -> getModQualInfoModuleState moduleStateHM (getModuleFromProvenance $ localProv route method) ) routeListUnGrouped
  let otherModuleNames = DL.concat $ fmap (getModuleImportsForRouteState . getRoute) $ HMS.elems routeStateHM 
  let importsHM = constructImportsHMFromList (moduleQualNames ++ otherModuleNames)

  let routeDecls = fmap (routeDeclaration moduleStateHM typeStateHM) (DL.zip routeNames refRoutePaths)
  -- let ppRouteDecls = "\n\n" ++ (DL.concat $ fmap prettyPrint routeDecls)

  let routesWithMethods:: [(String, [Method])] = fmap getRouteAndMethod groupedRoutes
  let webapiInstDecl = webApiInstance contractName routesWithMethods
  -- let ppWebapiInstDecl = "\n\n" ++ (prettyPrint webapiInstDecl)
  let apiInstVectorList = HMS.foldlWithKey' (generateContractTypeInsts moduleStateHM) [] contractState
  let apiContractInstDecls = fmap (\(topVector, tyVectorList) -> apiInstanceDeclaration topVector tyVectorList)   apiInstVectorList
  -- let apiContractInsts = fmap (\decl -> "\n\n" ++ (prettyPrint decl) ) apiContractInstDecls 
  let contractLangExts = fmap languageExtension langExtsForContract
  -- let routeTypeModuleImports = Set.toList moduleAndQualNames
  -- TODO: Use the passed down HashMap for imports in order to get qualImports for Contract.
  -- We would need to go through all the type definitions of any type we are using in the Contract
  -- and then get its qualification from the ImportsHM and add it here.

  let allQualImportsForContract = (HMS.toList importsHM) ++ qualImportsForContract
  let contractImports = 
        fmap moduleImport 
          ( (DL.zip importsForContract (cycle [(False, Nothing)]) ) 
            ++ (fmap (\(fullModuleName, qual) ->  (fullModuleName, (True, Just $ ModuleName noSrcSpan qual)) ) allQualImportsForContract) ) 
  let allContractDecls = 
        [emptyDataDeclaration contractName] ++ routeDecls ++ [webapiInstDecl] ++ apiContractInstDecls


  let finalContractModule = 
        Module noSrcSpan 
          (Just $ ModuleHead noSrcSpan (ModuleName noSrcSpan "Contract") Nothing Nothing)
          (contractLangExts)
          (contractImports)
          (allContractDecls)
  writeFile (genDirPath ++ "src/Contract.hs") (prettyPrint finalContractModule)
 where
  getModuleImportsForRouteState :: [RoutePiece Ref] -> [(String, String)]
  getModuleImportsForRouteState routePieceRefList =
    DL.concat $ fmap (\singleRpRef ->
                      case  singleRpRef of 
                          Static _ -> []
                          Dynamic ref -> getRefImports ref moduleStateHM) routePieceRefList

  selectFirstRouteFromList :: [(Route UnparsedPiece, Method)]  -> Route UnparsedPiece
  selectFirstRouteFromList routeList = 
    case routeList of
      [] -> error $ "Expected atleast one element in Route ([RoutePiece])! "
      (firstElem, _):_ -> firstElem

  constructRouteName :: Route Ref -> String
  constructRouteName routeInfo =
    let routePieces = getRoute routeInfo
        routePiecesStr = T.unpack (mkRouteName routePieces)
    in routePiecesStr ++ "R"

  lookupRouteInRouteState ::(Route UnparsedPiece) -> Route Ref
  lookupRouteInRouteState unparsedRoute = 
    case HMS.lookup unparsedRoute routeStateHM of
      Just refRoute -> refRoute
      Nothing -> error $ "Expected to find (Route UnparsedPiece) in RouteState HashMap! "


  getRouteAndMethod :: [(Route UnparsedPiece, Method)] -> (String, [Method])
  getRouteAndMethod routeList = 
    DL.foldl' (\(_, methodList) (currentRoutePieces, currentMethod) -> 
                    let routeNameStr = (constructRouteName . lookupRouteInRouteState) currentRoutePieces
                    in (routeNameStr, currentMethod:methodList)  ) ("", []) routeList
      
  _prettifyRouteName :: [RoutePiece Ref] -> String
  _prettifyRouteName routePieces = case routePieces of
    [] -> error "Expected atleast one RoutePiece in the route! Got an empty list!"
    (Static ""):[] -> "BaseRoute"
    rPieces -> DL.concat $ flip fmap rPieces (\routePiece -> 
          case routePiece of
            Static rPieceTxt -> T.unpack $ T.toTitle rPieceTxt
            Dynamic rPieceRef -> 
              let (firstChar:remainingChar) = (showRefTy Nothing (Just typeStateHM) ) rPieceRef
              in (Char.toUpper firstChar):remainingChar )

  generateContractTypeInsts :: ModuleState
                            -> [(Vector 4 String, [Vector 4 String])] 
                            -> ((Route UnparsedPiece), Method) 
                            -> (ContractInfo Ref) 
                            -> [(Vector 4 String, [Vector 4 String])]
  generateContractTypeInsts modStateHM acc (unparsedRoute, method) contractInfo = 
    let refRoute = lookupRouteInRouteState unparsedRoute
        routeNameStr = constructRouteName refRoute
        topLevelVector = fromMaybeSV $ SV.fromList ["W.ApiContract", contractName, qualMethodName method, routeNameStr]
        typeInsts = parseContractInfo routeNameStr contractInfo method modStateHM typeStateHM
    in (topLevelVector, typeInsts):acc
  
  fromMaybeSV :: Maybe a -> a
  fromMaybeSV = fromJustNote "Expected a list with 4 elements for WebApi instance! "
  

qualMethodName :: Method -> String
qualMethodName = ("W." ++) . show

parseContractInfo :: String 
                  -> ContractInfo Ref 
                  -> Method 
                  -> ModuleState 
                  -> TypeState 
                  -> [Vector 4 String]
parseContractInfo routeNameStr contractInfo method moduleStateHM typeStateHM = 
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
            let reqBodyFinalTy = "'[" ++ DL.intercalate "," (fmap (\(cTy, reqBodyTy) -> "W.Content '[" ++ ("W." ++ (T.unpack cTy) ) ++ "] " ++ (insertQual reqBodyTy) ) contentAndReqBody) ++ "]"
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

  insertQual :: Ref -> String
  insertQual inputRef = showRefTyWithQual moduleStateHM typeStateHM Nothing inputRef
    -- let prov = localProv unpRouteInfo method
    --     sgMod = getModuleFromProvenance prov
    -- in 
        
    --     (_, modQualName) = getModQualInfoModuleState sgMod moduleStateHM  False
    -- in modQualName ++ "." ++ (T.unpack inputType)
    

constructImportsHMFromList :: [(String, String)] -> Imports
constructImportsHMFromList = HMS.fromList
  
  -- let uniqueModuleNames = DL.nub moduleNames
  -- in 
  --     DL.foldl' (\accHM (modName, modQualName) -> 
  --       case HMS.null accHM of
  --         False ->
  --           case HMS.member modName accHM of
  --             True -> accHM
  --             False -> 
  --               case modQualName == "P" of
  --                 True -> HMS.insert modName modQualName accHM
  --                 False ->
  --                     case ((DL.elem modQualName) . HMS.elems) accHM of
  --                       True -> addModQualToImportsHM accHM (modName, modQualName) 1
  --                       False -> HMS.insert modName modQualName accHM
  --         True -> HMS.singleton modName modQualName) HMS.empty uniqueModuleNames 
--  where
--   addModQualToImportsHM :: HMS.HashMap String String -> (String, String) -> Int -> HMS.HashMap String String
--   addModQualToImportsHM importHM (currentMod, currentQual) ctr = 
--     case ((DL.elem (currentQual ++ (show ctr) ) ) . HMS.elems) importHM of
--       True -> addModQualToImportsHM importHM (currentMod, currentQual) (succ ctr)
--       False -> HMS.insert currentMod (currentQual ++ (show ctr)) importHM

showRoutePiece :: RoutePiece UnparsedPiece -> String
showRoutePiece routePiece = 
  case routePiece of
    Static pieceTxt -> T.unpack pieceTxt
    Dynamic unpPiece -> T.unpack unpPiece

-- [((r1,POST), ContractInfo {}), ((r2, GET), ContractInfo {}), ((r3, GET), ContractInfo {}), ((r4, GET), ContractInfo {}), ((r1,GET), ContractInfo {})]

-- constructRouteNameFromRouteList :: Route UnparsedPiece -> String
-- constructRouteNameFromRouteList = undefined

-- getModuleQualNameFromProvenance :: SG.Module -> (String, String)
-- getModuleQualNameFromProvenance provenanceMod =
--   case provenanceMod of
--     Global -> (globalTypesHsModuleName ++ globalDefnsModuleName, globalDefnsQualName)
--     Local routeInfo stdMethodName -> 
--       let qualName = getQualOfRoute (getRoute routeInfo) (Just stdMethodName)  
--       in (constructLocalTypeModuleName routeInfo (Just stdMethodName), qualName)
--     RouteLocal routeInfo -> 
--       let qualName = getQualOfRoute (getRoute routeInfo) Nothing
--       in (constructLocalTypeModuleName routeInfo Nothing, qualName)

--  where
--   constructLocalTypeModuleName :: Route UnparsedPiece -> Maybe Method -> String
--   constructLocalTypeModuleName routeInfo mMethodName =
--     let routeModuleName = parseRouteIntoModuleName routeInfo
--     in 
--       case mMethodName of
--         Just stdMethodName -> 
--           (localRouteMethodTypesModName routeModuleName stdMethodName) ++ localRouteMethodTypesModuleName
--         Nothing -> 
--           (routeLevelTypesModName routeModuleName) ++ routeLevelTypesModuleName

--   parseRouteIntoModuleName :: Route UnparsedPiece -> String
--   parseRouteIntoModuleName routeInfo =
--     case getRoute routeInfo of
--       [] -> error $ "Encountered empty Route! Expected the route to contain atleast one piece!"
--       routeInfoList -> DL.intercalate "." $ fmap (validateRouteModuleName . showRoutePiece) routeInfoList  

generateModulesFromTypeState :: TypeState -> FilePath -> ModuleState -> [InstanceTemplate] -> IO (Set.Set (String, String) )
generateModulesFromTypeState tState genPath moduleStateHM instanceTemplateList = do
  let moduleTypesHM = parseTypeStateIntoModuleTypes tState moduleStateHM
  let singleModuleResults = fmap parseSingleModuleTypes $ HMS.toList moduleTypesHM
  importsList <- Prelude.mapM (\(moduleContents, impsForModule, (modulePath, fName), errTxts ) -> do
        createDirectoryIfMissing True modulePath
        writeFile (modulePath ++ fName) (prettyPrint moduleContents) 
        putStrLn $ T.unpack errTxts
        pure impsForModule ) singleModuleResults 
  pure $ Set.fromList importsList

 where
  parseSingleModuleTypes :: (SG.Module, ([TypeDefinition], Imports) ) -> (LHE.Module SrcSpanInfo, (String, String), (String, String), T.Text )
  parseSingleModuleTypes (sgModule, (typeDefnsList, importsHM) ) = 
    let (relativeModulePath, hsFileName, _) = getModulePathAndQualInfo moduleStateHM sgModule
        typesModuleDir = genPath ++ relativeModulePath
        (modName, modQualName) = getModQualInfoModuleState moduleStateHM sgModule
        (declAcc, errMsgs, userImports) = (DL.foldl' (modAndTypesToHSEModule sgModule moduleStateHM) ([],"", []) typeDefnsList)
        uqUserImports = DL.nub userImports
        importsWithUserImports = importsForTypesModule ++ (fmap T.unpack uqUserImports)
        hseModule = 
          Module noSrcSpan 
            (Just $ ModuleHead noSrcSpan (ModuleName noSrcSpan modName) Nothing Nothing)
            (fmap languageExtension languageExtensionsForTypesModule)
            (fmap moduleImport 
              ( (DL.zip importsWithUserImports (cycle [(False, Nothing)]) ) 
                ++ (qualTyModuleImports (HMS.toList importsHM) ) )) 
            (declAcc)
    in (hseModule, (modName, modQualName), (typesModuleDir, hsFileName), errMsgs)
    
    
  modAndTypesToHSEModule :: SG.Module -> ModuleState -> ([Decl SrcSpanInfo], T.Text, [T.Text]) -> TypeDefinition -> ([Decl SrcSpanInfo], T.Text, [T.Text])
  modAndTypesToHSEModule sgModule modState (declAcc, errAcc, userImportsAcc) tyDefn = 
    let (newDataDecls, errMsgs, userImports) =  constructDeclFromCustomType sgModule tyDefn modState tState instanceTemplateList
        -- map over typeInstances, lookup and get InstanceTemplates, construct instanceTemplates, add to Decl List
    in (declAcc ++ newDataDecls, errAcc <> errMsgs, userImports ++ userImportsAcc)


  qualTyModuleImports :: [(String, String)] -> [(String, (Bool, Maybe (ModuleName SrcSpanInfo)))]
  qualTyModuleImports conditionalImportList =
    let qualImportList = qualifiedImportsForTypesModule ++ conditionalImportList
    in fmap (\(fullModuleName, qual) ->  (fullModuleName, (True, Just $ ModuleName noSrcSpan qual)) ) qualImportList

lookupInstance :: T.Text -> Instance -> [InstanceTemplate] -> Either T.Text InstanceTemplate
lookupInstance tyCons inst instanceTemplateList = 
  case DL.find isMatchingInstanceTemplate instanceTemplateList of
    Just template -> Right template
    -- TODO : Inform user that this produced no result and that an Instance Template has to be written!
    Nothing -> -- {instanceType = ""{-ParamInstance (QueryParam) -}, className ="", methods = []}
      Left $ "\n\nERROR: Did not find an Instance Template for the Instance that we are trying to construct! "
        <> "\nInstance: " <> (T.pack $ show inst)
        <> "\nType Name: " <> tyCons
        -- <> "\nInstanceTemplates: " <> (T.pack $ show instanceTemplateList) 


 where
  isMatchingInstanceTemplate :: InstanceTemplate -> Bool
  isMatchingInstanceTemplate currentInstTemplate = 
    case getInstFromInstanceInfo (instanceType currentInstTemplate) of
      Just matchedInst -> matchedInst == inst
      Nothing -> False

  getInstFromInstanceInfo :: InstanceInfo -> Maybe Instance
  getInstFromInstanceInfo instInfo = 
    let httpMediaType = createHttpMediaType $ mediaType instInfo
    in case instanceOf instInfo of
        "QueryParam" -> Just $ ParamInstance QueryParam 
        "FormParam" -> Just $ ParamInstance FormParam
        "BodyParam" -> Just $ ParamInstance (BodyParam httpMediaType) 
        "FileParam" -> Just $ ParamInstance FileParam
        "ToHeader" -> Just $ ParamInstance HeaderParam
        "ApiOutput" -> Just $ OutputInstance (ApiOutput httpMediaType)
        "ApiError" -> Just $ OutputInstance (ApiError httpMediaType)
        "HeaderOutput" -> Just $ OutputInstance HeaderOutput
        _ -> Nothing -- Placeholder for an unmatching instance

  createHttpMediaType :: Maybe MediaType -> [HTTP.MediaType] 
  createHttpMediaType (Just (MediaType mTy mSubTy)) = 
    case mSubTy of 
      Just subTy -> [(HTTP.//) (encodeUtf8 mTy) (encodeUtf8 subTy)]
      Nothing -> [(HTTP.//) (encodeUtf8 mTy) ""]
  createHttpMediaType Nothing = []
  
validateRouteDirectoryPath :: String -> String
validateRouteDirectoryPath = id

validateRouteModuleName :: String -> String
validateRouteModuleName = id
    

-- NOTE : Do NOT call this function.
-- This function should only be called from parseDataConsRef AND getModuleImportsForRouteState
-- All other calls to get Imports should get them from the ImportsHM
getRefImports :: Ref -> ModuleState -> [(String, String)]
getRefImports ref moduleStateHM = 
  case ref of
    (Inline _ prim) -> 
      case prim of
        Date -> [("Data.Time.Calendar", "P")]
        DateTime -> [("Data.Time.Clock", "P")]
        Password -> error "Encountered Password! Import required?"
        Byte -> [("Data.ByteString", "P")]
        Binary -> [("Data.ByteString", "P")]
        Text -> [("Data.Text", "P")]
        Float -> [("Prelude","P")]
        Double -> [("Prelude","P")]
        Number -> [("CommonTypes", "P")]
        Int -> [("Prelude","P")]
        Int32 -> [("Data.Int", "P")]
        Int64 -> [("Data.Int", "P")]
        Bool -> [("Prelude","P")]
        File -> []
        Null -> []
        Default innerRef -> ("CommonTypes", "P"):(getRefImports innerRef moduleStateHM) 
        Maybe innerRef -> ("Prelude","P"):getRefImports innerRef moduleStateHM
        Array innerRef -> getRefImports innerRef moduleStateHM
        Tuple innerRefList -> DL.concat $ fmap (\inRef -> getRefImports inRef moduleStateHM) innerRefList 
        MultiSet innerRef -> ("WebApi.Param","P"):getRefImports innerRef moduleStateHM
        DelimitedCollection _ innerRef  -> ("CommonTypes", "P"):(getRefImports innerRef moduleStateHM) 
    (Ref typeMeta ) -> 
      let sgMod = getModuleFromTypeMeta typeMeta
      in [getModQualInfoModuleState moduleStateHM sgMod]





-- customTypeForProdTy :: CustomType
-- customTypeForProdTy =
--   CustomType "Foo" [DataConstructor "Foo" [(Just "rec1", Inline Text), (Just "rec2",Inline Bool)] ]

-- customTypeForEnum :: CustomType
-- customTypeForEnum =
--     CustomType "SomeFooEnum" [DataConstructor "Available" [] , DataConstructor "Pending" [], DataConstructor "Cancelled" []]

-- customTypeForComplexSumTy :: CustomType
-- customTypeForComplexSumTy = 
--   let routeDefn = Route {getRoute = [Static "getStatus", Dynamic "withCons"]}
--       dCons1 = DataConstructor "AvailableCons" [(Nothing, Ref (Local  routeDefn GET ["Available"]) "defName1" "AvlTyName" )]
--       dCons2 = DataConstructor "PendingCons" [(Nothing, Ref (Local  routeDefn GET ["Pending"]) "defName2" "AvlTyName" )]
--   in CustomType "ComplexSumTy" [dCons1, dCons2]

constructDeclFromCustomType :: SG.Module 
                            -> TypeDefinition 
                            -> ModuleState 
                            -> TypeState 
                            -> [InstanceTemplate] 
                            -> ([Decl SrcSpanInfo], T.Text, [T.Text])
constructDeclFromCustomType sgModule typeDefn moduleStateHM typeStateHM instanceTemplateList = 
  let customTy = customHaskType typeDefn
      derivingList = fmap (T.unpack) $ derivingConstraints typeDefn
      typeInstances = DL.nub $ instances typeDefn
  in case customTy of
      CustomType tyConstructor dataConsList -> 
        let (qualConDecls, instDeclsWithUserImports, errMsgs) = qualConDeclsWithInstDecls sgModule tyConstructor (Left dataConsList) typeInstances
            currentDataDecl = DataDecl noSrcSpan  
                                (dataOrNewType False)
                                Nothing 
                                (declHead $ T.unpack tyConstructor)
                                (qualConDecls)
                                (derivingDecl derivingList)
            (instDecls, userImports) = DL.unzip instDeclsWithUserImports
        in (currentDataDecl:instDecls, errMsgs, DL.concat userImports)
      CustomNewType tyConstructor dConsName mRec ref  ->
        let (qualConDecls, instDeclsWithUserImports, errMsgs) = (qualConDeclsWithInstDecls sgModule tyConstructor (Right (dConsName, mRec, ref) ) typeInstances)
            currentDataDecl = DataDecl noSrcSpan  
                                (dataOrNewType True)
                                Nothing 
                                (declHead $ T.unpack tyConstructor)
                                (qualConDecls)
                                (derivingDecl derivingList)
            (instDecls, userImports) = DL.unzip instDeclsWithUserImports
        in (currentDataDecl:instDecls, errMsgs, DL.concat userImports)
        
 where 
  dataOrNewType :: Bool -> DataOrNew SrcSpanInfo
  dataOrNewType isNewType =
    if isNewType
    then NewType noSrcSpan
    else DataType noSrcSpan

  declHead :: String -> DeclHead SrcSpanInfo
  declHead declHeadName = DHead noSrcSpan (nameDecl declHeadName) 

  qualConDeclsWithInstDecls :: SG.Module 
                            -> TypeConstructor
                            -> Either [DataConstructor] (T.Text, Maybe RecordName, Ref) 
                            -> [Instance]
                            -> ([QualConDecl SrcSpanInfo], [(Decl SrcSpanInfo, [T.Text])], T.Text)
  qualConDeclsWithInstDecls currentModuleInfo tyCons dataCons typeInstances = 
    case dataCons of
      Left dataConsList 
        | isProductType dataConsList ->
            -- TODO: We should check that the 2 lists are of equal lengths?
          let [DataConstructor dConsName mRecRefList] = dataConsList
              typeNames = fmap (showRefTyWithQual moduleStateHM typeStateHM (Just currentModuleInfo)) $ snd $ DL.unzip mRecRefList
              recVals = catMaybes $ fst $ DL.unzip mRecRefList
              recordNamesWithTypes = DL.zip (fmap T.unpack recVals) typeNames
              fieldDecls = fmap fieldDecl recordNamesWithTypes
              qualDecl = 
                [QualConDecl noSrcSpan Nothing Nothing 
                  (RecDecl noSrcSpan (nameDecl $ T.unpack dConsName) fieldDecls)]                
              (errMessages, instDecls) = partitionEithers $ fmap (constructInstDecl instanceTemplateList dConsName mRecRefList) typeInstances
          in (qualDecl, instDecls, T.concat errMessages)
        -- Enum Type
        | isEnumType dataConsList ->
          let qualDecls = 
                fmap (\(DataConstructor dConsName mRecRefList) -> 
                          let typeConsList = fmap (typeConstructor . (showRefTyWithQual moduleStateHM typeStateHM (Just currentModuleInfo) ) ) $ snd $ DL.unzip mRecRefList
                          in QualConDecl noSrcSpan Nothing Nothing 
                              (ConDecl noSrcSpan (nameDecl $ T.unpack dConsName) typeConsList) ) dataConsList
              (errMessages, instDecls) = partitionEithers $ fmap (tempInstDeclForSums instanceTemplateList tyCons) typeInstances
              -- instDecls = []
              -- errMessages = ""
          in (qualDecls, instDecls, T.concat errMessages)
          -- Sum Type
        | otherwise  -> 
          let qualDecls = 
                fmap (\(DataConstructor dConsName mRecRefList) -> 
                          let typeConsList = fmap (typeConstructor . (showRefTyWithQual moduleStateHM typeStateHM (Just currentModuleInfo) ) ) $ snd $ DL.unzip mRecRefList
                          in QualConDecl noSrcSpan Nothing Nothing 
                              (ConDecl noSrcSpan (nameDecl $ T.unpack dConsName) typeConsList) ) dataConsList
              (errMessages, instDecls) = partitionEithers $  fmap (tempInstDeclForSums instanceTemplateList tyCons) typeInstances
              -- instDecls = []
              -- errMessages = ""
          in (qualDecls, instDecls, T.concat errMessages)
      Right (dConsTxt, mRecName, refTy) ->  
        let recTy = showRefTyWithQual moduleStateHM typeStateHM (Just currentModuleInfo) refTy
            qualDecls =
              case mRecName of
                Just recName ->
                  [QualConDecl noSrcSpan Nothing Nothing 
                        (RecDecl noSrcSpan (nameDecl $ T.unpack dConsTxt) 
                            [fieldDecl (T.unpack recName, recTy)] )]
                Nothing ->
                  let tyConsName = typeConstructor recTy
                  in [QualConDecl noSrcSpan Nothing Nothing 
                      (ConDecl noSrcSpan (nameDecl $ T.unpack dConsTxt) [tyConsName])]
        in (qualDecls, [], "")

   where
    isProductType :: [DataConstructor] -> Bool
    isProductType dCons = 
      case dCons of
        [singleDCon] -> hasRecordField singleDCon
        _ -> False

    hasRecordField :: DataConstructor -> Bool
    hasRecordField (DataConstructor _ recNameRefList) = 
      not $ DL.null $ catMaybes $ fst $ DL.unzip recNameRefList

    isEnumType :: [DataConstructor] -> Bool
    isEnumType dCons =
      let ctorList = DL.concat $ fmap (\(DataConstructor _ recNameRefList) -> recNameRefList ) dCons
      in case ctorList of
          [] -> True
          _ -> False
    
    tempInstDeclForSums :: [InstanceTemplate] -> T.Text -> Instance -> Either T.Text (Decl SrcSpanInfo, [T.Text])
    tempInstDeclForSums instTemplateList tyConsTxt currentInst = 
      case lookupInstance tyConsTxt currentInst instTemplateList of
        Left errMsg -> Left errMsg
          
        Right instTmp -> 
          let instHead = instanceHeadTxt instTmp tyConsTxt 
              instWhere = " where"
              instMethods = 
                T.concat $ fmap (\methodTemp -> "\n  " <> methodName methodTemp <> " = P.undefined" ) (methods instTmp)
              parseModeWithExts = LHE.defaultParseMode {LHE.extensions = [LHE.EnableExtension LHE.DataKinds, LHE.EnableExtension LHE.MultiParamTypeClasses]}
          in 
            case LHE.parseDeclWithMode parseModeWithExts (T.unpack $ instHead <> instWhere <> instMethods ) of
              LHE.ParseOk decl -> 
                case decl of
                  LHE.InstDecl _ _ _ _ -> 
                    Right (decl, [])
                  _ -> Left $ "Not an InstDecl"

              LHE.ParseFailed srcLoc errString -> 
                Left $ "\n\nERROR: Failed while parsing an Instance text! "
                  <> "\nLocation: " <> (T.pack $ show srcLoc)
                  <> "\nError Message: " <> (T.pack errString)
                  <> "\n" <> instHead
                  <> "\nPlease verify that the Instance Template/Instance Text is correct!"
        -- _ -> Left "Output Instance encountered. Not generating it for now."


    instanceHeadTxt :: InstanceTemplate -> T.Text -> T.Text
    instanceHeadTxt instTmp tyCon =
      "instance " <> (className instTmp) <> " " <> tyCon 

    constructInstDecl :: [InstanceTemplate] -> DataConstructorName -> [(Maybe RecordName, Ref)] -> Instance -> Either T.Text (Decl SrcSpanInfo, [T.Text]) 
    constructInstDecl instTemplateList dConsName mRecRefList currentInst = 
      case lookupInstance dConsName currentInst instTemplateList of
        Left errMsg -> Left errMsg
          
        Right instTmp -> 
          let ctor = dConsName
              tyCon = tyCons
              fields = fmap (refListToField tyCons) mRecRefList  
              varName = "val"
              instanceCode = 
                instanceHeadTxt instTmp tyCon
                  <> " where\n  "
                  <> (T.concat $ fmap (showInstanceMethod ctor varName fields) (methods instTmp) )
              parseModeWithExts = LHE.defaultParseMode {LHE.extensions = [LHE.EnableExtension LHE.DataKinds, LHE.EnableExtension LHE.MultiParamTypeClasses]}
              userImports = importNames instTmp
          in 
            case LHE.parseDeclWithMode parseModeWithExts (T.unpack instanceCode) of
              LHE.ParseOk decl -> 
                case decl of
                  LHE.InstDecl _ _ _ _ -> 
                    -- appendFile "/Users/kahlil/projects/ByteAlly/webapi/webapi-swagger/sampleFiles/Sample.hs" (LHE.prettyPrint decl)
                    Right (decl, userImports)
                  _ -> -- error $ 
                    Left $ "\n\nERROR: The decl we parsed is NOT an InstDecl! "
                        <> "Expected an InstDecl as we are parsing a Custom Instance! "
                        <> "\nGot: " <> (T.pack $ show decl)
              LHE.ParseFailed srcLoc errString -> 
                Left $ "\n\nERROR: Failed while parsing an Instance text! "
                  <> "\nLocation: " <> (T.pack $ show srcLoc)
                  <> "\nError Message: " <> (T.pack errString)
                  <> "\n" <> instanceCode
                  <> "\nPlease verify that the Instance Template/Instance Text is correct!"
     where
      showInstanceMethod :: T.Text -> T.Text -> [(T.Text, T.Text)] -> MethodTemplate -> T.Text
      showInstanceMethod ctor varName fields methodTemplate = 
        (methodName methodTemplate) <> " = "  <> (body methodTemplate)
                        <> " \\" <> varName <> " -> "
                        <> T.intercalate (" \n      " <> (fieldCombinator methodTemplate)
                                              <> " ")
                                        ( showCtorAndFieldTemplates (ctorTemplate methodTemplate ctor ctor) 
                                              (DL.map (\(x, y) -> fieldTemplate methodTemplate x y varName) fields)
                                        )
                                      
      showCtorAndFieldTemplates :: Maybe T.Text -> [T.Text] -> [T.Text]                                    
      showCtorAndFieldTemplates mCtorTemplate fieldTemplateList =
        case  mCtorTemplate of
          Just ctorText -> ctorText:fieldTemplateList
          Nothing -> fieldTemplateList

      refListToField :: T.Text -> (Maybe RecordName, Ref) -> (T.Text, RecordName) 
      refListToField tyConsTxt (mRecName, ref) = 
        let fjnErr = "Expected RecordName in DataConstructor of a Product Type!"
                        ++ "\nGot a Nothing! Debug Info: "
                        ++ "\nData Cons Name: " ++ (show dConsName) 
                        ++ "\nRecord and Ref List: " ++ (show mRecRefList)
            recName = fromJustNote fjnErr mRecName
            refTy = T.pack $ showRefTyWithQual moduleStateHM typeStateHM (Just currentModuleInfo) ref
            recNameWithSig::T.Text = "(" <> recName <> " :: " <> tyConsTxt <> " -> " <> refTy <> ")"
            defnName = 
              case ref of
                (Ref (Definition _ defName)) -> defName
                (Inline defName _) -> defName 
        in (defnName, recNameWithSig)

-- -- Product Types
-- [QualConDecl noSrcSpan Nothing Nothing (RecDecl noSrcSpan (nameDecl constructorName) fieldDecls )]

-- -- FOR Complex Sum Types -> A List of these
-- QualConDecl noSrcSpan Nothing Nothing 
-- (ConDecl noSrcSpan (nameDecl cName) [typeConstructor consType])

-- -- For Enums
-- fmap (\construcorVal -> QualConDecl noSrcSpan Nothing Nothing 
--       (ConDecl noSrcSpan 
--         (nameDecl construcorVal) [] ) )

showRefTyWithQual :: ModuleState -> TypeState -> Maybe SG.Module -> Ref -> String
showRefTyWithQual moduleStateHM typeStateHM mModule (Inline _ prim) = 
  let (qual, ty) = showPrimitiveTy (Just moduleStateHM) (Just typeStateHM) mModule prim
  in addBracesIfNested (qual ++ ty) (isNestedType prim)
showRefTyWithQual moduleStateHM typeStateHM mModule (Ref typeMeta) = 
  let sgModule = getModuleFromTypeMeta typeMeta
      tyCon = getTypeConstructor $ lookupTypeStateGetCustomHaskType typeMeta typeStateHM
  in printRefWithQual moduleStateHM sgModule mModule tyCon

showRefTy :: Maybe SG.Module -> Maybe TypeState -> Ref -> String
showRefTy mModule mTyState (Inline _ prim) = addBracesIfNested (snd $ showPrimitiveTy Nothing mTyState mModule prim) (isNestedType prim) 
showRefTy _ mTyState (Ref typeMeta) = 
  let typeStateHM = fromJustNote ("Expected a TypeState but got a Nothing! Type Meta: " ++ (show typeMeta) ) mTyState
  in printRef (getTypeConstructor $ lookupTypeStateGetCustomHaskType typeMeta typeStateHM)

showPrimitiveTy :: Maybe ModuleState -> Maybe TypeState -> Maybe SG.Module -> Primitive -> (String, String)
showPrimitiveTy mModuleState mTypeState mModule prim =
  case prim of
    Date -> ("P.", "Day")
    DateTime -> ("P.", "UTCTime")
    Password -> ("", "PASSWORD (TODO)")
    Byte -> ("P.", "ByteString" )
    Binary -> ("P.", "ByteString")
    Text -> ("P.", "Text")
    Float -> ("P.", "Float")
    Double -> ("P.", "Double")
    Number -> ("P.", "SwaggerNumber")
    Int -> ("P.", "Int")
    Int32 -> ("P.", "Int32")
    Int64 -> ("P.", "Int64")
    Bool -> ("P.", "Bool")
    File -> ("P.", "File")
    Null -> ("","()")
    -- TODO: What to do about qual of inner types? i.e. P. qual
    Default innerRef -> ("P.","Default " ++ (getRefOrRefWithQual mModuleState mTypeState mModule innerRef) ) -- (maybe (showRefTy mModule innerRef) (\modState -> showRefTyWithQual modState mModule innerRef) mModuleState) )
    Maybe innerRef -> ("P." ,"Maybe " ++ (getRefOrRefWithQual mModuleState mTypeState mModule innerRef) )
    Array innerRef -> ("","[" ++ (getRefOrRefWithQual mModuleState mTypeState mModule innerRef) ++ "]" )
    Tuple innerRefList -> ("","(" ++ (DL.intercalate "," (fmap (\iRef -> getRefOrRefWithQual mModuleState mTypeState mModule iRef) innerRefList) ) ++ ")")
    MultiSet innerRef -> ("P.","MultiSet " ++ (getRefOrRefWithQual mModuleState mTypeState mModule innerRef) )
    DelimitedCollection delimiter innerRef  -> 
      let delimChar = 
            case delimiter of
              SlashT -> "\"\t\" "
              Space -> "\" \"" 
              Pipe -> "\"|\""
              Comma -> "\",\" "
      in ("P.","DelimitedCollection" ++ delimChar ++ (showRefTy mModule mTypeState innerRef))

 where
  getRefOrRefWithQual :: Maybe ModuleState -> Maybe TypeState -> Maybe SG.Module -> Ref -> String
  getRefOrRefWithQual mModState mTyState mMod passedRef = 
    case (mModState, mTyState) of
      (Just modState, Just tyState) -> showRefTyWithQual modState tyState mMod passedRef
      (Nothing, Nothing) -> showRefTy mModule mTyState passedRef
      _ -> error $ "Expected either both ModuleState and TypeState to be present or none to be present."
              ++ "\nGot only one of two present! Debug Info: ModuleState : " ++ (show mModState) 
              ++ "\nTypeState: " ++ (show mTyState) 
              ++ "\nRef : " ++ (show passedRef)

isNestedType :: Primitive -> Bool
isNestedType primVal = 
-- ref = 
--   case ref of
--     (Inline _ primVal) -> 
    case primVal of
      Date -> False
      DateTime -> False
      Password -> False
      Byte -> False
      Binary -> False
      Text -> False
      Float -> False
      Double -> False
      Number -> False
      Int -> False
      Int32 -> False
      Int64 -> False
      Bool -> False
      File -> False
      Null -> False
      Default _ -> True 
      Maybe _ -> True
      Array _ -> True
      Tuple _List -> True
      MultiSet _ -> True
      DelimitedCollection _ _  -> True
    -- (Ref _ ) -> False

addBracesIfNested :: String -> Bool -> String
addBracesIfNested strVal isNested = 
  case isNested of
    True -> "(" ++ strVal ++ ")"
    False -> strVal

printRef :: T.Text -> String
printRef typeTxt = T.unpack typeTxt 
    
printRefWithQual :: ModuleState -> SG.Module -> Maybe SG.Module -> T.Text -> String
printRefWithQual moduleStateHM sgModule mCurrentSgModule typeTxt = 
  case mCurrentSgModule of
    Just sgMod ->
        case isSameModule sgModule sgMod of 
          True -> T.unpack typeTxt
          False ->
            let (_, modQualName) = getModQualInfoModuleState moduleStateHM sgModule
            in modQualName ++ "." ++ (printRef typeTxt)
    Nothing -> 
      let (_, modQualName) = getModQualInfoModuleState moduleStateHM sgModule
      in  modQualName ++ "." ++ (printRef typeTxt)
      


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

fieldDecl :: (String, String) -> FieldDecl SrcSpanInfo
fieldDecl (fieldName, fieldType) = 
  FieldDecl noSrcSpan [nameDecl fieldName] (TyCon noSrcSpan (UnQual noSrcSpan (nameDecl fieldType)))

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



routeDeclaration :: ModuleState -> TypeState -> (String, Route Ref) -> Decl SrcSpanInfo
routeDeclaration moduleStateHM typeStateHM (routeNameStr, routePathComponents) = 
  let routePieceList = getRoute routePathComponents
  in 
    case routePieceList of
      [] -> -- TODO: This needs discussion and a possible change.
        TypeDecl noSrcSpan
          (declarationHead routeNameStr)
          (TyApp noSrcSpan
            (typeConstructor "W.Static")
            (recursiveTypeForRoute routePieceList) )

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

 where
  recursiveTypeForRoute :: [RoutePiece Ref] -> Type SrcSpanInfo
  recursiveTypeForRoute routeRefComponents = 
    case routeRefComponents of
      [] -> promotedType "BaseRoute" -- TODO: This needs discussion and a possible change.
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
  processPathComponent :: RoutePiece Ref -> Type SrcSpanInfo
  processPathComponent refRoutePiece = 
    case refRoutePiece of
      Static staticPathPiece -> promotedType $ T.unpack staticPathPiece
      Dynamic pType -> typeConstructor $ showRefTyWithQual moduleStateHM typeStateHM Nothing pType

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


#if MIN_VERSION_haskell_src_exts(1,20,0)
derivingDecl :: [String] -> [Deriving SrcSpanInfo]
derivingDecl derivingList = [Deriving noSrcSpan Nothing $ fmap iRule derivingList]

#else
derivingDecl :: [String] -> Maybe (Deriving SrcSpanInfo)
derivingDecl derivingList = Just $ Deriving noSrcSpan $ fmap iRule derivingList
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





parseTextIntoInstDecl :: T.Text -> Decl SrcSpanInfo
parseTextIntoInstDecl instanceTxt = 
  case parseDecl (T.unpack instanceTxt) of
    ParseOk decl -> 
      case decl of
        InstDecl _ _ _ _ -> decl
        _ -> error $ "The decl we parsed is NOT an InstDecl! "
          ++ "Expected an InstDecl as we are parsing a Custom Instance! "
          ++ "Got: " ++ (show decl)
    ParseFailed srcLoc errString -> 
      error $ "Failed while parsing an Instance text! "
        ++ "\nLocation: " ++ (show srcLoc)
        ++ "\nError Message: " ++ errString
        ++ "\nPlease verify that the Instance Template/Instance Text is correct!"
