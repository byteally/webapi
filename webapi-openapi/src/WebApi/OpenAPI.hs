{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

{-# OPTIONS_GHC -Wno-type-defaults #-}
module WebApi.OpenAPI where

import Data.ByteString.Lazy as B (readFile)
import Data.Aeson ( decode )
import Data.OpenApi
    ( Components(_componentsSchemas, _componentsParameters, _componentsRequestBodies, _componentsResponses, _componentsHeaders),
      OpenApi(_openApiComponents, _openApiPaths, _openApiInfo),
      OpenApiItems(OpenApiItemsArray, OpenApiItemsObject),
      OpenApiType(..),
      PathItem(PathItem),
      Reference(Reference),
      Referenced(..),
      Schema(Schema,_schemaType, _schemaFormat, _schemaItems, _schemaRequired,
             _schemaProperties, _schemaOneOf),
      Definitions,
      Param(_paramName, _paramSchema, _paramIn),
      Operation(_operationParameters, _operationRequestBody, _operationResponses),
      ParamLocation(ParamHeader, ParamCookie, ParamQuery),
      Info(_infoTitle),
      RequestBody(_requestBodyContent),
      MediaTypeObject(_mediaTypeObjectSchema),
      Response(_responseContent,_responseHeaders),
      Header(_headerSchema),
      Responses(_responsesResponses,_responsesDefault,Responses))
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
import Data.HashMap.Strict.InsOrd as HMO (toList,lookup, empty, fromList, delete, InsOrdHashMap, elems)
import Data.Text as T ( unpack, Text, append, splitAt, toUpper, take, pack, dropEnd, concat, split, toLower, breakOnEnd)
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
import Data.List as L (delete, nub)
import Data.HashMap.Internal as HM (HashMap, singleton, unions, lookup, empty, insert, union, toList, fromList)
import Language.Haskell.TH (Extension(..))
import Test.FitSpec.Utils(contained)
import Network.HTTP.Media ( MediaType , (//), (/:), parseAccept)
import Data.Maybe ( fromMaybe, catMaybes )


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
        compReqBodies = _componentsRequestBodies . _openApiComponents $ oApi
        compResponses = _componentsResponses . _openApiComponents $ oApi
        compHeaders = _componentsHeaders . _openApiComponents $ oApi
        oApiName = _infoTitle . _openApiInfo $ oApi
        modelList = evalState
                        (mapM (\(x,y) -> createModelData (_schemaType y) (x,y)) (HMO.toList compSchemas))
                        (ModelGenState S.empty S.empty (S.fromList keywords) HM.empty)
        hsModuleModel = module' (Just modName) Nothing impsModel (namedTy: Prelude.concat modelList)
        (routeInfo,typeSynList) = bimap Prelude.concat Prelude.concat . unzip $
                                      evalState
                                        (mapM (createTypeSynData compParams compReqBodies compResponses compHeaders) (HMO.toList . _openApiPaths $ oApi))
                                        (ModelGenState (S.fromList seenVariables) S.empty S.empty HM.empty)
        simplifiedRouteInfo = (fmap . fmap) (map fst) routeInfo
        apiContractInstances =  Prelude.concat $ mkApiContractInstances oApiName <$> routeInfo
        hsModuleTypeSyn = module' (Just typeSynName) Nothing impsTypeSyn (untypedDef:typeSynList ++ webApiInstance oApiName simplifiedRouteInfo ++ apiContractInstances)
    writeModule (pkgHome </> "src") (modName <.> "hs") es hsModuleModel
    writeModule (pkgHome </> "src") (typeSynName <.> "hs") es2 hsModuleTypeSyn
    writeCabal pkgName mkPkgConfig modName [typeSynName] pkgHome

    where impsModel =
                 [ import' "Data.Int"
                 , import' "Data.Vector"
                 , exposing (import' "GHC.Types") [var "Symbol"]
                 , exposing (import' "Data.Text") [var "Text"]
                 ]
          impsTypeSyn =
                 [ import' "WebApi.Contract"
                 , import' modName
                 , import' "Data.Int"
                 , exposing (import' "GHC.Types") [var "Symbol"]
                 , exposing (import' "Data.Text") [var "Text"]
                 , import' "Data.Vector"
                 ]
          namedTy = newtype' ":::" [kindedVar "fld" (var "Symbol") , bvar "a"]
                        (prefixCon "Field" [field $ var "a"]) []
          es = [TypeOperators,KindSignatures,DataKinds,DuplicateRecordFields]
          es2 = [DataKinds,TypeOperators,TypeSynonymInstances,FlexibleInstances,MultiParamTypeClasses,TypeFamilies]
          keywords = ["type","class"]
          seenVariables = ["Untyped"]
          pkgName = T.unpack . flip T.append "-models" . T.pack . dropExtension . takeFileName $ fp
          pkgHome = destFp </> pkgName
          modName = "OpenApiModels"
          typeSynName = "WebApiInstances"
          dataTypeForOApi a = data' (textToOccNameStr . upperFirstChar $ a) [] [] []
          webApiInstance a b = [dataTypeForOApi a,
                                    instance' (var "WebApi" @@ var (textToRdrNameStr a))
                                              [tyFamInst "Apis" [var $ textToRdrNameStr a] (listPromotedTy (oneRoute <$> b))]]
          oneRoute (tName,methList) = var "Route" @@ listPromotedTy (var . textToRdrNameStr <$> methList) @@ var  (textToRdrNameStr tName)
          untypedDef = data' "Untyped" [] [prefixCon "Maybe" [field (var "Text")]] []

collectSchemasForSerializing :: 
    Definitions RequestBody -> 
    Definitions Response ->  
    PathItem -> 
    [(ContentTypesOApi,Text)]
collectSchemasForSerializing compReqBodies compResponses (PathItem _ _ piGet piPut piPost piDelete piOptions piHead piPatch piTrace _ _) =
    nub . Prelude.concat $ 
        collectOperationSchemas compReqBodies compResponses <$> [piGet,piPut,piPost,piDelete,piOptions,piHead,piPatch,piTrace]

collectOperationSchemas :: 
    Definitions RequestBody -> 
    Definitions Response -> 
    Maybe Operation -> 
    [(ContentTypesOApi,Text)]
collectOperationSchemas _ _ Nothing = []
collectOperationSchemas compReqBodies compResponses (Just operationData) = nub $
                                               collectRequestSchemas compReqBodies (_operationRequestBody operationData)
                                               ++ collectResponseSchemas compResponses (_operationResponses operationData)

collectResponseSchemas :: Definitions Response -> Responses -> [(ContentTypesOApi, Text)]
collectResponseSchemas compResponses (Responses resDef resps) = 
    let mResps = case resDef of
                    Nothing -> HMO.elems resps
                    Just x -> x:HMO.elems resps
    in  Prelude.concat $ mediaTypeObjMapToSchema . _responseContent . refValToVal compResponses <$> mResps

collectRequestSchemas ::
    Definitions RequestBody
    -> Maybe (Referenced RequestBody) ->
    [(ContentTypesOApi, Text)]
collectRequestSchemas _ Nothing = []
collectRequestSchemas compReqBodies (Just reqBody) =
        mediaTypeObjMapToSchema (_requestBodyContent (refValToVal compReqBodies reqBody))

mediaTypeObjMapToSchema :: InsOrdHashMap MediaType MediaTypeObject -> [(ContentTypesOApi, Text)]
mediaTypeObjMapToSchema x = fetchJusts $ bimap findMediaType findMediaTypeObjSchema <$> HMO.toList x

findMediaType :: MediaType -> ContentTypesOApi
findMediaType x = case HM.lookup x mediaTypeMap of
                    Nothing -> error "Invalid MediaType"
                    (Just a) -> a

findMediaTypeObjSchema :: MediaTypeObject -> Maybe Text
findMediaTypeObjSchema x = case _mediaTypeObjectSchema x of
                                (Just (Ref (Reference a))) -> Just a
                                _ -> Nothing

mkApiContractInstances ::
    Text ->
    (Text, [(Text, (Maybe HsType', Maybe HsType', Maybe HsType', Maybe HsType',Maybe HsType',Maybe HsType', Maybe HsType'))]) ->
    [HsDecl']
mkApiContractInstances oApiName (typName,instanceInfo) =
    mkOneInstance <$> instanceInfo
    where mkOneInstance (methName,(headInfo,queryInfo,cookieInfo,reqBodyInfo,apiOutInfo,apiErrInfo,headerOutInfo)) =
                instance' (var "ApiContract" @@ var (textToRdrNameStr oApiName) @@ var (textToRdrNameStr methName) @@ var (textToRdrNameStr typName))
                          (Prelude.concat (mkTypeSyns methName <$> [("HeaderIn",headInfo)
                                                                   ,("QueryParam",queryInfo)
                                                                   ,("CookieIn",cookieInfo)
                                                                   ,("RequestBody",reqBodyInfo)
                                                                   ,("ApiOut",apiOutInfo)
                                                                   ,("ApiErr",apiErrInfo)
                                                                   ,("HeaderOut",headerOutInfo)
                                                                   ]))
          mkTypeSyns _ (_,Nothing) = []
          mkTypeSyns methName (tName,Just typ) = [tyFamInst (textToRdrNameStr tName)
                                                            [var (textToRdrNameStr methName),var (textToRdrNameStr typName) ]
                                                            typ]


createTypeSynData ::
    (MonadState ModelGenState m) =>
    Definitions Param ->
    Definitions RequestBody ->
    Definitions Response ->
    Definitions Header ->
    (FilePath,PathItem) ->
    m ([(Text, [(Text, (Maybe HsType', Maybe HsType', Maybe HsType', Maybe HsType',Maybe HsType',Maybe HsType', Maybe HsType'))])],[HsDecl'])
createTypeSynData compsParam compReqBodies compResponses compHeaders (fp,PathItem _ _ piGet piPut piPost piDelete piOptions piHead piPatch piTrace _ piParams) = do
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
    (typSyns,ct1) <- unzip <$> mapM createTypSynonym overridesParam
    (commonTypSyn,ct2) <- unzip <$> let diff = S.difference (pairLisToSet opList) (pairLisToSet overridesParam)
                              in if S.null diff
                              then return []
                              else do
                                 ((a,_b),c) <- createTypSynonym ("",commonParams)
                                 return [((a,S.toList diff),c)]
    (apiConInsData,ct3) <- bimap unions Prelude.concat . unzip
                                        <$> mapM (createApiContractInsData compsParam compReqBodies compResponses compHeaders paramsMap) opList
    return ((fmap . fmap) (applyApiContractInfo apiConInsData) <$> commonTypSyn ++ typSyns, Prelude.concat ct1 ++ Prelude.concat ct2 ++ ct3)
    where pairLisToSet = S.fromList . fmap fst
          applyApiContractInfo apiInfoMap a =
                    case HM.lookup a apiInfoMap of
                        Nothing -> error "No api info for this method"
                        (Just x) -> (a,x)

fetchJusts :: [(a, Maybe b)] -> [(a, b)]
fetchJusts =
    fmap justVal  . filter filterJusts
    where justVal = \case
                        (_,Nothing) -> error "Unexpected Value"
                        (a,Just b) -> (a,b)
          filterJusts = \case
                           (_,Nothing) -> False
                           _ -> True


createApiContractInsData ::
    (MonadState ModelGenState m) =>
    Definitions Param ->
    Definitions RequestBody ->
    Definitions Response ->
    Definitions Header ->
    HashMap Text (Bool, Param) ->
    (Text,Operation) ->
    m (HashMap Text (Maybe HsType',Maybe HsType',Maybe HsType', Maybe HsType',Maybe HsType',Maybe HsType', Maybe HsType'),[HsDecl'])
createApiContractInsData compsParam compsReqBodies compResponses compHeaders commonParamMap (opName,operationData) = do
    let opParamsMap = refParamsToParams compsParam (_operationParameters operationData)
        overrideParams = HM.toList $ opParamsMap `union` commonParamMap
        opReqBody = _operationRequestBody operationData
        opResponses = _responsesResponses . _operationResponses $ operationData
        responseList = fmap (refValToVal compResponses) <$> HMO.toList opResponses
        defaultResponse = _responsesDefault . _operationResponses $ operationData
        headerOutSchemas = headersToSchema <$>  filter (not . Prelude.null)
                                                    (responseToHeader <$> ( case defaultResponse of
                                                                        Nothing -> responseList
                                                                        (Just x) -> (0,refValToVal compResponses x):responseList))
    (headtypTuple,ct1)  <- createType ParamHeader overrideParams
    (querytypTuple,ct2)  <- createType ParamQuery  overrideParams
    (cookietypTuple,ct3) <- createType ParamCookie overrideParams
    (reqBody,ct4) <- createReqBody compsReqBodies opReqBody
    (apiOutType,ct5) <- createApiOut
                            compResponses
                            (findResponseApiOut opResponses)
                            defaultResponse
    (apiErrType,ct6) <- createApiErr compResponses defaultResponse (HMO.toList(findResponseApiErr opResponses))
    (headerOutType,ct7) <- createHeaderOut headerOutSchemas
    return ( HM.singleton opName (headtypTuple,querytypTuple,cookietypTuple,reqBody,apiOutType,apiErrType,headerOutType)
           , ct1 ++ ct2 ++ ct3 ++ ct4 ++ ct5 ++ ct6 ++ ct7)
    where mFilter x = filter (\(_a,(_b,c)) -> _paramIn c == x)
          createType a b = mkTypeTuple $ fmap (\ (_, x) -> _paramSchema x) <$> mFilter a b
          mkTypeTuple [] = return (Nothing,[])
          mkTypeTuple schemas = do
              (schemaList,childTypes) <-
                               unzip <$> mapM mayBeSchemaToHsType schemas
              let typeTuple =  tuple $ (\(x,y)-> op (stringTy $ T.unpack x) ":::" y) <$> schemaList
              return (Just typeTuple,Prelude.concat childTypes)
          responseToHeader (_,res) = fmap (_headerSchema . refValToVal compHeaders) <$> (HMO.toList . _responseHeaders $ res)
          findResponseApiOut hMap = case catMaybes [HMO.lookup x hMap | x <- [200..299]] of
                                        [] -> Nothing
                                        [x] -> Just x
                                        _ -> error "More than One ApiOut Type"
          findResponseApiErr hmap = foldr HMO.delete hmap [200..299]

createHeaderOut ::
    MonadState ModelGenState m =>
    [Referenced Schema] -> m (Maybe HsType', [HsDecl'])
createHeaderOut [] = return (Nothing,[])
createHeaderOut x = do
    ((_,typ),childTypes) <- mkSumType "HeaderOutSumType" True x
    return (Just typ,childTypes)

headersToSchema :: [(Text, Maybe (Referenced Schema))] -> Referenced Schema
headersToSchema [] = error "Impossible state"
headersToSchema hdrs =
    Inline $ let Schema { .. } = mkEmptySchema
             in mkEmptySchema { _schemaType = Just OpenApiObject
                              , _schemaRequired = fst <$> hdrs
                              , _schemaProperties = HMO.fromList $ fmap maySchemaToSchema <$> hdrs
                              }

createApiOut ::
    MonadState ModelGenState m =>
    Definitions Response ->
    Maybe (Referenced Response) ->
    Maybe (Referenced Response) ->
    m (Maybe HsType', [HsDecl'])
createApiOut _ Nothing Nothing = return (Just $ var "()",[])
createApiOut hMap Nothing x = createApiOut hMap x Nothing
createApiOut hMap (Just res) defRes = do
    let inlineResp = refValToVal hMap res
        mediaTypList = HMO.toList . _responseContent $ inlineResp
    if Prelude.null mediaTypList
    then case defRes of
            Nothing -> createApiOut hMap Nothing Nothing
            x -> createApiOut hMap x Nothing
    else do
        let (_,maySchema) =  mediaTypeObjToSchema mediaTypList
        ((_,typ),childTypes) <- mayBeSchemaToHsType ("ApiOutType",maySchema)
        return (Just typ,childTypes)

createApiErr ::
    MonadState ModelGenState m =>
    Definitions Response ->
    Maybe (Referenced Response) ->
    [(Int, Referenced Response)] -> m (Maybe HsType', [HsDecl'])
createApiErr _ Nothing [] = return (Just $ var "()",[])
createApiErr hMap (Just x) [] = createApiErr hMap Nothing [(0,x)]
createApiErr hMap defRes resList = do
    let resList' = case defRes of
                      Nothing -> resList
                      (Just x) -> (0,x):resList
        inlineResp = refValToVal hMap . snd <$> resList'
        mediaTypList =  HMO.toList . _responseContent  <$> inlineResp
    if Prelude.null (Prelude.concat mediaTypList)
    then createApiErr hMap Nothing []
    else do
        let neMediaTypList = maySchemaToSchema . snd . mediaTypeObjToSchema <$> filter (not . Prelude.null) mediaTypList
        ((_,typ),childTypes) <- mkSumType "ApiErrSumType" True neMediaTypList
        return (Just typ,childTypes)

mkEmptySchema :: Schema
mkEmptySchema = Schema Nothing Nothing [] Nothing Nothing Nothing Nothing Nothing HMO.empty  Nothing
                       Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing
                       Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing
                       Nothing Nothing Nothing Nothing Nothing

mayBeSchemaToHsType ::
    MonadState ModelGenState m =>
    (Text, Maybe (Referenced Schema)) -> m ((Text, HsType'), [HsDecl'])
mayBeSchemaToHsType (x,y) = parseRecordFields (x,maySchemaToSchema y) True

maySchemaToSchema ::
    Maybe (Referenced Schema) ->
    Referenced Schema
maySchemaToSchema Nothing = Ref (Reference "Untyped")
maySchemaToSchema (Just x) = x

createReqBody ::
    (MonadState ModelGenState m) =>
    Definitions RequestBody ->
    Maybe (Referenced RequestBody) ->
    m (Maybe HsType',[HsDecl'])
createReqBody _ Nothing = return (Nothing,[])
createReqBody compReqBodies (Just refReqBody) = do
    let reqBody = refValToVal compReqBodies refReqBody
    let (typName,maySchema) = mediaTypeObjToSchema . HMO.toList . _requestBodyContent $ reqBody
    ((_,desiredType),childTypes) <- mayBeSchemaToHsType ("requestBody",maySchema)
    let finalType = if typName == "JSON"
                    then listPromotedTy [desiredType]
                    else listPromotedTy [var "Content" @@ listPromotedTy [var $ textToRdrNameStr typName] @@ desiredType]
    return (Just finalType,childTypes)

mediaTypeObjToSchema :: [(MediaType, MediaTypeObject)] -> (Text, Maybe (Referenced Schema))
mediaTypeObjToSchema [(mediaTyp,mediaTypObj)] =
    case HM.lookup mediaTyp mediaTypeMap of
        Nothing -> error $ "Unknown media type " ++ show mediaTyp
        Just typName -> (T.pack . show $ typName,_mediaTypeObjectSchema mediaTypObj)
mediaTypeObjToSchema x = error $ "Invalid Request Body" ++ show x

mediaTypeMap :: HashMap MediaType ContentTypesOApi
mediaTypeMap = HM.fromList
                    [ ("text" // "plain" /: ("charset", "utf-8"),PlainText)
                    , ("application" // "json",JSON)
                    , ("text" // "html" /: ("charset", "utf-8"),HTML)
                    , ("application" // "octet-stream",OctetStream)
                    , ("multipart" // "form-data", MultipartFormData)
                    , ("application" // "x-www-form-urlencoded", UrlEncoded)
                    , (textToMediaType "application/vnd.oracle.resource+json;type=singular",JSON)
                    , (textToMediaType "application/vnd.oracle.resource+json;type=error",JSON)
                    , (textToMediaType "application/vnd.oracle.resource+json;type=collection",JSON)
                    ]
data ContentTypesOApi = PlainText | JSON | HTML | OctetStream | MultipartFormData | UrlEncoded deriving (Show, Eq)


textToMediaType :: String -> MediaType
textToMediaType t = fromMaybe "MEDIA TYPE PARSE ERROR" (parseAccept $ fromString t)

createTypSynonym ::
    (MonadState ModelGenState m) =>
    (Text ,[Either Text (Text, Maybe (Bool, Param))]) ->
    m ((Text,[Text]),[HsDecl'])
createTypSynonym (oName,params) = do
    varName <- mkUnseenVar (mkTypeSynName params (T.append oName "R"))
    (typInfo,childTypes) <-unzip <$> mapM parseTypeSynInfo params
    return ((varName, [oName]), type' (textToOccNameStr varName)
                                       []
                                       (case typInfo of
                                           [x] -> var "Static" @@ x
                                           _ -> foldr1 (`op` ":/") typInfo): Prelude.concat childTypes)

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

refValToVal :: Definitions a -> Referenced a -> a
refValToVal compVals (Ref (Reference a)) =
        case HMO.lookup a compVals of
                Nothing -> error "Reference Value Not Found"
                Just x -> x
refValToVal _compVals (Inline x) = x

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
          dpends = ["base","text", "ghc-prim","vector","webapi-contract"]
          xposedMods = modName:exposMods
          iterateOption option = concatMap (\x -> option ++ "=" ++ x)

ppExtension :: Extension -> String
ppExtension e = "{-# LANGUAGE " <> show e <> " #-}\n"

createModelData ::
    (MonadState ModelGenState m) =>
    Maybe OpenApiType ->  (Text,Schema) -> m [HsDecl']
createModelData (Just OpenApiObject) (dName,dSchema) = do
    let reqParams =  _schemaRequired dSchema
    unseenVar <- mkUnseenVar (upperFirstChar dName)
    (rFields,childTypes) <- unzip <$> mapM (\(x,y) -> parseRecordFields (x,y) (x `elem` reqParams))  (HMO.toList . _schemaProperties $ dSchema)
    ModelGenState { keywordsToAvoid } <- get
    let frFields = bimap (textToOccNameStr . lowerFirstChar) field .
                        (\(x,y)-> (if member x keywordsToAvoid
                                   then T.append x "_"
                                   else x,y)) <$> rFields
    return $ Prelude.concat childTypes ++ [data' (textToOccNameStr unseenVar) [] [recordCon (textToOccNameStr . upperFirstChar $ dName) frFields] []]
createModelData Nothing (dName,dSchema) =
    case _schemaOneOf dSchema of
        Nothing -> error "Unexpected Schema type"
        Just [] -> error "Bad OneOf Specification"
        Just [_x] -> error "Bad OneOf Specification"
        Just x -> do
            (_a,b) <- mkSumType dName True x
            return b
createModelData _ _ = error "Unknown Top Level Schema"

parseRecordFields ::
    (MonadState ModelGenState m) =>
    (Text, Referenced Schema) -> Bool -> m ((Text,HsType'),[HsDecl'])
parseRecordFields (dName,Ref (Reference x)) isReq =
    return ((dName, createHsType isReq (upperFirstChar x)),[])
parseRecordFields (dName,Inline dSchema) isReq = parseInlineFields (_schemaType dSchema) dName dSchema isReq

createHsType :: Bool -> Text -> HsType'
createHsType isReq x =
    let rdrx = textToRdrNameStr x
    in if isReq
       then var rdrx
       else var "Maybe" @@ var rdrx

parseInlineFields ::
    (MonadState ModelGenState m) =>
    Maybe OpenApiType -> Text -> Schema -> Bool -> m ((Text,HsType'),[HsDecl'])
parseInlineFields (Just OpenApiString) dName _dSchema isReq =
    return ((dName, createHsType isReq "Text"),[])
parseInlineFields (Just OpenApiNumber ) dName _dSchema isReq =
    return ((dName, createHsType isReq "Double"),[])
parseInlineFields (Just OpenApiInteger) dName dSchema isReq =
    return ((dName, createHsType isReq parsedInt),[])
    where parsedInt = parseIntegerFld (_schemaFormat dSchema)
parseInlineFields (Just OpenApiBoolean) dName _dSchema isReq =
    return ((dName, createHsType isReq "Bool"),[])
parseInlineFields (Just OpenApiArray ) dName dSchema _isReq =
    case _schemaItems dSchema of
        Nothing -> error "No _schemaItems value for Array"
        Just (OpenApiItemsObject sch) -> do
            ((dName2,dType),childTypes) <- parseRecordFields (dName,sch) True
            return ((dName2,var "Vector" @@ dType),childTypes)
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
        Nothing -> if Prelude.null (_schemaProperties dSchema)
                   then error "Unexpected Schema type"
                   else parseInlineFields (Just OpenApiObject) dName dSchema isReq
        Just x -> mkSumType dName isReq x

mkSumType :: 
    MonadState ModelGenState m =>
    Text -> Bool -> [Referenced Schema] -> m ((Text, HsType'), [HsDecl'])
mkSumType dName isReq [a] = parseRecordFields (dName,a) isReq
mkSumType dName isReq x = do
            ModelGenState { createdSums } <- get
            (isReg,vName) <- registerSumType (upperFirstChar dName) x createdSums
            if isReg
            then return ((dName,createHsType isReq vName), [])
            else do
                let newVarList = mkNewVariables vName (length x)
                unSeenVars <- mapM mkUnseenVar newVarList
                (typList,childTypes) <- unzip <$> mapM (\(a,b) -> parseRecordFields (a,b) True)  (zip unSeenVars x)
                let oneOfTyp = data' (textToOccNameStr vName) [] ((\(a,b) -> prefixCon (textToOccNameStr a) [field b]) <$> typList) []
                return ((dName,createHsType isReq vName), oneOfTyp : Prelude.concat childTypes)


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
            if areSchemasSame x schemaList
             then return (True,tName)
             else do
                 unseenVar <- mkUnseenVar tName
                 let maxNo = read  (T.unpack . snd $ T.breakOnEnd tName unseenVar) :: Int
                     doesTypeExist = checkTypExistence [ findDups a | a <- [0..maxNo]]
                 case doesTypeExist of
                     Nothing -> do
                        modify (updateSumTypes (unseenVar,schemaList))
                        return (False,unseenVar)
                     Just tName' -> return (True,tName')
    where findDups a = let x = T.append tName (T.pack . show $ a)
                       in (x,HM.lookup x hm)
          areSchemasSame x y = contained x y && contained y x
          checkTypExistence x = findTyp $ filter (areSchemasSame schemaList . snd) (fetchJusts x)
          findTyp x = case x of
                        [] -> Nothing
                        [(a,_b)] -> Just a
                        _ -> error "How did this happen"

mkNewVariables :: Text -> Int -> [Text]
mkNewVariables dName x =  [T.concat [dName,"C", T.pack . show $ a] | a <- [1..x]]

parseIntegerFld :: Maybe Text -> Text
parseIntegerFld (Just x) = let y = upperFirstChar x
                            in if T.take 3 y == "Int"
                               then y
                               else error "Invalid Integer Format"
parseIntegerFld Nothing = "Int"

removeUnsupportedSymbols :: Text -> Text
removeUnsupportedSymbols = removeSymbol '-' . removeSymbol '!' . removeSymbol ':' . removeSymbol ' '

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
    let selems = T.split (== c) x
        fir = head selems
    in fir : (upperFirstChar <$> tail selems)


retOApi :: Maybe p -> p
retOApi (Just x) = x
retOApi Nothing = error "Can't decode OpenAPI spec file"

readOpenAPI :: FilePath -> IO OpenApi
readOpenAPI fp = do
    fileContent <- B.readFile fp
    return $ retOApi (decode fileContent :: Maybe OpenApi)
