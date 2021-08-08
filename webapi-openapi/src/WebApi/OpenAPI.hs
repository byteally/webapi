{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# LANGUAGE BangPatterns #-}
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
      Responses(_responsesResponses,_responsesDefault))
import GHC.SourceGen
    ( data',
      field,
      prefixCon,
      recordCon,
      exposing,
      import',
      module',
      occNameToStr,
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
      tyFamInst,
      valBind,
      lambda,
      conP_,
      string,
      HsExpr',
      conP,
      funBind,
      match,
      list,
      funBinds,
      as',
      tuplePromotedTy
    )
import GHC.Paths (libdir)
import GHC ( runGhc )
import Data.HashMap.Strict.InsOrd as HMO (toList,lookup, empty, fromList, delete)
import Data.Text as T ( unpack, Text, append, splitAt, toUpper, take, pack, dropEnd, concat, split, toLower, breakOnEnd, isPrefixOf)
import Data.Text.IO as T (writeFile)
import GhcPlugins(getDynFlags,mkVarOcc)
import Control.Monad.State.Class ( MonadState(get), modify )
import Control.Monad.State.Lazy(evalState)
import Data.Set as S(Set, empty, fromList, member, insert,difference,null,toList)
import Data.String ( IsString(fromString) )
import Data.Bifunctor ( Bifunctor(bimap), second)
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
import Control.Monad(when)
import Debug.Trace(trace, traceM)

data ModelGenState =
    ModelGenState { seenVars :: Set Text
                  , imports :: Set Text
                  , keywordsToAvoid :: Set Text
                  , createdSums :: HashMap Text [Referenced Schema]
                  , jsonInstances :: Set Text
                  }

data PkgConfig =
    PkgConfig { authorName :: Text
              , email :: Text
              }

newtype ChildType = ChildType HsDecl'
newtype Instance = Instance HsDecl'

data DataTypeInfo =
    DataTypeInfo {
        pName :: Text,
        typ :: HsType',
        child_types :: [ChildType],
        child_instances :: [Instance]
    }

mkPkgConfig :: PkgConfig
mkPkgConfig = PkgConfig "\"Pankaj Singh Sijwali\"" "pankajsijwali1@gmail.com"

generateModels ::
    FilePath -> FilePath  -> FilePath -> IO ()
generateModels fp destFp reqPrefix = do
    oApi <- readOpenAPI fp
    let compSchemas =  _componentsSchemas . _openApiComponents $ oApi
        compParams = _componentsParameters . _openApiComponents $ oApi
        compReqBodies = _componentsRequestBodies . _openApiComponents $ oApi
        compResponses = _componentsResponses . _openApiComponents $ oApi
        compHeaders = _componentsHeaders . _openApiComponents $ oApi
        oApiName = _infoTitle . _openApiInfo $ oApi
        modelList = evalState
                        (mapM (\(x,y) -> createModelData (_schemaType y) compSchemas (x,y)) (HMO.toList compSchemas))
                        (ModelGenState S.empty S.empty (S.fromList keywords) HM.empty (S.fromList seenVariables))
        hsModuleModel = module' (Just modName) Nothing impsModel (rmChildTypeLayer (Prelude.concat modelList))
        (routeInfo,typeSynList,instances) = (\(a,b,c) -> (Prelude.concat a,Prelude.concat b,Prelude.concat c))
                                   (unzip3 $
                                       evalState
                                        (mapM (createTypeSynData compSchemas compParams compReqBodies compResponses compHeaders) (filter (T.isPrefixOf (T.pack reqPrefix) . T.pack . fst) (HMO.toList . _openApiPaths $ oApi)))
                                        (ModelGenState (S.fromList seenVariables) S.empty (S.fromList keywords) HM.empty (S.fromList seenVariables)))

    let simplifiedRouteInfo = (fmap . fmap) (map fst) routeInfo
        apiContractInstances =  Prelude.concat $ mkApiContractInstances oApiName <$> routeInfo
        hsModuleTypeSyn = module' (Just typeSynName) Nothing impsTypeSyn (untypedDef:rmChildTypeLayer typeSynList ++ webApiInstance oApiName simplifiedRouteInfo ++ apiContractInstances ++ rmInstanceLayer instances)
    writeModule (pkgHome </> "src") (modName <.> "hs") es hsModuleModel
    writeModule (pkgHome </> "src") (typeSynName <.> "hs") es2 hsModuleTypeSyn
    writeCabal pkgName mkPkgConfig modName [typeSynName] pkgHome

    where impsModel =
                 [ import' "Data.Int"
                 , import' "Data.Vector"
                 , import' "Record"
                 , exposing (import' "GHC.Types") [var "Symbol"]
                 , exposing (import' "Data.Text") [var "Text"]
                 ]
          impsTypeSyn =
                 [ import' "WebApi.Contract"
                 , import' modName
                 , import' "Data.Int"
                 , exposing (import' "GHC.Types") [var "Symbol"]
                 , exposing (import' "Data.Text") [var "Text"]
                 , import' "Data.Vector" `as'` "V"
                 , import' "Data.Aeson"
                 , import' "Control.Applicative"
                 ]

          es = [TypeOperators,KindSignatures,DataKinds,DuplicateRecordFields]
          es2 = [DataKinds,TypeOperators,TypeSynonymInstances,FlexibleInstances,MultiParamTypeClasses,TypeFamilies, OverloadedStrings]
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
          rmChildTypeLayer = fmap (\(ChildType x) -> x)
          rmInstanceLayer = fmap (\(Instance x) -> x)

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
    Definitions Schema ->
    Definitions Param ->
    Definitions RequestBody ->
    Definitions Response ->
    Definitions Header ->
    (FilePath,PathItem) ->
    m ([(Text, [(Text, (Maybe HsType', Maybe HsType', Maybe HsType', Maybe HsType',Maybe HsType',Maybe HsType', Maybe HsType'))])],[ChildType],[Instance])
createTypeSynData compSchemas compsParam compReqBodies compResponses compHeaders (fp,PathItem _ _ piGet piPut piPost piDelete piOptions piHead piPatch piTrace _ piParams) = do
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
    (typSyns,ct1) <- unzip <$> mapM (createTypSynonym compSchemas) overridesParam
    (commonTypSyn,ct2) <- unzip <$> let diff = S.difference (pairLisToSet opList) (pairLisToSet overridesParam)
                              in if S.null diff
                              then return []
                              else do
                                 ((a,_b),c) <- createTypSynonym compSchemas ("",commonParams)
                                 return [((a,S.toList diff),c)]
    (apiConInsData,ct3,ci3) <-  unzip3 <$> mapM (createApiContractInsData compSchemas compsParam compReqBodies compResponses compHeaders paramsMap) opList
    return ((fmap . fmap) (applyApiContractInfo (unions apiConInsData)) <$> commonTypSyn ++ typSyns
           , Prelude.concat ct1 ++ Prelude.concat ct2 ++ Prelude.concat ct3
           , Prelude.concat ci3)
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
    Definitions Schema ->
    Definitions Param ->
    Definitions RequestBody ->
    Definitions Response ->
    Definitions Header ->
    HashMap Text (Bool, Param) ->
    (Text,Operation) ->
    m (HashMap Text (Maybe HsType',Maybe HsType',Maybe HsType', Maybe HsType',Maybe HsType',Maybe HsType', Maybe HsType'),[ChildType],[Instance])
createApiContractInsData compSchemas compsParam compsReqBodies compResponses compHeaders commonParamMap (opName,operationData) = do
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
    (reqBody,ct4,ci4) <- createReqBody compSchemas compsReqBodies opReqBody
    (apiOutType,ct5,ci5) <- createApiOut
                            compSchemas
                            compResponses
                            (findResponseApiOut opResponses)
                            defaultResponse
    (apiErrType,ct6,ci6) <- createApiErr compSchemas compResponses defaultResponse (HMO.toList(findResponseApiErr opResponses))
    (headerOutType,ct7) <- createHeaderOut compSchemas headerOutSchemas
    return ( HM.singleton opName (headtypTuple,querytypTuple,cookietypTuple,reqBody,apiOutType,apiErrType,headerOutType)
           , ct1 ++ ct2 ++ ct3 ++ ct4 ++ ct5 ++ ct6 ++ ct7
           , ci4 ++ ci5 ++ ci6
           )
    where mFilter x = filter (\(_a,(_b,c)) -> _paramIn c == x)
          createType a b = mkTypeTuple $ fmap (\ (_, x) -> _paramSchema x) <$> mFilter a b
          mkTypeTuple [] = return (Nothing,[])
          mkTypeTuple schemas = do
              dataTypeInfoList <- mapM (\x -> mayBeSchemaToHsType x False Nothing compSchemas) schemas
              let (schemaList,childTypes) = unzip $ (\(DataTypeInfo a b c _) -> ((a,b),c) ) <$> dataTypeInfoList
              let typeTuple = var "Rec" @@ (listPromotedTy $ (\(x,y)-> tuplePromotedTy [stringTy (T.unpack x), y]) <$> schemaList)
              return (Just typeTuple,Prelude.concat childTypes)
          responseToHeader (_,res) = fmap (_headerSchema . refValToVal compHeaders) <$> (HMO.toList . _responseHeaders $ res)
          findResponseApiOut hMap = case catMaybes [HMO.lookup x hMap | x <- [200..299]] of
                                        [] -> Nothing
                                        [x] -> Just x
                                        _ -> error "More than One ApiOut Type"
          findResponseApiErr hmap = foldr HMO.delete hmap [200..299]

createHeaderOut ::
    MonadState ModelGenState m =>
    Definitions Schema -> [Referenced Schema] -> m (Maybe HsType', [ChildType])
createHeaderOut _compSchemas [] = return (Nothing,[])
createHeaderOut compSchemas x = do
    DataTypeInfo {typ, child_types} <- mkSumType "HeaderOutSumType" True x True False Nothing compSchemas
    return (Just typ,child_types)

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
    Definitions Schema ->
    Definitions Response ->
    Maybe (Referenced Response) ->
    Maybe (Referenced Response) ->
    m (Maybe HsType', [ChildType],[Instance])
createApiOut _ _ Nothing Nothing = return (Just $ var "()",[],[])
createApiOut compSchemas hMap Nothing x = createApiOut compSchemas hMap x Nothing
createApiOut compSchemas hMap (Just res) defRes = do
    let inlineResp = refValToVal hMap res
        mediaTypList = HMO.toList . _responseContent $ inlineResp
    if Prelude.null mediaTypList
    then case defRes of
            Nothing -> createApiOut compSchemas hMap Nothing Nothing
            x -> createApiOut compSchemas hMap x Nothing
    else do
        let (cType,maySchema) =  mediaTypeObjToSchema mediaTypList
        DataTypeInfo {typ,child_types,child_instances} <- mayBeSchemaToHsType ("ApiOutType",maySchema) True (Just cType) compSchemas
        return (Just typ,child_types, child_instances)

createApiErr ::
    MonadState ModelGenState m =>
    Definitions Schema ->
    Definitions Response ->
    Maybe (Referenced Response) ->
    [(Int, Referenced Response)] -> m (Maybe HsType', [ChildType],[Instance])
createApiErr _ _ Nothing [] = return (Just $ var "()",[],[])
createApiErr compSchemas hMap (Just x) [] = createApiErr compSchemas hMap Nothing [(0,x)]
createApiErr compSchemas hMap defRes resList = do
    let resList' = case defRes of
                      Nothing -> resList
                      (Just x) -> (0,x):resList
        inlineResp = refValToVal hMap . snd <$> resList'
        mediaTypList =  HMO.toList . _responseContent  <$> inlineResp
    if Prelude.null (Prelude.concat mediaTypList)
    then createApiErr compSchemas hMap Nothing []
    else do
        let (cType,neMediaTypList) = unzip $ second maySchemaToSchema . mediaTypeObjToSchema <$> filter (not . Prelude.null) mediaTypList
            ctype' = case nub cType of
                        [a] -> a
                        _ -> error "Conflicting ApiErr Type"
        DataTypeInfo {typ,child_types,child_instances} <- mkSumType "ApiErrSumType" True neMediaTypList True True (Just ctype') compSchemas

        return (Just typ,child_types,child_instances)

mkEmptySchema :: Schema
mkEmptySchema = Schema Nothing Nothing [] Nothing Nothing Nothing Nothing Nothing HMO.empty  Nothing
                       Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing
                       Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing
                       Nothing Nothing Nothing Nothing Nothing

mayBeSchemaToHsType ::
    MonadState ModelGenState m =>
    (Text, Maybe (Referenced Schema)) ->
    Bool ->
    Maybe ContentTypesOApi ->
    Definitions Schema ->
    m DataTypeInfo
mayBeSchemaToHsType (x,y) = parseRecordFields (x,maySchemaToSchema y) True

maySchemaToSchema ::
    Maybe (Referenced Schema) ->
    Referenced Schema
maySchemaToSchema Nothing = Ref (Reference "Untyped")
maySchemaToSchema (Just x) = x

createReqBody ::
    (MonadState ModelGenState m) =>
    Definitions Schema ->
    Definitions RequestBody ->
    Maybe (Referenced RequestBody) ->
    m (Maybe HsType',[ChildType],[Instance])
createReqBody _ _ Nothing = return (Nothing,[],[])
createReqBody compSchemas compReqBodies (Just refReqBody) = do
    let reqBody = refValToVal compReqBodies refReqBody
    let (typName,maySchema) = mediaTypeObjToSchema . HMO.toList . _requestBodyContent $ reqBody
    DataTypeInfo {typ,child_types,child_instances} <- mayBeSchemaToHsType ("requestBody",maySchema) True (Just typName) compSchemas
    let finalType = if typName == JSON
                    then listPromotedTy [typ]
                    else listPromotedTy [var "Content" @@ listPromotedTy [var $ textToRdrNameStr (T.pack . show $ typName)] @@ typ]
    return (Just finalType,child_types, child_instances)

mediaTypeObjToSchema :: [(MediaType, MediaTypeObject)] -> (ContentTypesOApi, Maybe (Referenced Schema))
mediaTypeObjToSchema [(mediaTyp,mediaTypObj)] =
    case HM.lookup mediaTyp mediaTypeMap of
        Nothing -> error $ "Unknown media type " ++ show mediaTyp
        Just typName -> (typName,_mediaTypeObjectSchema mediaTypObj)
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
    Definitions Schema ->
    (Text ,[Either Text (Text, Maybe (Bool, Param))]) ->
    m ((Text,[Text]),[ChildType])
createTypSynonym compSchemas(oName,params) = do
    varName <- mkUnseenVar (mkTypeSynName params (T.append oName "R"))
    (typInfo,childTypes) <-unzip <$> mapM (parseTypeSynInfo compSchemas) params
    return ((varName, [oName]), ChildType (
                                  type' (textToOccNameStr varName)
                                        []
                                        (case typInfo of
                                           [x] -> var "Static" @@ x
                                           _ -> foldr1 (`op` ":/") typInfo)): Prelude.concat childTypes)

parseTypeSynInfo ::
    (MonadState ModelGenState m) =>
    Definitions Schema ->
    Either Text (Text, Maybe (Bool, Param)) ->
    m (HsType',[ChildType])
parseTypeSynInfo _ (Left x) = return (stringTy $ T.unpack x,[])
parseTypeSynInfo _ (Right (_,Nothing)) = error "Type Not Found"
parseTypeSynInfo compSchemas (Right (x,Just (_,y))) =
    case _paramSchema y of
        Nothing -> error "No Parameter Schema"
        Just s  -> do
            DataTypeInfo {typ, child_types} <- parseRecordFields (x,s) True False Nothing compSchemas
            return (typ,child_types)


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
updateSeenVars v (ModelGenState x y z w a) =
  ModelGenState ( S.insert v x) y z w a

updateJsonInstances :: Text -> ModelGenState -> ModelGenState
updateJsonInstances v (ModelGenState x y z w a) =
  ModelGenState x y z w (S.insert v a)

updateSumTypes :: (Text,[Referenced Schema]) -> ModelGenState -> ModelGenState
updateSumTypes (a,b) (ModelGenState w x y z v) =
    ModelGenState w x y (HM.insert a b z) v

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
          dpends = ["base","text", "ghc-prim","vector","aeson","webapi-contract"]
          xposedMods = modName:exposMods
          iterateOption option = concatMap (\x -> option ++ "=" ++ x)

ppExtension :: Extension -> String
ppExtension e = "{-# LANGUAGE " <> show e <> " #-}\n"

createModelData ::
    (MonadState ModelGenState m) =>
    Maybe OpenApiType -> Definitions Schema -> (Text,Schema) -> m [ChildType]
createModelData (Just OpenApiObject) compSchemas (dName,dSchema) = do
    let reqParams =  _schemaRequired dSchema
    unseenVar <- mkUnseenVar (upperFirstChar dName)
    dataTypeInfoList <- mapM (\(x,y) -> parseRecordFields (x,y) (x `elem` reqParams) False Nothing compSchemas)  (HMO.toList . _schemaProperties $ dSchema)
    let (rFields,childTypes) = unzip $ (\(DataTypeInfo a b c _) -> ((a,b),c)) <$> dataTypeInfoList
    ModelGenState { keywordsToAvoid } <- get
    let frFields = (\(x,y)-> (textToOccNameStr $ avoidKeywords x keywordsToAvoid,y)) .
                      bimap (removeUnsupportedSymbols . lowerFirstChar) field  <$> rFields
    return $ Prelude.concat childTypes ++
             [ChildType $ data' (textToOccNameStr unseenVar) [] [recordCon (textToOccNameStr unseenVar) frFields] []]
createModelData Nothing compSchemas (dName,dSchema) =
    case _schemaOneOf dSchema of
        Nothing -> error "Unexpected Schema type"
        Just [] -> error "Bad OneOf Specification"
        Just [_x] -> error "Bad OneOf Specification"
        Just x -> do
            DataTypeInfo {child_types} <- mkSumType dName True x True False Nothing compSchemas
            return child_types
createModelData _ _ _ = error "Unknown Top Level Schema"

avoidKeywords :: Text -> Set Text -> Text
avoidKeywords x keywordsToAvoid = if member x keywordsToAvoid
                                  then T.append x "_"
                                  else x

parseRecordFields ::
    (MonadState ModelGenState m) =>
    (Text, Referenced Schema) ->
    Bool ->
    Bool ->
    Maybe ContentTypesOApi ->
    Definitions Schema ->
    m DataTypeInfo
parseRecordFields (dName,Ref (Reference x)) isReq generateInstance instanceType compSchemas = do
    let sName = removeUnsupportedSymbols . upperFirstChar $ x
    if generateInstance
    then do
        let schemaVal = refValToVal compSchemas (Ref (Reference x))
        instanceList <- createInstanceData instanceType sName schemaVal compSchemas
        return $ DataTypeInfo dName (createHsType isReq sName) [] instanceList
    else return $ DataTypeInfo  dName (createHsType isReq sName) [] []
parseRecordFields (dName,Inline dSchema) isReq generateInstance instanceType compSchemas =
    parseInlineFields (_schemaType dSchema) dName dSchema isReq generateInstance instanceType compSchemas

createInstanceData ::
    MonadState ModelGenState m =>
    Maybe ContentTypesOApi ->
    Text ->
    Schema ->
    Definitions Schema ->
    m [Instance]
createInstanceData (Just JSON) schemaName schemaVal compSchemas = do
    ModelGenState {jsonInstances} <- get
    --traceM $ show (schemaName,member schemaName jsonInstances)
    if member schemaName jsonInstances
    then return []
    else do
        modify (updateJsonInstances schemaName)
        createJsonInstances (_schemaType schemaVal) schemaName schemaVal compSchemas
createInstanceData _ _ _ _= error "Unhandled MediaType"

createJsonInstances ::
    MonadState ModelGenState m =>
    Maybe OpenApiType ->
    Text ->
    Schema ->
    Definitions Schema ->
    m [Instance]
createJsonInstances (Just OpenApiObject) schemaName schemaVal compSchemas = do
    let reqParams =  _schemaRequired schemaVal
        schemaProperties = HMO.toList . _schemaProperties $ schemaVal
    childInstances <- fmap child_instances <$> mapM (\(x,y) -> parseRecordFields (x,y) (x `elem` reqParams) True (Just JSON) compSchemas) schemaProperties
    fromJsonInstance <- createFromJsonInstancesRecord schemaName schemaVal compSchemas
    toJsonInstance <- createToJsonInstancesRecord schemaName schemaVal
    return $ Prelude.concat childInstances ++ [fromJsonInstance, toJsonInstance]
createJsonInstances _ _ _ _ = error "Unhandled Top Level Referenced Schema Type"

createFromJsonInstancesRecord ::
    MonadState ModelGenState m =>
    Text ->
    Schema ->
    Definitions Schema ->
    m Instance
createFromJsonInstancesRecord dName schemaVal compSchemas = do
    let reqParams =  _schemaRequired schemaVal
        schemaProperties = HMO.toList . _schemaProperties $ schemaVal
    fromjsonExpr <- createFromJsonFieldExpr compSchemas dName reqParams schemaProperties
    let fromjsonInst = instance' (var "FromJSON" @@ var (textToRdrNameStr dName))
                                     [valBind "parseJSON"
                                              (op (var "withObject" @@ string (T.unpack dName))
                                                  "$"
                                                  (lambda [conP_ "v"] fromjsonExpr)
                                              )]
    return $ Instance fromjsonInst

createFromJsonFieldExpr ::
    (MonadState ModelGenState m) =>
    Definitions Schema ->
    Text ->
    [Text] ->
    [(Text,Referenced Schema)] ->
    m HsExpr'
createFromJsonFieldExpr compSchemas dName reqParams schemaProps = do
    ModelGenState {keywordsToAvoid} <- get
    let instInfo =  (\(x,y) -> if _schemaType (refValToVal compSchemas y) == Just OpenApiArray && notElem x reqParams
                               then op (op (var "v")
                                           (findSeparatorSymbol False)
                                           (string . T.unpack . (`avoidKeywords` keywordsToAvoid) . lowerFirstChar . removeUnsupportedSymbols $ x))
                                       ".!="
                                       (var "V.empty")
                               else op (var "v")
                                       (findSeparatorSymbol (x `elem` reqParams))
                                       (string . T.unpack . (`avoidKeywords` keywordsToAvoid) . lowerFirstChar . removeUnsupportedSymbols $ x)
                    ) <$> schemaProps
        instInfo' = op (var $ textToRdrNameStr dName) "<$>" (head instInfo):tail instInfo
    return $ foldl1 (`op` "<*>") instInfo'
    where findSeparatorSymbol True = ".:"
          findSeparatorSymbol False = ".:?"


createToJsonInstancesRecord ::
    (MonadState ModelGenState m) =>
    Text ->
    Schema ->
    m Instance
createToJsonInstancesRecord schemaName schemaVal = do
    ModelGenState {keywordsToAvoid} <- get
    let schemaProperties = HMO.toList . _schemaProperties $ schemaVal
        sFields =  (`avoidKeywords` keywordsToAvoid) . lowerFirstChar . removeUnsupportedSymbols . fst <$> schemaProperties
        rFieldList = bvar . textToOccNameStr <$> sFields
        associationList = list $ (\x -> op (string . T.unpack $ x) ".=" (var . textToRdrNameStr $x)) <$> sFields
    let toJsonInst = instance' (var "ToJSON" @@ var (textToRdrNameStr schemaName))
                               [funBind "toJSON" (match [conP (textToRdrNameStr schemaName) rFieldList] (var "object" @@ associationList) ) ]
    return $ Instance toJsonInst

createHsType :: Bool -> Text -> HsType'
createHsType isReq x =
    let rdrx = textToRdrNameStr x
    in if isReq
       then var rdrx
       else var "Maybe" @@ var rdrx

parseInlineFields ::
    (MonadState ModelGenState m) =>
    Maybe OpenApiType ->
    Text ->
    Schema ->
    Bool ->
    Bool ->
    Maybe ContentTypesOApi ->
    Definitions Schema ->
    m DataTypeInfo
parseInlineFields (Just OpenApiString) dName _dSchema isReq _ _ _=
    return $ DataTypeInfo dName (createHsType isReq "Text") [] []
parseInlineFields (Just OpenApiNumber ) dName _dSchema isReq _ _ _=
    return $ DataTypeInfo dName (createHsType isReq "Double") [] []
parseInlineFields (Just OpenApiInteger) dName dSchema isReq _ _ _=
    return $ DataTypeInfo dName (createHsType isReq parsedInt) [] []
    where parsedInt = parseIntegerFld (_schemaFormat dSchema)
parseInlineFields (Just OpenApiBoolean) dName _dSchema isReq _ _ _=
    return $ DataTypeInfo dName (createHsType isReq "Bool") [] []
parseInlineFields (Just OpenApiArray ) dName dSchema _isReq generateInstance instanceType compSchemas=
    case _schemaItems dSchema of
        Nothing -> error "No _schemaItems value for Array"
        Just (OpenApiItemsObject sch) -> do
            DataTypeInfo {..} <- parseRecordFields (dName,sch) True generateInstance instanceType compSchemas
            return $ DataTypeInfo pName (var "Vector" @@ typ) child_types child_instances
        Just (OpenApiItemsArray _) -> error "OpenApiItemsArray Array type"
parseInlineFields (Just OpenApiNull ) _dName _dSchema _isReq _ _ _=
    error "Null OpenApi Type"
parseInlineFields (Just OpenApiObject ) dName dSchema isReq generateInstance instanceType compSchemas = do
    dataTypeInfoList <- mapM (\(x,y) -> parseRecordFields (x,y) (x `elem` reqParams) generateInstance instanceType compSchemas) (HMO.toList . _schemaProperties $ dSchema)
    let (childInlines,childTypes,childInstances) = unzip3 $ (\(DataTypeInfo a b c d) -> ((a,b),c,d)) <$> dataTypeInfoList
    let typeTuple = var "Rec" @@ (listPromotedTy $ (\(x,y)-> tuplePromotedTy [stringTy (T.unpack x), y]) <$> childInlines)
    return $ DataTypeInfo dName
                          (if isReq then typeTuple else var "Maybe" @@ typeTuple)
                          (Prelude.concat childTypes)
                          (Prelude.concat childInstances)
    where reqParams =  _schemaRequired dSchema

parseInlineFields Nothing dName dSchema isReq generateInstance instanceType compSchemas=
    case _schemaOneOf dSchema of
        Nothing -> if Prelude.null (_schemaProperties dSchema)
                   then error "Unexpected Schema type"
                   else parseInlineFields (Just OpenApiObject) dName dSchema isReq generateInstance instanceType compSchemas
        Just x -> mkSumType dName isReq x False generateInstance instanceType compSchemas

mkSumType ::
    MonadState ModelGenState m =>
    Text ->
    Bool ->
    [Referenced Schema] ->
    Bool ->
    Bool ->
    Maybe ContentTypesOApi ->
    Definitions Schema ->
    m DataTypeInfo
mkSumType dName isReq [a] _isTopLevel generateInstance instanceType compSchemas =
    parseRecordFields (dName,a) isReq generateInstance instanceType compSchemas
mkSumType dName isReq x isTopLevel generateInstance instanceType compSchemas = do
            let dName' = if isTopLevel then dName else T.append dName "SumType"
            ModelGenState { createdSums } <- get
            (isReg,vName) <- registerSumType (upperFirstChar dName') x createdSums
            let newVarList = mkNewVariables vName (length x)
            unSeenVars <- if isReg then return newVarList else mapM mkUnseenVar newVarList
            dataTypeInfoList <- mapM (\(a,b) -> parseRecordFields (a,b) True generateInstance instanceType compSchemas)  (zip unSeenVars x)
            let (typList,childTypes,childInstances) = unzip3 $ (\(DataTypeInfo a b c d) -> ((a,b),c,d)) <$> dataTypeInfoList
            encodeDecodeInstances <- if generateInstance
                                     then (Prelude.concat childInstances ++)  <$> createInstancesSumType instanceType vName (fst <$> typList)
                                     else return []
            if isReg
            then return $ DataTypeInfo dName (createHsType isReq vName) [] encodeDecodeInstances
            else do
                let oneOfTyp = ChildType $ data' (textToOccNameStr vName) [] ((\(a,b) -> prefixCon (textToOccNameStr a) [field b]) <$> typList) []
                return $ DataTypeInfo dName
                                      (createHsType isReq vName)
                                      (oneOfTyp : Prelude.concat childTypes)
                                       encodeDecodeInstances

f :: Monad m => Text -> [HsDecl'] -> m ()
f a xs = do
    traceM $ "Trace Message : " ++ show a
    mapM_ (\(!_x) -> pure ()) xs

createInstancesSumType ::
    MonadState ModelGenState m =>
    Maybe ContentTypesOApi ->
    Text ->
    [Text] ->
    m [Instance]
createInstancesSumType (Just JSON) tName consList = do
    ModelGenState {jsonInstances} <- get
    --traceM $ show (tName,member tName jsonInstances)
    if member tName jsonInstances
    then return []
    else do
        modify (updateJsonInstances tName)
        when (tName == "ItemSumType") $ trace "ItemType" (return ())
        let fromJsonInst = createFromJsonInstanceSumType tName consList
            toJsonInst = createToJsonInstanceSumType tName consList
        return [fromJsonInst,toJsonInst]
createInstancesSumType _ _ _= error "Unhandled MediaType Sum"

createFromJsonInstanceSumType ::
    Text ->
    [Text] ->
    Instance
createFromJsonInstanceSumType tName consList = Instance $
    instance' (var "FromJSON" @@ var (textToRdrNameStr tName))
              [funBind "parseJSON" $ match [bvar "v"] fieldInfo]
    where fieldInfo = foldl1 (`op` "<|>") $ (\x -> op (var (textToRdrNameStr x)) "<$>" (var "parseJSON" @@ var "v")) <$> consList


createToJsonInstanceSumType ::
    Text ->
    [Text] ->
    Instance
createToJsonInstanceSumType tName consList = Instance $
    instance' (var "ToJSON" @@ var (textToRdrNameStr tName))
              [funBinds "toJSON" matchList]
    where matchList = (`match` (var "toJSON" @@ var "x")) . (\x -> [conP x [bvar "x"]]) . textToRdrNameStr <$> consList


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
