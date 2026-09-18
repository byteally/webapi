{-# LANGUAGE CPP                        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
module WebApi.OpenAPI where

import Data.ByteString.Lazy as B (readFile)
import Data.Aeson (eitherDecode, FromJSON (..), withObject, (.:), (.:?), Value (..))
import Data.OpenApi
    ( Components(_componentsSchemas, _componentsParameters, _componentsRequestBodies, _componentsResponses, _componentsHeaders),
      OpenApi(_openApiComponents, _openApiPaths, _openApiInfo),
      OpenApiItems(OpenApiItemsArray, OpenApiItemsObject),
      OpenApiType(..),
      PathItem(PathItem),
      Reference(Reference),
      Referenced(..),
      Schema(Schema,_schemaType, _schemaFormat, _schemaItems, _schemaRequired,
             _schemaProperties, _schemaOneOf, _schemaEnum),
      Definitions,
      Param(_paramName, _paramSchema, _paramIn, _paramRequired),
      Operation(_operationParameters, _operationRequestBody, _operationResponses, _operationSummary, _operationOperationId),
      ParamLocation(ParamHeader, ParamCookie, ParamQuery, ParamPath),
      Info(_infoTitle),
      RequestBody(_requestBodyContent),
      MediaTypeObject(_mediaTypeObjectSchema),
      Response(_responseContent,_responseHeaders),
      Header(_headerSchema),
      Responses(_responsesResponses,_responsesDefault))
import GHC.SourceGen
    ( data',
      deriving',
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
      HsDerivingClause',
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
      tuple,
      as'
    )
import Data.HashMap.Strict.InsOrd as HMO (toList,lookup, empty, fromList, delete, null)
import Data.Text as T ( unpack, Text, append, splitAt, toUpper, take, pack, dropEnd, concat, split, toLower, breakOnEnd, isPrefixOf)
import Data.Text.IO as T (writeFile)
import qualified Data.Text.Encoding as TE
import qualified Data.Text as TQ
#if (MIN_VERSION_ghc(9,0,0))
import GHC.Plugins(mkVarOcc)
import GHC.Utils.Outputable(ppr,renderWithContext, defaultSDocContext)
#else
import GhcPlugins(getDynFlags,mkVarOcc)
import Outputable(ppr,showSDoc)
import GHC.Paths (libdir)
import GHC ( runGhc )
#endif
import Control.Monad.State.Class ( MonadState(get), modify )
import Control.Monad.State.Lazy(runState)
import Data.Set as S(Set, empty, fromList, member, insert,difference,null,toList)
import qualified Data.Set as SetQ
import Data.String ( IsString(fromString) )
import Data.Bifunctor ( Bifunctor(bimap), second)
import Ormolu
    ( ormolu, defaultConfig, Config(cfgCheckIdempotence) )
import System.FilePath.Posix
    ( (<.>), (</>), dropExtension, takeFileName, splitDirectories )
import System.Directory ( createDirectoryIfMissing )
import Data.Char (isAlphaNum)
import qualified Data.Char
import Data.List as L (delete, nub)
import qualified Data.List
import qualified Crypto.Hash.SHA256 as SHA256
import qualified Data.ByteString as BSS
import Data.HashMap.Internal as HM (HashMap, singleton, unions, lookup, empty, insert, union, toList, fromList)
import qualified Data.HashMap.Internal as HMQ
import Language.Haskell.TH (Extension(..))
import Test.FitSpec.Utils(contained)
import Network.HTTP.Media ( MediaType , (//), (/:), parseAccept)
import Data.Maybe ( fromMaybe, catMaybes )
import Control.Monad(when, unless)
import Control.Applicative ((<|>))
import Control.Exception (SomeException, catch)
import Debug.Trace(traceM)

data ModelGenState =
    ModelGenState { seenVars :: Set Text
                  , imports :: Set Text
                  , keywordsToAvoid :: Set Text
                  , createdSums :: HashMap Text [Referenced Schema]
                  , jsonInstances :: Set Text
                  -- the concrete instance layer's name sets (M9)
                  , inlineRecords :: Set Text
                  , paramRecords :: Set Text
                  , bodyTypes :: Set Text
                  , resultTypes :: Set Text
                  -- enums-as-sums (M10c-4)
                  , enumsAsSums :: Bool
                  , enumSums :: HashMap [Text] Text
                    -- ^ canonical (sorted) value-set -> interned type name
                  , enumTypes :: Set Text
                  -- inline-record naming keyed by SHAPE, not field names
                  -- alone (two {refName,id} objects with different id
                  -- enums must not alias — M10c-4 bug fix)
                  , inlineShapes :: HashMap Text [([(Text, Text)], Text)]
                  -- the modular layout (a generator config): component
                  -- names resolved once to Haskell names, the module whose
                  -- declarations are being generated, and the behaviours
                  -- the legacy single-module output keeps as they were
                  , modular :: Bool
                  , compNames :: HashMap Text Text
                  , curModule :: Text
                  , inlinePrefix :: Text
                  , warnings :: [Text]
                  , connectionParams :: Set Text
                    -- ^ modular: query parameters the connection sets on
                    -- every request (a tenant's organization, a shop), left
                    -- out of each operation's own parameters
                  }

-- | The state both passes start from; the legacy layout keeps every
-- behaviour its golden output pins.
initState :: Bool -> [Text] -> ModelGenState
initState sumEnums reserved =
    ModelGenState { seenVars = S.fromList reserved
                  , imports = S.empty
                  , keywordsToAvoid = S.fromList haskellKeywords
                  , createdSums = HM.empty
                  , jsonInstances = S.empty
                  , inlineRecords = S.empty
                  , paramRecords = S.empty
                  , bodyTypes = S.empty
                  , resultTypes = S.empty
                  , enumsAsSums = sumEnums
                  , enumSums = HM.empty
                  , enumTypes = S.empty
                  , inlineShapes = HM.empty
                  , modular = False
                  , compNames = HM.empty
                  , curModule = ""
                  , inlinePrefix = "NsObj"
                  , warnings = []
                  , connectionParams = S.empty
                  }

haskellKeywords :: [Text]
haskellKeywords = [ "case","class","data","default","deriving","do","else"
                  , "foreign","if","import","in","infix","infixl","infixr"
                  , "instance","let","module","newtype","of","then","type"
                  , "where","forall" ]

warn :: MonadState ModelGenState m => Text -> m ()
warn w = modify (\st -> st { warnings = w : warnings st })

-- | The Haskell type a component reference names. Legacy: the component
-- name, capitalised and stripped. Modular: the name resolved up front for
-- every component (so a declaration and every reference to it agree), and
-- "Opaque" where a schema is missing.
refName :: ModelGenState -> Text -> Text
refName st x
  | modular st = case HM.lookup x (compNames st) of
                   Just n -> n
                   Nothing | x == "Untyped" -> "Opaque"
                           | otherwise -> pascalName x
  | otherwise = removeUnsupportedSymbols (upperFirstChar x)

-- | The name a component's own declaration takes.
componentDeclName :: MonadState ModelGenState m => Text -> m Text
componentDeclName dName = do
  st <- get
  if modular st then pure (refName st dName) else mkUnseenVar (upperFirstChar dName)

-- | kebab-case, snake_case and dotted names as one PascalCase identifier.
pascalName :: Text -> Text
pascalName t =
  case T.concat (upperFirstChar <$> filter (not . TQ.null) (TQ.split (\c -> not (isAlphaNum c)) t)) of
    n | TQ.null n -> "T"
      | Data.Char.isDigit (TQ.head n) -> T.append "T" n
      | otherwise -> n

data PkgConfig =
    PkgConfig { authorName :: Text
              , email :: Text
              }

-- | One curated entry of the naming map (M10): the FQN/OpId name for an
-- operation, optionally pinning its action uuid (a published corpus
-- references both) and overriding the doc's summary. Keys are
-- @"METHOD /path"@ — total and unique by construction, where
-- operationId is optional and vendor-owned.
data NameEntry =
    NameEntry { neName :: Text
              , neUuid :: Maybe Text
              , neSummary :: Maybe Text
              , neDefaults :: Maybe (HM.HashMap Text (HM.HashMap Text Text))
                -- ^ curated binding defaults: request part -> field -> a
                -- Haskell expression for the field's VALUE (spliced
                -- verbatim as @setField @field (Const (expr))@ on the
                -- binding's typed request). Curation, not schema policy:
                -- e.g. NetSuite's required-but-HATEOAS body.links defaults
                -- to @mempty :: Vector NsLink@ (the value, no outer parentheses).
              }

instance FromJSON NameEntry where
  parseJSON = withObject "NameEntry" $ \o ->
    NameEntry <$> o .: "name" <*> o .:? "uuid" <*> o .:? "summary" <*> o .:? "defaults"

type NamingMap = HM.HashMap Text NameEntry

-- | What an operation is called, resolved once where the Operation and
-- its path meet: curated (naming map) -> sanitized operationId ->
-- Nothing (the emission's positional fallback). Rides the routeInfo
-- tuple into every consumer.
data OpMeta =
    OpMeta { omKey :: Text          -- ^ "METHOD /path", the map coordinate
           , omName :: Maybe Text   -- ^ resolved op name; Nothing = positional
           , omUuid :: Maybe Text   -- ^ curated uuid pin only
           , omSummary :: Maybe Text
           , omDefaults :: Maybe (HM.HashMap Text (HM.HashMap Text Text))
           , omPathParam :: Maybe HsType'
             -- ^ modular: a named record for a route with several
             -- captures (webapi's default is a tuple, which dhall-do's
             -- bridge has no instances for, and which names nothing)
           , omConnParams :: [Text]
             -- ^ modular: the connection parameters the operation takes,
             -- which the connection supplies instead of the operation
           }

resolveOpMeta :: NamingMap -> FilePath -> Text -> Operation -> OpMeta
resolveOpMeta namingMap path method oper =
    OpMeta { omKey = key, omName = name, omUuid = uuid, omSummary = summ
           , omDefaults = neDefaults =<< curated, omPathParam = Nothing, omConnParams = [] }
    where key = method <> " " <> T.pack path
          curated = HM.lookup key namingMap
          name = (neName <$> curated) <|> (sanitizeOpName <$> _operationOperationId oper)
          uuid = case neUuid =<< curated of
                   Just u | not (validUuid u) ->
                     error ("naming map " <> T.unpack key <> ": bad uuid literal " <> T.unpack u)
                   x -> x
          summ = (neSummary =<< curated) <|> _operationSummary oper

-- | operationId -> a lowerCamel Haskell-safe name: the base sanitizer's
-- char set plus the separators vendors actually use in operation ids.
sanitizeOpName :: Text -> Text
sanitizeOpName =
    lowerFirstChar . removeSymbol '_' . removeSymbol '.' . removeSymbol '/'
      . removeUnsupportedSymbols

validUuid :: Text -> Bool
validUuid t =
    TQ.length t == 36
      && and [ TQ.index t i == '-' | i <- [8, 13, 18, 23] ]
      && TQ.all (\c -> c == '-' || c `TQ.elem` "0123456789abcdef") t

-- | The per-(method, route) contract slots plus the op's resolved
-- naming ('OpMeta') — the shape routeInfo carries everywhere.
type OpSlot =
    ( Maybe HsType', Maybe HsType', Maybe HsType', Maybe HsType'
    , Maybe HsType', Maybe HsType', Maybe HsType', OpMeta )

-- | A generated declaration, with the name it declares: the modular
-- layout routes each to its module by that name, and emits the registry's
-- bridge instances only for data types, never for synonyms.
data DeclKind = DataDecl | SynDecl deriving (Eq, Show)
data ChildType = ChildType { ctName :: Text, ctKind :: DeclKind, ctDecl :: HsDecl' }
-- | A generated instance, with the type it is for (it lives beside it).
data Instance = Instance { instFor :: Text, instDecl :: HsDecl' }

dataCT, synCT :: Text -> HsDecl' -> ChildType
dataCT n = ChildType n DataDecl
synCT n = ChildType n SynDecl

data DataTypeInfo =
    DataTypeInfo {
        pName :: Text,
        typ :: HsType',
        child_types :: [ChildType],
        child_instances :: [Instance]
    }

mkPkgConfig :: PkgConfig
mkPkgConfig = PkgConfig "\"Pankaj Singh Sijwali\"" "pankajsijwali1@gmail.com"

-- | Every generated data type carries the instances the Dhall bridge's
-- generic defaults hang off (M9 spike: Show/Eq for the test layer,
-- Generic for both webapi's param codecs and the bridge walks).
stdDeriving :: [HsDerivingClause']
stdDeriving = [deriving' [var "Show", var "Eq", var "Generic"]]

generateModels ::
    FilePath -> FilePath  -> FilePath -> NamingMap -> Bool -> IO ()
generateModels fp destFp reqPrefix namingMap sumEnums = do
    oApi <- readOpenAPI fp
    let compSchemas =  _componentsSchemas . _openApiComponents $ oApi
        compParams = _componentsParameters . _openApiComponents $ oApi
        compReqBodies = _componentsRequestBodies . _openApiComponents $ oApi
        compResponses = _componentsResponses . _openApiComponents $ oApi
        compHeaders = _componentsHeaders . _openApiComponents $ oApi
        oApiName = _infoTitle . _openApiInfo $ oApi
        appName = removeUnsupportedSymbols (upperFirstChar oApiName)
        (modelList, modelSt) = runState
                        (mapM (\(x,y) -> createModelData (_schemaType y) compSchemas (x,y)) (HMO.toList compSchemas))
                        ((initState sumEnums seenVariables) { seenVars = S.empty, jsonInstances = S.fromList seenVariables })
        hsModuleModel = module' (Just modName) Nothing impsModel
                          (concatMap (\(cts, insts) -> rmChildTypeLayer cts ++ rmInstanceLayer insts) modelList)
        -- the contract pass continues the models pass's state: name and
        -- instance dedup must be global or the two modules would emit
        -- duplicate decls/instances (and GHC would refuse the package)
        ((routeInfo,typeSynList,instances), synSt) =
                                   (\(rs, st) -> ((\(a,b,c) -> (Prelude.concat a,Prelude.concat b,Prelude.concat c)) (unzip3 [ (w, x ++ y, z) | (w, x, y, z) <- rs ]), st))
                                   (runState
                                        (mapM (createTypeSynData namingMap appName compSchemas compParams compReqBodies compResponses compHeaders) (filter (T.isPrefixOf (T.pack reqPrefix) . T.pack . fst) (HMO.toList . _openApiPaths $ oApi)))
                                        (modelSt { imports = S.empty, paramRecords = S.empty, bodyTypes = S.empty, resultTypes = S.empty }))

    -- M10: resolved op names are identities (the FQN and the type-level
    -- OperationId) — they must be catalog-unique after sanitizing, and a
    -- naming-map key that matched no op is a typo worth flagging
    let finalOps = [ (omKey om, finalOpName synName methName om)
                   | (synName, ms) <- routeInfo, (methName, (_,_,_,_,_,_,_,om)) <- ms ]
        dups = [ (n, ks)
               | (n, ks) <- HMQ.toList (HMQ.fromListWith (++) [ (n, [k]) | (k, n) <- finalOps ])
               , Prelude.length ks > 1 ]
        unmatched = filter (`Prelude.notElem` fmap fst finalOps) (HMQ.keys namingMap)
    unless (Prelude.null dups) $
      error ("op names collide after resolution (curate the naming map): " <> show dups)
    unless (Prelude.null unmatched) $
      putStrLn ("[openapi] naming-map keys matching no operation: " <> show unmatched)

    let simplifiedRouteInfo = (fmap . fmap) (map fst) routeInfo
        apiContractInstances =  Prelude.concat $ mkApiContractInstances appName <$> routeInfo
        hsModuleTypeSyn = module' (Just typeSynName) Nothing impsTypeSyn (untypedDef:rmChildTypeLayer typeSynList ++ webApiInstance appName simplifiedRouteInfo ++ apiContractInstances ++ rmInstanceLayer instances)
    writeModule (pkgHome </> "src") (modName <.> "hs") es hsModuleModel
    writeModule (pkgHome </> "src") (typeSynName <.> "hs") es2 hsModuleTypeSyn
    writeModuleText (pkgHome </> "src-registry") ("ConcreteRegistry" <.> "hs")
      (concreteRegistryText appName modName typeSynName (map fst (HMO.toList compSchemas)) modelSt synSt routeInfo)
    writeCabal pkgName mkPkgConfig modName [typeSynName] pkgHome

    where impsModel =
                 [ import' "Data.Int"
                 , import' "Data.Vector" `as'` "V"
                 , exposing (import' "GHC.Generics") [var "Generic"]
                 , exposing (import' "Data.Text") [var "Text"]
                 , import' "Data.Aeson"
                 , import' "Control.Applicative"
                 ]
          impsTypeSyn =
                 [ import' "WebApi.Contract"
                 , import' "WebApi.Param"
                 , import' modName
                 , import' "Data.Int"
                 , exposing (import' "GHC.Generics") [var "Generic"]
                 , exposing (import' "Data.Text") [var "Text"]
                 , import' "Data.Vector" `as'` "V"
                 , import' "Data.Aeson"
                 , import' "Control.Applicative"
                 ]

          es = [TypeOperators,KindSignatures,DataKinds,DuplicateRecordFields,DeriveGeneric,OverloadedStrings]
          es2 = [DataKinds,TypeOperators,TypeSynonymInstances,FlexibleInstances,MultiParamTypeClasses,TypeFamilies, OverloadedStrings,DeriveGeneric,DuplicateRecordFields]
          seenVariables = ["Untyped"]
          pkgName = T.unpack . flip T.append "-models" . T.pack . dropExtension . takeFileName $ fp
          pkgHome = destFp </> pkgName
          modName = "OpenApiModels"
          typeSynName = "WebApiInstances"
          dataTypeForOApi a = data' (textToOccNameStr a) [] [] []
          webApiInstance a b = [dataTypeForOApi a,
                                    instance' (var "WebApi" @@ var (textToRdrNameStr a))
                                              [tyFamInst "Apis" [var $ textToRdrNameStr a] (listPromotedTy (oneRoute <$> b))]]
          oneRoute (tName,methList) = var "Route" @@ listPromotedTy (var . textToRdrNameStr <$> methList) @@ var  (textToRdrNameStr tName)
          untypedDef = data' "Untyped" [] [prefixCon "Maybe" [field (var "Text")]] []
          rmChildTypeLayer = fmap ctDecl
          rmInstanceLayer = fmap instDecl

mkApiContractInstances ::
    Text ->
    (Text, [(Text, OpSlot)]) ->
    [HsDecl']
mkApiContractInstances oApiName (typName,instanceInfo) =
    mkOneInstance <$> instanceInfo
    where mkOneInstance (methName,(headInfo,queryInfo,cookieInfo,reqBodyInfo,apiOutInfo,apiErrInfo,headerOutInfo,om)) =
                instance' (var "ApiContract" @@ var (textToRdrNameStr oApiName) @@ var (textToRdrNameStr methName) @@ var (textToRdrNameStr typName))
                          (opIdSyn methName om :
                           Prelude.concat (mkTypeSyns methName <$> [("PathParam",omPathParam om)
                                                                   ,("HeaderIn",headInfo)
                                                                   ,("QueryParam",queryInfo)
                                                                   ,("CookieIn",cookieInfo)
                                                                   ,("RequestBody",reqBodyInfo)
                                                                   ,("ApiOut",apiOutInfo)
                                                                   ,("ApiErr",apiErrInfo)
                                                                   ,("HeaderOut",headerOutInfo)
                                                                   ]))
          -- the modern contract requires an injective OperationId per
          -- (method, route); the name only surfaces in wire logs
          -- the resolved name doubles as the OperationId Symbol — a
          -- legal record-field name, so CompactServer can serve the
          -- contract (the positional fallback keeps the old hyphenated
          -- text)
          opIdSyn methName om = tyFamInst "OperationId"
                                       [var (textToRdrNameStr methName), var (textToRdrNameStr typName)]
                                       (var "'OpId" @@ var (textToRdrNameStr oApiName)
                                                    @@ stringTy (T.unpack (fromMaybe (T.append (T.toLower methName) (T.append "-" typName)) (omName om))))
          mkTypeSyns _ (_,Nothing) = []
          mkTypeSyns methName (tName,Just typ) = [tyFamInst (textToRdrNameStr tName)
                                                            [var (textToRdrNameStr methName),var (textToRdrNameStr typName) ]
                                                            typ]


createTypeSynData ::
    (MonadState ModelGenState m) =>
    NamingMap ->
    Text ->
    Definitions Schema ->
    Definitions Param ->
    Definitions RequestBody ->
    Definitions Response ->
    Definitions Header ->
    (FilePath,PathItem) ->
    m ([(Text, [(Text, OpSlot)])],[ChildType],[ChildType],[Instance])
createTypeSynData namingMap appName compSchemas compsParam compReqBodies compResponses compHeaders (fp,PathItem _ _ piGet piPut piPost piDelete piOptions piHead piPatch piTrace _ piParams) = do
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
    (typSyns,ct1) <- unzip <$> mapM (createTypSynonym appName compSchemas) overridesParam
    (commonTypSyn,ct2) <- unzip <$> let diff = S.difference (pairLisToSet opList) (pairLisToSet overridesParam)
                              in if S.null diff
                              then return []
                              else do
                                 ((a,_b),c) <- createTypSynonym appName compSchemas ("",commonParams)
                                 return [((a,S.toList diff),c)]
    (apiConInsData,ct3,ci3) <-  unzip3 <$> mapM (createApiContractInsData namingMap fp compSchemas compsParam compReqBodies compResponses compHeaders paramsMap) opList
    return ((fmap . fmap) (applyApiContractInfo (unions apiConInsData)) <$> commonTypSyn ++ typSyns
           , Prelude.concat ct1 ++ Prelude.concat ct2   -- the route synonyms
           , Prelude.concat ct3                         -- the contract's own types
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
    NamingMap ->
    FilePath ->
    Definitions Schema ->
    Definitions Param ->
    Definitions RequestBody ->
    Definitions Response ->
    Definitions Header ->
    HashMap Text (Bool, Param) ->
    (Text,Operation) ->
    m (HashMap Text OpSlot,[ChildType],[Instance])
createApiContractInsData namingMap fp compSchemas compsParam compsReqBodies compResponses compHeaders commonParamMap (opName,operationData) = do
    ModelGenState { connectionParams = connParams } <- get
    let opParamsMap = refParamsToParams compsParam (_operationParameters operationData)
        allParams = HM.toList $ opParamsMap `union` commonParamMap
        isConn (n, (_, prm)) = _paramIn prm == ParamQuery && n `S.member` connParams
        overrideParams = filter (not . isConn) allParams
        connTaken = Data.List.sort [ n | x@(n, _) <- allParams, isConn x ]
        opReqBody = _operationRequestBody operationData
        opResponses = _responsesResponses . _operationResponses $ operationData
        responseList = fmap (refValToVal compResponses) <$> HMO.toList opResponses
        defaultResponse = _responsesDefault . _operationResponses $ operationData
        headerOutSchemas = headersToSchema <$>  filter (not . Prelude.null)
                                                    (responseToHeader <$> ( case defaultResponse of
                                                                        Nothing -> responseList
                                                                        (Just x) -> (0,refValToVal compResponses x):responseList))
    -- modular: several captures make a named path record, in path order
    ModelGenState { modular = isModularOp } <- get
    let captures = [ c | Right c <- parseFilePath fp ]
        byName = HM.fromList overrideParams
        pathParams = [ (c, p') | c <- captures, Just p' <- [HM.lookup c byName] ]
    (pathTyp,ct0,ci0) <- if isModularOp && Prelude.length captures >= 2 && Prelude.length pathParams == Prelude.length captures
                         then mkParamRecord ParamPath pathParams
                         else return (Nothing,[],[])
    (headtypTuple,ct1,ci1)  <- createType ParamHeader overrideParams
    (querytypTuple,ct2,ci2)  <- createType ParamQuery  overrideParams
    (cookietypTuple,ct3,ci3') <- createType ParamCookie overrideParams
    (reqBody,ct4,ci4) <- createReqBody compSchemas compsReqBodies opReqBody
    ModelGenState { modular = isModular } <- get
    apiOutResp <- case catMaybes [HMO.lookup x opResponses | x <- [200..299]] of
                    (x : _ : _) | isModular -> do
                      warn (opName <> " " <> T.pack fp <> ": several 2xx responses; the lowest is ApiOut")
                      pure (Just x)
                    _ -> pure (findResponseApiOut opResponses)
    (apiOutType,ct5,ci5) <- createApiOut
                            compSchemas
                            compResponses
                            apiOutResp
                            defaultResponse
    (apiErrType,ct6,ci6) <- createApiErr compSchemas compResponses defaultResponse (HMO.toList(findResponseApiErr opResponses))
    (headerOutType,ct7) <- createHeaderOut compSchemas headerOutSchemas
    return ( HM.singleton opName (headtypTuple,querytypTuple,cookietypTuple,reqBody,apiOutType,apiErrType,headerOutType,opMeta { omPathParam = pathTyp, omConnParams = connTaken })
           , ct0 ++ ct1 ++ ct2 ++ ct3 ++ ct4 ++ ct5 ++ ct6 ++ ct7
           , ci0 ++ ci1 ++ ci2 ++ ci3' ++ ci4 ++ ci5 ++ ci6
           )
    where opMeta = resolveOpMeta namingMap fp opName operationData
          -- param records are named after the op when it has a name —
          -- catalog-wide "GetQP0"-style digit suffixes only for the
          -- positional fallback
          pBase = fromMaybe (T.toLower opName) (omName opMeta)
          mFilter x = filter (\(_a,(_b,c)) -> _paramIn c == x)
          -- Header params are dropped for now: their wire names
          -- (X-NetSuite-*, Prefer) are not legal record fields and the
          -- param codecs read wire names from field names. They are
          -- operational headers, session-level concerns, not operation
          -- vocabulary. Recorded as a spike finding.
          createType ParamHeader b = do
              let hs = fst <$> mFilter ParamHeader b
              ModelGenState { modular = isModular } <- get
              if isModular && not (Prelude.null hs)
              then mkHeaderRecord (mFilter ParamHeader b)
              else do
                when (not (Prelude.null hs)) $
                  traceM ("[openapi] " <> T.unpack opName <> ": dropping header params " <> show hs)
                return (Nothing,[],[])
          createType a b = mkParamRecord a (mFilter a b)
          partLabel ParamQuery = "Q"
          partLabel ParamCookie = "C"
          partLabel ParamPath = "P"
          partLabel _ = "X"
          promotedPart ParamQuery = "'QueryParam"
          promotedPart ParamCookie = "'Cookie"
          promotedPart ParamPath = "'PathParam"
          promotedPart _ = "'QueryParam"
          -- a named record per (operation, part): nominal Generic records
          -- are what both webapi's param codecs and the Dhall bridge walk
          mkParamRecord _ [] = return (Nothing,[],[])
          mkParamRecord loc ps = do
              vName <- mkUnseenVar (T.concat [upperFirstChar pBase, partLabel loc, "P"])
              modify (\st -> st { paramRecords = S.insert vName (paramRecords st) })
              dataTypeInfoList <- mapM (\(pname,(_,param)) ->
                                          parseRecordFields (pname, maySchemaToSchema (_paramSchema param))
                                                            (fromMaybe False (_paramRequired param))
                                                            False Nothing compSchemas) ps
              let (schemaList,childTypes) = unzip $ (\(DataTypeInfo a b c _) -> ((a,b),c) ) <$> dataTypeInfoList
                  wireUnsafe = [x | (x,_) <- schemaList, removeUnsupportedSymbols x /= x]
              ModelGenState { modular = isModular } <- get
              when (not (Prelude.null wireUnsafe)) $
                if isModular
                then warn (fromMaybe opName (omName opMeta) <> ": parameter names change under sanitizing: " <> TQ.intercalate ", " wireUnsafe)
                else traceM ("[openapi] " <> T.unpack opName <> ": param wire names change under sanitizing: " <> show wireUnsafe)
              ModelGenState { keywordsToAvoid } <- get
              let mkFld (x,y) = (textToOccNameStr (avoidKeywords (removeUnsupportedSymbols (lowerFirstChar x)) keywordsToAvoid), field y)
                  decl = dataCT vName $ data' (textToOccNameStr vName) [] [recordCon (textToOccNameStr vName) (mkFld <$> schemaList)] stdDeriving
                  -- a path record is only ever encoded (a client's); webapi
                  -- decodes paths in its router, with no FromParam 'PathParam
                  pInsts = Instance vName (instance' (var "ToParam" @@ var (fromString (promotedPart loc)) @@ var (textToRdrNameStr vName)) [])
                         : [ Instance vName $ instance' (var "FromParam" @@ var (fromString (promotedPart loc)) @@ var (textToRdrNameStr vName)) []
                           | loc /= ParamPath ]
              return (Just (var (textToRdrNameStr vName)), decl : Prelude.concat childTypes, pInsts)
          -- modular: header parameters as a record whose fields are the
          -- header names in snake case (X-Upsert -> x_upsert), and a
          -- ToHeader that sends each under its wire name, an absent
          -- optional header left out (webapi's generic ToHeader would send
          -- the field names themselves)
          mkHeaderRecord ps = do
              vName <- mkUnseenVar (T.concat [upperFirstChar pBase, "HP"])
              modify (\st -> st { paramRecords = S.insert vName (paramRecords st) })
              let fieldOf wire = T.toLower (TQ.replace "-" "_" wire)
              dataTypeInfoList <- mapM (\(pname,(_,param)) ->
                                          parseRecordFields (fieldOf pname, maySchemaToSchema (_paramSchema param))
                                                            (fromMaybe False (_paramRequired param))
                                                            False Nothing compSchemas) ps
              let (schemaList,childTypes) = unzip $ (\(DataTypeInfo a b c _) -> ((a,b),c) ) <$> dataTypeInfoList
              ModelGenState { keywordsToAvoid } <- get
              let mkFld (x,y) = (textToOccNameStr (avoidKeywords x keywordsToAvoid), field y)
                  decl = dataCT vName $ data' (textToOccNameStr vName) [] [recordCon (textToOccNameStr vName) (mkFld <$> schemaList)] stdDeriving
                  hvars = [ ("v" <> show i, pname, fromMaybe False (_paramRequired param)) | (i, (pname, (_, param))) <- zip [1 :: Int ..] ps ]
                  pairE wire x = tuple [var "mk" @@ string (T.unpack wire), var "encodeParam" @@ var x]
                  entry (v, wire, isReq')
                    | isReq' = var "Just" @@ pairE wire (fromString v)
                    | otherwise = var "fmap" @@ lambda [bvar "x"] (pairE wire "x") @@ var (fromString v)
                  inst = Instance vName $ instance' (var "ToHeader" @@ var (textToRdrNameStr vName))
                           [funBind "toHeader" (match [conP (textToRdrNameStr vName) ((\(v,_,_) -> bvar (fromString v)) <$> hvars)]
                                                      (var "catMaybes" @@ list (entry <$> hvars)))]
              return (Just (var (textToRdrNameStr vName)), decl : Prelude.concat childTypes, [inst])
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
    Inline $ mkEmptySchema { _schemaType = Just OpenApiObject
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
        ModelGenState { modular = isModular } <- get
        (cType,maySchema) <- if isModular
                             then fromMaybe (JSON, Nothing) <$> pickMedia "a response" mediaTypList
                             else pure (mediaTypeObjToSchema mediaTypList)
        case maySchema of
          Just (Ref (Reference x)) ->
            modify (\st -> st { resultTypes = S.insert (refName st x) (resultTypes st) })
          _ -> return ()
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
        ModelGenState { modular = isModular } <- get
        picked <- if isModular
                  then catMaybes <$> mapM (pickMedia "an error response") (filter (not . Prelude.null) mediaTypList)
                  else pure (mediaTypeObjToSchema <$> filter (not . Prelude.null) mediaTypList)
        let (cType,neMediaTypList) = unzip $ second maySchemaToSchema <$> picked
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
    ModelGenState { modular = isModular } <- get
    picked <- if isModular
              then pickMedia "a request body" (HMO.toList (_requestBodyContent reqBody))
              else pure (Just (mediaTypeObjToSchema . HMO.toList . _requestBodyContent $ reqBody))
    case picked of
      Just (JSON, _) -> reqBodyOf picked
      Just (other, _) | isModular -> do
        -- form and multipart bodies wait for FormParam/FileParam emission
        warn ("a " <> T.pack (show other) <> " request body is not generated yet; left out")
        return (Nothing, [], [])
      _ | isModular -> return (Nothing, [], [])
        | otherwise -> reqBodyOf picked
  where
   reqBodyOf picked = do
    let (typName,maySchema) = fromMaybe (error "no request body media type") picked
    case maySchema of
      Just (Ref (Reference x)) ->
        modify (\st -> st { bodyTypes = S.insert (refName st x) (bodyTypes st) })
      _ -> return ()
    DataTypeInfo {typ,child_types,child_instances} <- mayBeSchemaToHsType ("requestBody",maySchema) True (Just typName) compSchemas
    let finalType = if typName == JSON
                    then listPromotedTy [typ]
                    else listPromotedTy [var "Content" @@ listPromotedTy [var $ textToRdrNameStr (T.pack . show $ typName)] @@ typ]
    return (Just finalType,child_types, child_instances)

-- | The modular layout's choice among an operation's media types: JSON
-- when there is one (a vendor that also offers PDF or an image keeps its
-- JSON contract), else the one it knows; what it drops is a warning.
pickMedia :: MonadState ModelGenState m => Text -> [(MediaType, MediaTypeObject)] -> m (Maybe (ContentTypesOApi, Maybe (Referenced Schema)))
pickMedia ctx mts = do
    let known = [ (ct, _mediaTypeObjectSchema o, mt) | (mt, o) <- mts, Just ct <- [HM.lookup mt mediaTypeMap] ]
        isJsonish mt = TQ.isSuffixOf "json" (T.pack (show mt))
        jsonish = [ (JSON, _mediaTypeObjectSchema o, mt) | (mt, o) <- mts, isJsonish mt ]
        chosen = case [ k | k@(JSON, _, _) <- known ] ++ jsonish ++ known of
                   (c : _) -> Just c
                   [] -> Nothing
        dropped = [ T.pack (show mt) | (mt, _) <- mts, Just mt /= fmap (\(_, _, m) -> m) chosen ]
    unless (Prelude.null dropped) $
      warn (ctx <> ": media types " <> TQ.intercalate ", " dropped <> " left out"
              <> maybe " (no media type it knows; left out)" (\(_, _, m) -> "; kept " <> T.pack (show m)) chosen)
    pure ((\(c, sch, _) -> (c, sch)) <$> chosen)

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
    Text ->
    Definitions Schema ->
    (Text ,[Either Text (Text, Maybe (Bool, Param))]) ->
    m ((Text,[Text]),[ChildType])
createTypSynonym appName compSchemas(oName,params) = do
    varName <- mkUnseenVar (mkTypeSynName params (T.append oName "R"))
    (typInfo,childTypes) <-unzip <$> mapM (parseTypeSynInfo compSchemas) params
    -- modern route DSL: App :// "seg" :/ Capture :/ ... — plus the bare
    -- rpath under its own name, which the concrete registry ascribes to
    let rpathE = case typInfo of
          [x] -> x
          _ -> foldr1 (`op` ":/") typInfo
    return ((varName, [oName]), synCT varName (
                                  type' (textToOccNameStr varName)
                                        []
                                        (op (var (textToRdrNameStr appName)) "://" rpathE))
                              : synCT (T.append varName "Path") (type' (textToOccNameStr (T.append varName "Path")) [] rpathE)
                              : Prelude.concat childTypes)

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


-- | The op's final emitted name: resolved ('OpMeta') or the positional
-- fallback (method + the route synonym less its R suffix) — one formula
-- shared by the registry emission and the uniqueness check.
finalOpName :: Text -> Text -> OpMeta -> Text
finalOpName synName methName om =
    fromMaybe (T.toLower methName <> fromMaybe synName (TQ.stripSuffix "R" synName)) (omName om)

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
updateSeenVars v st = st { seenVars = S.insert v (seenVars st) }

updateJsonInstances :: Text -> ModelGenState -> ModelGenState
updateJsonInstances v st = st { jsonInstances = S.insert v (jsonInstances st) }

updateSumTypes :: (Text,[Referenced Schema]) -> ModelGenState -> ModelGenState
updateSumTypes (a,b) st = st { createdSums = HM.insert a b (createdSums st) }

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
        fromMaybe (error "Reference Value Not Found") (HMO.lookup a compVals)
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

renderHsType :: HsType' -> Text
renderHsType t = T.pack (renderWithContext defaultSDocContext (ppr t))

-- | Raw-text module emission (the concrete registry is boilerplate
-- around a few varying names; the formatter still gets a pass).
writeModuleText :: FilePath -> String -> Text -> IO ()
writeModuleText destFp fName contents = do
    txt <- ormolu defaultConfig { cfgCheckIdempotence = True } "" contents
              `catch` \(e :: SomeException) -> do
                 putStrLn ("[writeModuleText] formatter failed for " <> fName
                           <> " (writing unformatted): " <> show e)
                 pure contents
    createDirectoryIfMissing True destFp
    T.writeFile (destFp </> fName) txt

writeModule :: FilePath -> String -> [Extension] -> HsModule' -> IO ()
writeModule destFp fName es hsModule = do
    contents <- do
#if (MIN_VERSION_ghc(9,2,0))
      pure (renderWithContext defaultSDocContext (ppr hsModule))  
#else
      dynFlags <- runGhc (Just libdir) getDynFlags
      pure (showSDoc dynFlags (ppr hsModule))
#endif
    let fileContent = concatMap ppExtension es <> contents
    txt <- ormolu defaultConfig { cfgCheckIdempotence = True } "" (T.pack fileContent)
              `catch` \(e :: SomeException) -> do
                 putStrLn ("[writeModule] formatter failed for " <> fName
                           <> " (writing unformatted): " <> show e)
                 pure (T.pack fileContent)
    createDirectoryIfMissing True destFp
    T.writeFile (destFp </> fName) txt --(T.pack fileContent)

-- | The concrete instance layer (M9): the product bridge classes per
-- generated type, and one registration per operation — what a connector
-- vendors beside the contract. Boilerplate around names, so emitted as
-- text; the varying types ride in pretty-printed.
concreteRegistryText
  :: Text            -- ^ the app type name
  -> String -> String
  -> [Text]          -- ^ the component schema names
  -> ModelGenState   -- ^ the models pass's final state
  -> ModelGenState   -- ^ the contract pass's final state
  -> [(Text, [(Text, OpSlot)])]
  -> Text
concreteRegistryText appName modName typeSynName schemaNames modelSt synSt routeInfo = TQ.unlines $
  [ "{-# LANGUAGE DataKinds #-}"
  , "{-# LANGUAGE TypeApplications #-}"
  , "{-# LANGUAGE TypeOperators #-}"
  , "{-# LANGUAGE ScopedTypeVariables #-}"
  , "{-# LANGUAGE OverloadedStrings #-}"
  , "{-# LANGUAGE DuplicateRecordFields #-}"
  , "{-# LANGUAGE DisambiguateRecordFields #-}"
  , "{-# OPTIONS_GHC -Wno-orphans #-}"
  , ""
  , "-- Generated by openapi-model-generator: the concrete registry for " <> appName <> "."
  , "module ConcreteRegistry (" <> nsOpsName <> ") where"
  , ""
  , "import Data.List.NonEmpty (NonEmpty (..))"
  , "import Data.Maybe (fromMaybe)"
  , "import Data.Text (Text)"
  , "import qualified Data.Text as T"
  , "import qualified Data.UUID.Types as UUID"
  , "import GHC.Stack (HasCallStack)"
  , ""
  , "import WebApi.Contract hiding (OperationId)"
  , "import WebApi.Client.Session (AppIsElem, getSuccessOut)"
  , ""
  , "import Data.Vector (Vector)"
  , ""
  , "import Dhall.Do.Api.Bridge"
  , "import Dhall.Do.Api.Id (OperationId, FQN (..), mkOperationId)"
  , "import Dhall.Do.Api.WebApi.Concrete.Binding"
  , ""
  , "import " <> T.pack modName
  , "import " <> T.pack typeSynName
  , ""
  ]
  <> concatMap instanceLines (S.toList allTypes)
  <> [ ""
     , "_unusedVectorAnchor :: Maybe (Vector ()) "
     , "_unusedVectorAnchor = Nothing"
     , ""
     , "opIdOf :: HasCallStack => Text -> OperationId"
     , "opIdOf t = mkOperationId (fromMaybe (error (\"bad uuid literal: \" <> T.unpack t)) (UUID.fromText t))"
     , ""
     , "mkFqn :: Text -> FQN"
     , "mkFqn n = FQN { qualifier = \"" <> nsQualifier <> "\" :| [], name = n }"
     , ""
     , nsOpsName <> " :: forall apps. AppIsElem " <> appName <> " apps => ConcreteActions apps -> ConcreteActions apps"
     , nsOpsName <> " ="
     ]
  <> registrationLines
  where
    nsQualifier = T.toLower appName
    nsOpsName = nsQualifier <> "ConcreteOps"

    allTypes = SetQ.unions
      [ S.fromList (map (removeUnsupportedSymbols . upperFirstChar) schemaNames)
      , S.fromList (map (removeUnsupportedSymbols . upperFirstChar) (HMQ.keys (createdSums modelSt)))
      , inlineRecords modelSt, inlineRecords synSt
      , paramRecords synSt, bodyTypes synSt, resultTypes synSt
      , enumTypes modelSt, enumTypes synSt
      ]
    requestSide = SetQ.union (paramRecords synSt) (bodyTypes synSt)
    -- every named ApiErr type; () and Text carry ErrorText already
    errorTypes = S.fromList
      [ t | (_, methodInfos) <- routeInfo
          , (_, (_h,_q,_c,_b,_o,Just errT,_ho,_om)) <- methodInfos
          , let t = renderHsType errT
          , t `Prelude.notElem` ["()", "Text"] ]
    resultSide = resultTypes synSt
    -- OverrideType/HsSelect have no generic sum story (Override.hs /
    -- Select.hs carry no :+: instance) — suppress their emission for
    -- sum-like types instead of emitting uncompilable empty instances
    sumLike = SetQ.unions
      [ enumTypes modelSt, enumTypes synSt
      , S.fromList (map (removeUnsupportedSymbols . upperFirstChar) (HMQ.keys (createdSums modelSt)))
      , S.fromList (map (removeUnsupportedSymbols . upperFirstChar) (HMQ.keys (createdSums synSt)))
      ]

    instanceLines n =
      [ "instance HsType " <> n
      , "instance ToHsVal " <> n
      , "instance FromHsVal " <> n
      ]
      <> [ "instance OverrideType " <> n | n `S.member` requestSide, not (n `S.member` sumLike) ]
      <> [ "instance HsSelect " <> n | n `S.member` resultSide, not (n `S.member` sumLike) ]
      -- a run's failure line renders the error body (dhall-do-api's
      -- ErrorText; the class default goes through ToJSON)
      <> [ "instance ErrorText " <> n | n `S.member` errorTypes ]
      <> [ "" ]

    ops = [ (synName, methName, outT, om)
          | (synName, methodInfos) <- routeInfo
          , (methName, (_h,_q,_c,_b,outInfo,_e,_ho,om)) <- methodInfos
          , let outT = maybe "()" renderHsType outInfo
          ]

    registrationLines =
      [ "    " <> (if i == 0 then "  " else ". ") <> registrationExpr appName op'
      | (i, op') <- zip [0 :: Int ..] ops
      ]


-- | One operation's registration: its id (curated, or the sha of its final
-- name), FQN, summary and curated request defaults, ascribed its binding
-- type. Shared by the legacy and modular registries.
registrationExpr :: Text -> (Text, Text, Text, OpMeta) -> Text
registrationExpr appName (synName, methName, outT, om) =
    "addConcreteOp (opIdOf \"" <> registryOpUuid appName synName methName om <> "\") (mkFqn \"" <> finalOpName synName methName om <> "\") (ConcreteOp ((concreteBinding (Right . getSuccessOut)) { cbSummary = Just \"" <> registryEscape (fromMaybe (finalOpName synName methName om) (omSummary om)) <> "\"" <> registryDefaultsField om <> " } :: ConcreteBinding apps " <> methName <> " " <> appName <> " " <> synName <> "Path (" <> TQ.replace "\n" " " outT <> ")))"

-- curated defaults ride in on the binding's typed request (design D1):
-- one setter per part over emptyRequest, one setField per curated
-- field over unsetRecord — every name GHC-checked against the record
registryDefaultsField :: OpMeta -> Text
registryDefaultsField om = case omDefaults om of
    Nothing -> ""
    Just parts ->
      ", cbRequest = "
        <> foldr
             (\(part, flds) inner ->
                partSetter part <> " ("
                  <> TQ.concat [ "setField @\"" <> fld <> "\" (Const (" <> expr <> ")) " | (fld, expr) <- HMQ.toList flds ]
                  <> "unsetRecord) (" <> inner <> ")")
             "emptyRequest"
             (HMQ.toList parts)
  where
    partSetter = \case
      "query" -> "setQuery"
      "form" -> "setForm"
      "header" -> "setHeader"
      "path" -> "setPath"
      "body" -> "setBody"
      "file" -> "setFile"
      other -> error ("naming map: unknown request part " <> T.unpack other)

-- a curated uuid pins the action id (published corpora reference
-- it); otherwise the deterministic sha of the final name
registryOpUuid :: Text -> Text -> Text -> OpMeta -> Text
registryOpUuid appName synName methName om =
    fromMaybe (uuidFromSeed ("dhall-do-connector|" <> appName <> "|" <> finalOpName synName methName om)) (omUuid om)

-- a deterministic 32-hex identity for the seed, laid out as a UUID
uuidFromSeed :: Text -> Text
uuidFromSeed seed =
    let hexed = T.pack (concatMap byteHex (BSS.unpack (SHA256.hash (TE.encodeUtf8 seed))))
        h a b = TQ.take b (TQ.drop a hexed)
    in TQ.intercalate "-" [h 0 8, h 8 4, h 12 4, h 16 4, h 20 12]
  where byteHex b = let d k = "0123456789abcdef" !! fromIntegral k in [d (b `div` 16), d (b `mod` 16)]

registryEscape :: Text -> Text
registryEscape = TQ.replace "\"" "'" . TQ.replace "\\" "/"

-- | A declaration as source text (the modular layout writes module
-- headers itself and renders each declaration).
renderDecl :: HsDecl' -> Text
renderDecl d = T.pack (renderWithContext defaultSDocContext (ppr d))

-- Written directly rather than shelled out to @cabal init@: init's
-- @--overwrite@ moves an existing src/ aside, clobbering the modules
-- this generator just wrote.
writeCabal :: String -> PkgConfig -> String -> [String] -> FilePath ->IO ()
writeCabal pkgName (PkgConfig aName aEmail) modName exposMods pkgHome = do
    createDirectoryIfMissing True pkgHome
    Prelude.writeFile (pkgHome </> pkgName <.> "cabal") cabalFile
    where cabalFile = unlines
            [ "cabal-version:      3.0"
            , "name:               " ++ pkgName
            , "version:            0.1.0.0"
            , "synopsis:           Generated webapi contract (openapi-model-generator)"
            , "author:             " ++ T.unpack aName
            , "maintainer:         " ++ T.unpack aEmail
            , "build-type:         Simple"
            , ""
            , "library"
            , "    exposed-modules:  " ++ Data.List.intercalate ", " xposedMods
            , "    hs-source-dirs:   src"
            , "    default-language: Haskell2010"
            , "    build-depends:    " ++ Data.List.intercalate ", " dpends
            , ""
            , "-- the concrete registry (M9): the instance layer + registrations"
            , "-- a connector mounts; kept a sublibrary so the contract itself"
            , "-- stays free of the executor's closure"
            , "library registry"
            , "    visibility:       public"
            , "    exposed-modules:  ConcreteRegistry"
            , "    hs-source-dirs:   src-registry"
            , "    default-language: Haskell2010"
            , "    build-depends:    " ++ Data.List.intercalate ", "
                (dpends ++ [pkgName, "uuid-types", "webapi-session", "dhall", "dhall-do-api", "dhall-do-api-webapi"])
            ]
          dpends = ["base","text","vector","aeson","webapi-contract"]
          xposedMods = modName:exposMods

ppExtension :: Extension -> String
ppExtension e = "{-# LANGUAGE " <> show e <> " #-}\n"

-- M10: the models pass emits the To/FromJSON layer beside each data
-- decl — the 1,000+ hand-rolled instances were landing in the contract
-- module (the type-checker hot spot) purely because only that pass set
-- generateInstance. The contract pass is seeded from this pass's state,
-- so it emits instances only for contract-born types.
createModelData ::
    (MonadState ModelGenState m) =>
    Maybe OpenApiType -> Definitions Schema -> (Text,Schema) -> m ([ChildType],[Instance])
createModelData (Just OpenApiObject) _ (dName,dSchema)
  | HMO.null (_schemaProperties dSchema) = opaqueComponent dName "an object with no properties"
createModelData (Just OpenApiObject) compSchemas (dName,dSchema) = do
    let reqParams =  _schemaRequired dSchema
    unseenVar <- componentDeclName dName
    dataTypeInfoList <- mapM (\(x,y) -> parseRecordFields (x,y) (x `elem` reqParams) True (Just JSON) compSchemas)  (HMO.toList . _schemaProperties $ dSchema)
    let (rFields,childTypes,childInsts) = unzip3 $ (\(DataTypeInfo a b c d) -> ((a,b),c,d)) <$> dataTypeInfoList
    ModelGenState { keywordsToAvoid } <- get
    let frFields = (\(x,y)-> (textToOccNameStr $ avoidKeywords x keywordsToAvoid,y)) .
                      bimap (removeUnsupportedSymbols . lowerFirstChar) field  <$> rFields
    ModelGenState { jsonInstances } <- get
    ownInsts <- if member unseenVar jsonInstances
                then return []
                else do
                   modify (updateJsonInstances unseenVar)
                   fj <- createFromJsonInstancesRecord unseenVar dSchema compSchemas
                   tj <- createToJsonInstancesRecord unseenVar dSchema
                   return [fj, tj]
    return ( Prelude.concat childTypes ++
             [dataCT unseenVar $ data' (textToOccNameStr unseenVar) [] [recordCon (textToOccNameStr unseenVar) frFields] stdDeriving]
           , Prelude.concat childInsts ++ ownInsts )
createModelData Nothing compSchemas (dName,dSchema) =
    case _schemaOneOf dSchema of
        Just x@(_ : _ : _) -> do
            DataTypeInfo {child_types, child_instances} <- mkSumType dName True x True True (Just JSON) compSchemas
            return (child_types, child_instances)
        Just [_] -> legacyOr (error "Bad OneOf Specification") (opaqueComponent dName "a oneOf of one")
        Just [] -> legacyOr (error "Bad OneOf Specification") (opaqueComponent dName "an empty oneOf")
        Nothing
          | not (HMO.null (_schemaProperties dSchema)) ->
              createModelData (Just OpenApiObject) compSchemas (dName,dSchema)
          | otherwise -> legacyOr (error "Unexpected Schema type") (opaqueComponent dName "no type")
createModelData (Just OpenApiArray) compSchemas (dName,dSchema) =
    case _schemaItems dSchema of
        Just (OpenApiItemsObject sch) -> do
            unseenVar <- componentDeclName dName
            let occUnseenVar = textToOccNameStr unseenVar
            DataTypeInfo {typ,child_types,child_instances} <- parseRecordFields (dName,sch) True True (Just JSON) compSchemas
            let toptype = type' occUnseenVar [] (var "Vector" @@ typ)
            return (synCT unseenVar toptype:child_types, child_instances)
        Nothing -> legacyOr (error "No _schemaItems value for Array") (opaqueArrayComponent dName "an array with no items")
        Just (OpenApiItemsArray _) -> legacyOr (error "OpenApiItemsArray Array type") (opaqueArrayComponent dName "a tuple array")

createModelData (Just OpenApiString) _ (dName,dSchema)
    | Just vals@(_ : _) <- _schemaEnum dSchema, all isStringValue vals = do
        ModelGenState { enumsAsSums, enumSums, modular = isModular } <- get
        if not enumsAsSums
        then do
          unseenVar <- componentDeclName dName
          return (mkTopLevelBaseType "Text" unseenVar, [])
        else if isModular
        then do
          -- the enum is interned by value set (shared across modules, so it
          -- lands in the common module); the component is a synonym for it
          declName <- componentDeclName dName
          DataTypeInfo {typ, child_types, child_instances} <-
            mkEnumType (T.append declName "E") dName vals True True
          return (synCT declName (type' (textToOccNameStr declName) [] typ) : child_types, child_instances)
        else case HMQ.lookup (enumKey vals) enumSums of
          -- the set is already a type under another name: alias to it
          Just existing -> do
            unseenVar <- mkUnseenVar (upperFirstChar dName)
            return (mkTopLevelBaseType existing unseenVar, [])
          Nothing -> do
            DataTypeInfo {child_types, child_instances} <-
              mkEnumType (upperFirstChar dName) dName vals True True
            return (child_types, child_instances)
createModelData (Just OpenApiNull) _ (dName,_) =
    legacyOr (error "Top Level Schema Type: Null") (opaqueComponent dName "the null type")
createModelData (Just a) _ (dName,dSchema) = do
    unseenVar <- componentDeclName dName
    topType <- findTopType a
    return (mkTopLevelBaseType topType unseenVar, [])
    where findTopType OpenApiString = pure "Text"
          findTopType OpenApiNumber = pure "Double"
          findTopType OpenApiInteger = integerType dName (_schemaFormat dSchema)
          findTopType OpenApiBoolean = pure "Bool"
          findTopType _ = error "Top Level Schema : Invalid State"

-- | The legacy layout's behaviour, or the modular layout's.
legacyOr :: MonadState ModelGenState m => m a -> m a -> m a
legacyOr legacy modern = do
    ModelGenState { modular = isModular } <- get
    if isModular then modern else legacy

-- | A component the generator cannot type: the JSON it carries, kept whole.
opaqueComponent :: MonadState ModelGenState m => Text -> Text -> m ([ChildType],[Instance])
opaqueComponent dName why = do
    n <- componentDeclName dName
    warn ("component " <> dName <> ": " <> why <> "; typed as Opaque")
    return (mkTopLevelBaseType "Opaque" n, [])

opaqueArrayComponent :: MonadState ModelGenState m => Text -> Text -> m ([ChildType],[Instance])
opaqueArrayComponent dName why = do
    n <- componentDeclName dName
    warn ("component " <> dName <> ": " <> why <> "; typed as Vector Opaque")
    return ([synCT n (type' (textToOccNameStr n) [] (var "Vector" @@ var "Opaque"))], [])

isStringValue :: Value -> Bool
isStringValue = \case
    String _ -> True
    _ -> False

-- | An integer format as its Haskell type; an unknown format is Int.
integerType :: MonadState ModelGenState m => Text -> Maybe Text -> m Text
integerType ctx fmt = do
    ModelGenState { modular = isModular } <- get
    case fmt of
      Just x | T.take 3 (upperFirstChar x) == "Int" -> pure (upperFirstChar x)
             | isModular -> do
                 warn (ctx <> ": integer format " <> x <> " is not intN; typed as Int")
                 pure "Int"
             | otherwise -> error "Invalid Integer Format"
      Nothing -> pure "Int"

mkTopLevelBaseType :: Text -> Text -> [ChildType]
mkTopLevelBaseType x n = [synCT n $ type' (textToOccNameStr n) []  (var $ textToRdrNameStr x)]

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
-- A Ref names a component schema, and every component's decl AND
-- instances are emitted where the models pass processes that component —
-- recursing here would only re-walk it, and the recursive walk discards
-- inline child DECLS while registering their names, losing the decl for
-- good (found by the M10 relocation). So a Ref is only ever a name.
parseRecordFields (dName,Ref (Reference x)) isReq _generateInstance _instanceType _compSchemas = do
    st <- get
    let sName = refName st x
    return $ DataTypeInfo dName (createHsType isReq sName) [] []
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
createJsonInstances _ _ _ _ = return []

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
    return $ Instance dName fromjsonInst

createFromJsonFieldExpr ::
    (MonadState ModelGenState m) =>
    Definitions Schema ->
    Text ->
    [Text] ->
    [(Text,Referenced Schema)] ->
    m HsExpr'
createFromJsonFieldExpr compSchemas dName reqParams schemaProps = do
    ModelGenState {keywordsToAvoid, modular = isModular} <- get
    let _unused = keywordsToAvoid
        -- the JSON key is the wire name, verbatim; sanitizing is only
        -- for the Haskell field/constructor side. Legacy reads an absent
        -- array as empty; modular reads every optional field as Maybe.
        fieldExpr (x,y) = if not isModular && _schemaType (refValToVal compSchemas y) == Just OpenApiArray && notElem x reqParams
                               then op (op (var "v")
                                           (findSeparatorSymbol False)
                                           (string . T.unpack $ x))
                                       ".!="
                                       (var "V.empty")
                               else op (var "v")
                                       (findSeparatorSymbol (x `elem` reqParams))
                                       (string . T.unpack $ x)
        instInfo = fieldExpr <$> schemaProps
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
    ModelGenState {keywordsToAvoid, modular = isModular} <- get
    pure (if isModular then modularToJson schemaName schemaVal else legacyToJson keywordsToAvoid schemaName schemaVal)

legacyToJson :: Set Text -> Text -> Schema -> Instance
legacyToJson keywordsToAvoid schemaName schemaVal =
    let schemaProperties = HMO.toList . _schemaProperties $ schemaVal
        -- pattern variables are the sanitized field names; the JSON key
        -- stays the wire name
        sanitize = (`avoidKeywords` keywordsToAvoid) . lowerFirstChar . removeUnsupportedSymbols
        wireFields = fst <$> schemaProperties
        rFieldList = bvar . textToOccNameStr . sanitize <$> wireFields
        associationList = list $ (\x -> op (string . T.unpack $ x) ".=" (var . textToRdrNameStr . sanitize $ x)) <$> wireFields
        toJsonInst = instance' (var "ToJSON" @@ var (textToRdrNameStr schemaName))
                               [funBind "toJSON" (match [conP (textToRdrNameStr schemaName) rFieldList] (var "object" @@ associationList) ) ]
    in Instance schemaName toJsonInst

-- | The modular layout's ToJSON: an absent optional field is left out of
-- the object (not sent as null), so an update touches only what it names;
-- pattern variables are positional, so a field called @object@ cannot
-- shadow the function that builds the object.
modularToJson :: Text -> Schema -> Instance
modularToJson schemaName schemaVal =
    Instance schemaName $
      instance' (var "ToJSON" @@ var (textToRdrNameStr schemaName))
        [funBind "toJSON" (match [conP (textToRdrNameStr schemaName) (bvar . fromString . fst <$> vars)]
                                 (var "object" @@ (var "catMaybes" @@ list (pair <$> vars))))]
    where props = fst <$> HMO.toList (_schemaProperties schemaVal)
          reqs = _schemaRequired schemaVal
          vars = [ ("v" <> show i, p) | (i, p) <- zip [1 :: Int ..] props ]
          pair (v, p)
            | p `elem` reqs = var "Just" @@ op (string (T.unpack p)) ".=" (var (fromString v))
            | otherwise = var "fmap" @@ lambda [bvar "x"] (op (string (T.unpack p)) ".=" (var "x")) @@ var (fromString v)

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
parseInlineFields (Just OpenApiString) dName dSchema isReq generateInstance instanceType _compSchemas = do
    ModelGenState { enumsAsSums } <- get
    case _schemaEnum dSchema of
      -- sums only in JSON contexts: param records keep Text (their
      -- Encode/DecodeParam story is a documented cut line)
      Just vals@(_ : _) | enumsAsSums && instanceType == Just JSON && all isStringValue vals ->
        mkEnumType (T.append (upperFirstChar dName) "E") dName vals isReq generateInstance
      _ -> return $ DataTypeInfo dName (createHsType isReq "Text") [] []
parseInlineFields (Just OpenApiNumber ) dName _dSchema isReq _ _ _=
    return $ DataTypeInfo dName (createHsType isReq "Double") [] []
parseInlineFields (Just OpenApiInteger) dName dSchema isReq _ _ _= do
    parsedInt <- integerType dName (_schemaFormat dSchema)
    return $ DataTypeInfo dName (createHsType isReq parsedInt) [] []
parseInlineFields (Just OpenApiBoolean) dName _dSchema isReq _ _ _=
    return $ DataTypeInfo dName (createHsType isReq "Bool") [] []
parseInlineFields (Just OpenApiArray ) dName dSchema isReq generateInstance instanceType compSchemas = do
    ModelGenState { modular = isModular } <- get
    -- legacy: an array is never Maybe (absent reads as empty); modular: an
    -- optional array is Maybe, so an update can leave a list alone rather
    -- than send [] and clear it
    let wrap t = if isModular && not isReq then var "Maybe" @@ t else t
    case _schemaItems dSchema of
        Just (OpenApiItemsObject sch) -> do
            DataTypeInfo {..} <- parseRecordFields (dName,sch) True generateInstance instanceType compSchemas
            return $ DataTypeInfo pName (wrap (var "Vector" @@ typ)) child_types child_instances
        Nothing -> legacyOr (error "No _schemaItems value for Array") (opaqueField wrap "an array with no items")
        Just (OpenApiItemsArray _) -> legacyOr (error "OpenApiItemsArray Array type") (opaqueField wrap "a tuple array")
    where opaqueField wrap why = do
            warn (dName <> ": " <> why <> "; typed as Vector Opaque")
            return $ DataTypeInfo dName (wrap (var "Vector" @@ var "Opaque")) [] []
parseInlineFields (Just OpenApiNull ) dName _dSchema isReq _ _ _=
    legacyOr (error "Null OpenApi Type") $ do
      warn (dName <> ": the null type; typed as Opaque")
      return $ DataTypeInfo dName (createHsType isReq "Opaque") [] []

-- An inline object becomes a named record rather than an anonymous
-- @Rec@: the Dhall bridge's generic walks (and webapi's param codecs)
-- work over nominal 'Generic' records. The name is a pure function of
-- the field list, so the two generator passes (models, JSON instances)
-- agree on it without sharing state, and NetSuite's ubiquitous
-- @{id, refName}@ reference idiom collapses to one shared type.
parseInlineFields (Just OpenApiObject ) dName dSchema isReq generateInstance instanceType compSchemas = do
  ModelGenState { modular = isModular } <- get
  if isModular && HMO.null (_schemaProperties dSchema)
  then do
    warn (dName <> ": an object with no properties; typed as Opaque")
    return $ DataTypeInfo dName (createHsType isReq "Opaque") [] []
  else do
      dataTypeInfoList <- mapM (\(x,y) -> parseRecordFields (x,y) (x `elem` reqParams) generateInstance instanceType compSchemas) (HMO.toList . _schemaProperties $ dSchema)
      let (childInlines,childTypes,childInstances) = unzip3 $ (\(DataTypeInfo a b c d) -> ((a,b),c,d)) <$> dataTypeInfoList
      ModelGenState { inlinePrefix = objPrefix } <- get
      let baseName = T.append objPrefix (T.concat (upperFirstChar . removeUnsupportedSymbols . fst <$> childInlines))
          -- the SHAPE, not just the field names: two {refName,id} objects
          -- whose id fields carry different enums are different types —
          -- the old names-only key silently aliased them (first won)
          shape = [ (fn, renderHsType ft) | (fn, ft) <- childInlines ]
      -- seenVars BEFORE interning: a freshly minted variant name is put
      -- into seenVars by mkUnseenVar itself, and must still get its decl
      ModelGenState { seenVars = seenBefore } <- get
      (vName, isNewName) <- internInlineShape baseName shape
      let objType = var (textToRdrNameStr vName)
      ModelGenState { keywordsToAvoid } <- get
      let mkFld (x,y) = (textToOccNameStr (avoidKeywords (removeUnsupportedSymbols (lowerFirstChar x)) keywordsToAvoid), field y)
          objDecl = dataCT vName $ data' (textToOccNameStr vName) [] [recordCon (textToOccNameStr vName) (mkFld <$> childInlines)] stdDeriving
          newDecls = if isNewName && not (member vName seenBefore) then [objDecl] else []
      modify (updateSeenVars vName)
      modify (\st -> st { inlineRecords = S.insert vName (inlineRecords st) })
      jsonInsts <- if generateInstance
                   then do
                      ModelGenState { jsonInstances } <- get
                      if member vName jsonInstances
                      then return []
                      else do
                         modify (updateJsonInstances vName)
                         fj <- createFromJsonInstancesRecord vName dSchema compSchemas
                         tj <- createToJsonInstancesRecord vName dSchema
                         return [fj, tj]
                   else return []
      return $ DataTypeInfo dName
                            (if isReq then objType else var "Maybe" @@ objType)
                            (Prelude.concat childTypes ++ newDecls)
                            (Prelude.concat childInstances ++ jsonInsts)
  where reqParams =  _schemaRequired dSchema


parseInlineFields Nothing dName dSchema isReq generateInstance instanceType compSchemas=
    case _schemaOneOf dSchema of
        Nothing -> if Prelude.null (_schemaProperties dSchema)
                   then legacyOr (error "Unexpected Schema type") $ do
                          warn (dName <> ": no type; typed as Opaque")
                          return $ DataTypeInfo dName (createHsType isReq "Opaque") [] []
                   else parseInlineFields (Just OpenApiObject) dName dSchema isReq generateInstance instanceType compSchemas
        Just x -> mkSumType dName isReq x False generateInstance instanceType compSchemas

-- | First shape under a base name keeps the bare name (text-mode output
-- is byte-stable); a different shape gets a fresh suffixed name.
internInlineShape ::
    (MonadState ModelGenState m) => Text -> [(Text, Text)] -> m (Text, Bool)
internInlineShape baseName shape = do
    ModelGenState { inlineShapes = shapeTbl, modular = isModular, curModule = m } <- get
    -- modular: a shape is shared within its module only, so no module
    -- depends on a sibling for an inline record
    let key = if isModular then T.concat [m, "/", baseName] else baseName
        entries = fromMaybe [] (HMQ.lookup key shapeTbl)
    case Prelude.lookup shape entries of
      Just name -> return (name, False)
      Nothing -> do
        name <- if Prelude.null entries
                then return baseName
                else mkUnseenVar (T.append baseName "V")
        modify (\st -> st { inlineShapes =
          HMQ.insertWith (++) key [(shape, name)] (inlineShapes st) })
        return (name, True)

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
                let oneOfTyp = dataCT vName $ data' (textToOccNameStr vName) [] ((\(a,b) -> prefixCon (textToOccNameStr a) [field b]) <$> typList) stdDeriving
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
    if member tName jsonInstances
    then return []
    else do
        modify (updateJsonInstances tName)
        let fromJsonInst = createFromJsonInstanceSumType tName consList
            toJsonInst = createToJsonInstanceSumType tName consList
        return [fromJsonInst,toJsonInst]
createInstancesSumType _ _ _= error "Unhandled MediaType Sum"

createFromJsonInstanceSumType ::
    Text ->
    [Text] ->
    Instance
createFromJsonInstanceSumType tName consList = Instance tName $
    instance' (var "FromJSON" @@ var (textToRdrNameStr tName))
              [funBind "parseJSON" $ match [bvar "v"] fieldInfo]
    where fieldInfo = foldl1 (`op` "<|>") $ (\x -> op (var (textToRdrNameStr x)) "<$>" (var "parseJSON" @@ var "v")) <$> consList


createToJsonInstanceSumType ::
    Text ->
    [Text] ->
    Instance
createToJsonInstanceSumType tName consList = Instance tName $
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
        Just x ->
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

-- | The canonical identity of an enum: its sorted value list. Sorting
-- also fixes constructor order, so a later occurrence (any doc order)
-- rebuilds the identical type.
enumKey :: [Value] -> [Text]
enumKey = Data.List.sort . map asEnumStr

asEnumStr :: Value -> Text
asEnumStr = \case
    String s -> s
    other -> error ("non-string enum value unsupported: " <> show other)

-- | An enum schema as a real sum (M10c-4, --enumMode=sum): interned by
-- value-set (the catalog repeats the same 175 sets across 400+ fields),
-- nullary constructors prefixed by the type name (constructors share the
-- module namespace across 175 types), and a wire table for To/FromJSON —
-- enum values are rarely identifier-safe ("27", "Asia/Katmandu").
mkEnumType ::
    (MonadState ModelGenState m) =>
    Text -> Text -> [Value] -> Bool -> Bool -> m DataTypeInfo
mkEnumType nameHint dName vals isReq generateInstance = do
    let key = enumKey vals
    ModelGenState { enumSums = knownEnums } <- get
    case HMQ.lookup key knownEnums of
      Just tyName -> do
        insts <- emitEnumInstancesOnce tyName key generateInstance
        return $ DataTypeInfo dName (createHsType isReq tyName) [] insts
      Nothing -> do
        tyName <- mkUnseenVar nameHint
        modify (\st -> st { enumSums = HMQ.insert key tyName (enumSums st)
                          , enumTypes = S.insert tyName (enumTypes st) })
        let decl = dataCT tyName (data' (textToOccNameStr tyName) []
                     [ prefixCon (textToOccNameStr c) [] | (c, _) <- enumCtors tyName key ]
                     stdDeriving)
        insts <- emitEnumInstancesOnce tyName key generateInstance
        return $ DataTypeInfo dName (createHsType isReq tyName) [decl] insts

-- constructors from the canonical order; mangle-collisions ("a-b" vs
-- "aB") get index suffixes deterministically
enumCtors :: Text -> [Text] -> [(Text, Text)]
enumCtors tyName key = go S.empty key
    where go _ [] = []
          go seen (v : vs) =
            let base = T.append tyName (mangleEnumPiece v)
                c = if member base seen then bump (2 :: Integer) base else base
                bump n b = let b' = T.append b (T.pack (show n))
                           in if member b' seen then bump (n + 1) b else b'
            in (c, v) : go (S.insert c seen) vs

mangleEnumPiece :: Text -> Text
mangleEnumPiece v =
    case Prelude.filter (not . TQ.null) (TQ.split (not . isAlphaNum) v) of
      [] -> "U"
      pieces -> T.concat (upperFirstChar <$> pieces)

emitEnumInstancesOnce ::
    (MonadState ModelGenState m) => Text -> [Text] -> Bool -> m [Instance]
emitEnumInstancesOnce tyName key generateInstance = do
    ModelGenState { jsonInstances } <- get
    if not generateInstance || member tyName jsonInstances
    then return []
    else do
      modify (updateJsonInstances tyName)
      let pairs = enumCtors tyName key
          toJ = Instance tyName (instance' (var "ToJSON" @@ var (textToRdrNameStr tyName))
                  [funBinds "toJSON"
                     [ match [conP (textToRdrNameStr c) []] (var "String" @@ string (T.unpack w))
                     | (c, w) <- pairs ]])
          fromJ = Instance tyName (instance' (var "FromJSON" @@ var (textToRdrNameStr tyName))
                  [valBind "parseJSON"
                     (op (var "withText" @@ string (T.unpack tyName)) "$"
                        (lambda [conP_ "t"]
                           (var "maybe" @@ (var "fail" @@ string ("unexpected " <> T.unpack tyName <> " value"))
                                        @@ var "pure"
                                        @@ (var "lookup" @@ var "t"
                                              @@ list [ tuple [string (T.unpack w), var (textToRdrNameStr c)]
                                                      | (c, w) <- pairs ]))))])
      return [fromJ, toJ]

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


retOApi :: Either String p -> p
retOApi (Right x) = x
retOApi (Left x) = error $ "Can't decode OpenAPI spec file : " ++ show x

readOpenAPI :: FilePath -> IO OpenApi
readOpenAPI fp = do
    fileContent <- B.readFile fp
    return $ retOApi (eitherDecode fileContent :: Either String OpenApi)
