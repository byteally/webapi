{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | The modular layout: one cabal package per API, split into
-- sublibraries by who consumes them.
--
-- The input is one OpenAPI document whose top-level @x-zb@ object says
-- which module each component schema and path belongs to (zenbridge
-- connectors' @tools/zbc normalize@ writes it). For a module prefix @P@ and
-- modules @M@:
--
-- > library model-<m>    P.Model.<M>             the types, JSON instances
-- > library              P.App                   the app type
-- >                      P.Routes.<M>            route synonyms
-- >                      P.Contract              instance WebApi (the Apis list)
-- >                      P.Contract.<M>          param records, ApiContract instances
-- > library registry     P.Registry.Instances.<M> dhall-do bridge instances for the models
-- >                      P.Registry.Ops.<M>      the module's operations
-- >                      P.Registry              the whole registry: <opsName>
--
-- @WebApi@ is a superclass of @ApiContract@, so the @Apis@ list (which
-- names every route) and the contract instances (which need the WebApi
-- instance) live in different modules: routes, then the list, then the
-- instances.
module WebApi.OpenAPI.Modular
  ( GenConfig (..)
  , generateModular
  ) where

import Control.Exception (SomeException, catch)
import Control.Monad (forM, forM_, unless, when)
import Control.Monad.State.Class (modify)
import Control.Monad.State.Lazy (runState)
import Data.Aeson (FromJSON (..), Value (..), eitherDecode, encode, object, withObject, (.!=), (.:), (.:?), (.=))
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy as BL
import qualified Data.HashMap.Strict as HM
import qualified Data.HashMap.Strict.InsOrd as HMO
import Data.List (nub, sort)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.OpenApi
  ( AdditionalProperties (..)
  , Components (..)
  , OpenApi (..)
  , OpenApiItems (..)
  , Reference (..)
  , Referenced (..)
  , Schema (..)
  )
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import GHC.SourceGen (instance', listPromotedTy, tyFamInst, var, (@@))
import Ormolu (Config (cfgCheckIdempotence), defaultConfig, ormolu)
import System.Directory (createDirectoryIfMissing, removePathForcibly)
import System.FilePath (takeDirectory, (</>))
import System.IO (hPutStrLn, stderr)
import WebApi.OpenAPI

-- | What the modular layout needs to know about the package it writes.
data GenConfig = GenConfig
  { gcInput :: FilePath          -- ^ the normalized OpenAPI document (JSON)
  , gcOutDir :: FilePath         -- ^ the package root: the .cabal file and gen/
  , gcPackage :: Text
  , gcVersion :: Text
  , gcSynopsis :: Text
  , gcApp :: Text                -- ^ the webapi app type
  , gcQualifier :: Text          -- ^ every operation's FQN qualifier
  , gcModulePrefix :: Text       -- ^ e.g. Zenbridge.Connector.ZohoBooks
  , gcOpsName :: Text            -- ^ the registry's exported composition
  , gcSumEnums :: Bool
  , gcNamingMap :: Maybe FilePath
  , gcPathPrefix :: Text
  , gcFormat :: Bool             -- ^ run the formatter over every module
  , gcGhcOptions :: [Text]       -- ^ for every generated component
  , gcCabalExtra :: Text         -- ^ the package's hand-written stanzas, appended verbatim
  , gcConnectionParams :: [Text] -- ^ query parameters the connection sets, not the operation
  }

instance FromJSON GenConfig where
  parseJSON = withObject "GenConfig" $ \o -> do
    gcInput <- o .: "input"
    gcOutDir <- o .: "outDir"
    gcPackage <- o .: "package"
    gcVersion <- o .:? "version" .!= "0.1.0.0"
    gcSynopsis <- o .:? "synopsis" .!= "Generated webapi contract and dhall-do registry"
    gcApp <- o .: "app"
    gcQualifier <- o .: "qualifier"
    gcModulePrefix <- o .: "modulePrefix"
    gcOpsName <- o .:? "opsName" .!= (gcQualifier <> "Ops")
    gcSumEnums <- (== ("sum" :: Text)) <$> o .:? "enumMode" .!= "text"
    gcNamingMap <- o .:? "namingMap"
    gcPathPrefix <- o .:? "pathPrefix" .!= "/"
    gcFormat <- o .:? "format" .!= True
    gcGhcOptions <- o .:? "ghcOptions" .!= ["-O0"]
    gcCabalExtra <- o .:? "cabalExtra" .!= ""
    gcConnectionParams <- o .:? "connectionParams" .!= []
    pure GenConfig {..}

-- | The @x-zb@ object: modules, and the module of every component schema
-- and path. A document without one is one module, @api@.
data XZb = XZb
  { xModules :: [Text]
  , xSchema :: HM.HashMap Text Text
  , xPath :: HM.HashMap Text Text
  }

commonM :: Text
commonM = "common"

readXZb :: Value -> XZb
readXZb = \case
  Object o | Just (Object x) <- KM.lookup "x-zb" o ->
    let strs = \case
          Array a -> [t | String t <- foldr (:) [] a]
          _ -> []
        textMap = \case
          Object m -> HM.fromList [(K.toText k, t) | (k, String t) <- KM.toList m]
          _ -> HM.empty
        comps = case KM.lookup "components" x of
          Just (Object c) -> maybe HM.empty textMap (KM.lookup "schemas" c)
          _ -> HM.empty
    in XZb (maybe [] strs (KM.lookup "modules" x)) comps (maybe HM.empty textMap (KM.lookup "paths" x))
  _ -> XZb [commonM, "api"] HM.empty HM.empty

-- | Names no generated type may take: what the generated modules import,
-- and webapi's and dhall-do's vocabulary.
reservedTypeNames :: [Text]
reservedTypeNames =
  [ "Bool", "Char", "Double", "Either", "Float", "IO", "Int", "Integer", "Maybe", "Ordering", "String", "Word", "Rational"
  , "True", "False", "Just", "Nothing", "Left", "Right", "LT", "EQ", "GT"
  , "Eq", "Ord", "Show", "Read", "Enum", "Bounded", "Num", "Real", "Integral", "Fractional", "Floating", "RealFrac", "RealFloat"
  , "Functor", "Applicative", "Monad", "MonadFail", "Foldable", "Traversable", "Semigroup", "Monoid"
  , "Int8", "Int16", "Int32", "Int64", "Word8", "Word16", "Word32", "Word64", "Natural"
  , "Text", "Vector", "Value", "Generic", "Opaque", "Object", "Array", "Key", "Parser", "Result", "Series", "Encoding"
  , "FromJSON", "ToJSON", "Proxy", "Type", "Symbol", "Void", "Map", "Set", "UUID", "UTCTime", "ByteString", "Day", "Scientific"
  , "WebApi", "ApiContract", "Route", "Request", "Response", "Content", "JSON", "PlainText", "HTML", "OctetStream"
  , "MultipartFormData", "UrlEncoded", "Static", "OpId", "ApiError", "OtherError", "Resource", "Cookie", "Param"
  , "GET", "POST", "PUT", "DELETE", "PATCH", "HEAD", "OPTIONS", "TRACE", "CONNECT"
  , "QueryParam", "FormParam", "FileParam", "PathParam", "HeaderIn", "HeaderOut", "CookieIn", "CookieOut"
  , "ApiOut", "ApiErr", "RequestBody", "ContentTypes", "OperationId", "Apis", "Version"
  , "ToParam", "FromParam", "ToHeader", "FromHeader", "EncodeParam", "DecodeParam", "ParamK"
  , "HsType", "ToHsVal", "FromHsVal", "OverrideType", "HsSelect", "ErrorText", "TextIso", "ViaText"
  , "ConcreteActions", "ConcreteBinding", "ConcreteOp", "FQN", "Untyped"
  ]

-- | Every component schema's Haskell name, resolved once: its PascalCase
-- name, else the name under its module's, else a numbered one.
resolveNames :: [Text] -> [(Text, Text)] -> HM.HashMap Text Text
resolveNames reserved = go (S.fromList reserved) HM.empty
  where
    go _ acc [] = acc
    go taken acc ((c, m) : rest) =
      let base = pascalName c
          cands = [base, pascalName m <> base] ++ [base <> T.pack (show i) | i <- [2 :: Int ..]]
          n = head (filter (`S.notMember` taken) cands)
      in go (S.insert n taken) (HM.insert c n acc) rest

-- | The components a schema names, without following them.
schemaRefs :: Referenced Schema -> [Text]
schemaRefs = \case
  Ref (Reference r) -> [r]
  Inline s ->
    concatMap schemaRefs (HMO.elems (_schemaProperties s))
      ++ items (_schemaItems s)
      ++ concatMap schemaRefs (concat (mapMaybe id [_schemaOneOf s, _schemaAllOf s, _schemaAnyOf s]))
      ++ maybe [] schemaRefs (_schemaNot s)
      ++ case _schemaAdditionalProperties s of
           Just (AdditionalPropertiesSchema r) -> schemaRefs r
           _ -> []
  where
    items = \case
      Just (OpenApiItemsObject r) -> schemaRefs r
      Just (OpenApiItemsArray rs) -> concatMap schemaRefs rs
      Nothing -> []

-- | Where a generated declaration lives.
data Home = HModel Text | HRoutes Text | HContract Text
  deriving (Eq, Ord, Show)

generateModular :: GenConfig -> IO ()
generateModular cfg@GenConfig {..} = do
  raw <- either (fail . ((gcInput <> ": ") <>)) pure . eitherDecode =<< BL.readFile gcInput
  oApi <- readOpenAPI gcInput
  namingMap <- maybe (pure HM.empty) (\fp -> either (fail . ((fp <> ": ") <>)) pure . eitherDecode =<< BL.readFile fp) gcNamingMap
  let xzb = readXZb raw
      comps = _openApiComponents oApi
      compSchemas = _componentsSchemas comps
      schemaList = HMO.toList compSchemas
      modOfComp c = HM.lookupDefault (if HM.null (xSchema xzb) then "api" else commonM) c (xSchema xzb)
      modOfPath p = HM.lookupDefault (if HM.null (xPath xzb) then "api" else commonM) p (xPath xzb)
      reserved = gcApp : reservedTypeNames
      names = resolveNames reserved [(c, modOfComp c) | (c, _) <- schemaList]
      st0 = (initState gcSumEnums reserved)
              { modular = True
              , compNames = names
              , inlinePrefix = "Obj"
              , seenVars = S.fromList (reserved ++ HM.elems names)
              , connectionParams = S.fromList gcConnectionParams
              }

      -- the models pass: every component, in its module
      (modelOuts, st1) = flip runState st0 $ forM schemaList $ \(c, sch) -> do
        let m = modOfComp c
        modify (\st -> st {curModule = m})
        (cts, insts) <- createModelData (_schemaType sch) compSchemas (c, sch)
        pure (m, cts, insts)

      -- the contract pass: every path, in its module
      paths = [ (p, item) | (p, item) <- HMO.toList (_openApiPaths oApi), gcPathPrefix `T.isPrefixOf` T.pack p ]
      (pathOuts, st2) = flip runState st1 $ forM paths $ \(p, item) -> do
        let m = modOfPath (T.pack p)
        modify (\st -> st {curModule = m})
        (ri, rcts, ccts, insts) <-
          createTypeSynData namingMap gcApp compSchemas (_componentsParameters comps)
            (_componentsRequestBodies comps) (_componentsResponses comps) (_componentsHeaders comps) (p, item)
        pure (m, ri, rcts, ccts, insts)

  -- op names are identities (the FQN and the type-level OperationId)
  let finalOps = [ (omKey om, finalOpName synName methName om)
                 | (_, ri, _, _, _) <- pathOuts, (synName, ms) <- ri, (methName, (_,_,_,_,_,_,_,om)) <- ms ]
      dups = [ (n, ks) | (n, ks) <- HM.toList (HM.fromListWith (++) [ (n, [k]) | (k, n) <- finalOps ]), length ks > 1 ]
      unmatched = filter (`notElem` fmap fst finalOps) (HM.keys namingMap)
  unless (null dups) $ fail ("op names collide after resolution (curate the naming map): " <> show dups)
  unless (null unmatched) $ hPutStrLn stderr ("[openapi] naming-map keys matching no operation: " <> show unmatched)

  -- where every declaration and instance goes
  let enums = enumTypes st2
      homeFor def ct = if ctName ct `S.member` enums then HModel commonM else def
      placed =
        [ (homeFor (HModel m) ct, ct) | (m, cts, _) <- modelOuts, ct <- cts ]
          ++ [ (homeFor (HRoutes m) ct, ct) | (m, _, rcts, _, _) <- pathOuts, ct <- rcts ]
          ++ [ (homeFor (HContract m) ct, ct) | (m, _, _, ccts, _) <- pathOuts, ct <- ccts ]
      firstOf = HM.fromListWith (\_ old -> old) [ (ctName ct, h) | (h, ct) <- placed ]
      decls = [ (h, ct) | (h, ct) <- dedupe placed ]
      dedupe = go S.empty
        where go _ [] = []
              go seen ((h, ct) : rest)
                | ctName ct `S.member` seen = go seen rest
                | otherwise = (h, ct) : go (S.insert (ctName ct) seen) rest
      insts = concat [ i | (_, _, i) <- modelOuts ] ++ concat [ i | (_, _, _, _, i) <- pathOuts ]
      instHome i = fromMaybe (error ("no declaration for an instance of " <> T.unpack (instFor i))) (HM.lookup (instFor i) firstOf)
      declsAt h = [ ct | (h', ct) <- decls, h' == h ]
      instsAt h = [ i | i <- insts, instHome i == h ]
      routeInfoOf m = concat [ ri | (m', ri, _, _, _) <- pathOuts, m' == m ]

      modules = nub (commonM : xModules xzb ++ map fst3 modelOuts ++ [ m | (m, _, _, _, _) <- pathOuts ])
      fst3 (a, _, _) = a
      modelModules = [ m | m <- modules, m == commonM || not (null (declsAt (HModel m))) ]
      opModules = [ m | m <- modules, not (null (routeInfoOf m)) ]

      -- a model module needs the modules its components point into
      modelDeps m =
        sort . nub $
          [ commonM | m /= commonM ]
            ++ [ d | (c, sch) <- schemaList, modOfComp c == m, r <- schemaRefs (Inline sch)
                   , let d = modOfComp r, d /= m, d `elem` modelModules, HM.member r names ]

  forM_ modelModules $ \m ->
    when (m == commonM && any (/= commonM) (modelDeps m)) $
      fail ("the common module points into " <> show (modelDeps m) <> ": a shared component may only point at shared components")

  let dataTypes = S.fromList [ ctName ct | (_, ct) <- decls, ctKind ct == DataDecl ]
      -- what a synonym names: its right-hand side, as source
      synRhs = HM.fromList [ (ctName ct, rhsOf (renderDecl (ctDecl ct))) | (_, ct) <- decls, ctKind ct == SynDecl ]
      rhsOf d = stripParens (T.unwords (T.words (T.drop 1 (T.dropWhile (/= '=') d))))
      stripParens t
        | "(" `T.isPrefixOf` t && ")" `T.isSuffixOf` t = stripParens (T.strip (T.drop 1 (T.dropEnd 1 t)))
        | otherwise = t
      -- a body or result named through a synonym still needs its record's
      -- instances; an array body gets a whole-value override (dhall-do-api
      -- has no OverrideType for Vector yet — ENG-4) when its element is ours
      throughSyn ns = S.union ns (S.fromList [ r | n <- S.toList ns, Just r <- [HM.lookup n synRhs], r `S.member` dataTypes ])
      requestSide = throughSyn (S.union (paramRecords st2) (bodyTypes st2))
      resultSide = throughSyn (resultTypes st2)
      vectorBodies = nub
        [ (r, e) | n <- S.toList (S.union (paramRecords st2) (bodyTypes st2)), Just r <- [HM.lookup n synRhs]
                 , Just e <- [stripParens <$> T.stripPrefix "Vector " r], e `S.member` dataTypes ]
      vectorOverrides h =
        concat [ [ "instance OverrideType (" <> r <> ") where"
                 , "  overrideType = optionalOverrideType @(" <> r <> ")"
                 , "  overrideDefault = optionalOverrideDefault @(" <> r <> ")"
                 , "" ]
               | (r, e) <- vectorBodies, HM.lookup e firstOf == Just h ]
      sumLike = S.union enums (S.fromList (HM.keys (createdSums st2)))
      errorTypes = S.fromList
        [ t | (_, ri, _, _, _) <- pathOuts, (_, ms) <- ri, (_, (_h,_q,_c,_b,_o,Just errT,_ho,_om)) <- ms
            , let t = renderHsType errT, t `notElem` ["()", "Text"] ]
      instanceLines n =
        [ "instance HsType " <> n, "instance ToHsVal " <> n, "instance FromHsVal " <> n ]
          ++ [ "instance OverrideType " <> n | n `S.member` requestSide, not (n `S.member` sumLike) ]
          ++ [ "instance HsSelect " <> n | n `S.member` resultSide, not (n `S.member` sumLike) ]
          ++ [ "instance ErrorText " <> n | n `S.member` errorTypes ]
          ++ [ "" ]
      bridgeFor h = concatMap instanceLines [ ctName ct | ct <- declsAt h, ctName ct `S.member` dataTypes ] ++ vectorOverrides h

      p = gcModulePrefix
      modelMod m = p <> ".Model." <> pascalName m
      routesMod m = p <> ".Routes." <> pascalName m
      contractMod m = p <> ".Contract." <> pascalName m
      instancesMod m = p <> ".Registry.Instances." <> pascalName m
      opsMod m = p <> ".Registry.Ops." <> pascalName m
      opsFn m = lowerFirstChar (pascalName m) <> "Ops"
      appMod = p <> ".App"
      contractTop = p <> ".Contract"
      registryTop = p <> ".Registry"
      supportMod = p <> ".Registry.Support"
      genDir = gcOutDir </> "gen"
      srcFile dir modName = genDir </> dir </> T.unpack (T.replace "." "/" modName) <> ".hs"
      write = writeGenerated cfg

  removePathForcibly genDir

  -- models
  forM_ modelModules $ \m -> do
    let h = HModel m
        body = map (renderDecl . ctDecl) (declsAt h) ++ map (renderDecl . instDecl) (instsAt h)
        extra = if m == commonM then [opaqueDecl] else []
    write (srcFile ("model-" <> T.unpack m) (modelMod m)) $
      T.unlines $
        [ "{-# LANGUAGE DataKinds #-}"
        , "{-# LANGUAGE DeriveGeneric #-}"
        , "{-# LANGUAGE DuplicateRecordFields #-}"
        , "{-# LANGUAGE KindSignatures #-}"
        , "{-# LANGUAGE NoFieldSelectors #-}"
        , "{-# LANGUAGE OverloadedStrings #-}"
        , "{-# LANGUAGE TypeOperators #-}"
        , "{-# OPTIONS_GHC -Wno-unused-imports #-}"
        , ""
        , "-- Generated by openapi-model-generator: the " <> m <> " module's types. Do not edit."
        , "module " <> modelMod m <> " where"
        , ""
        ]
          ++ modelImports
          ++ [ "import " <> modelMod d | d <- modelDeps m ]
          ++ [ "" ]
          ++ extra
          ++ body

  -- the app type
  write (srcFile "contract" appMod) $ T.unlines
    [ "-- Generated by openapi-model-generator. Do not edit."
    , "module " <> appMod <> " (" <> gcApp <> ") where"
    , ""
    , "-- | " <> gcApp <> ", as a webapi application."
    , "data " <> gcApp
    ]

  -- routes
  forM_ opModules $ \m ->
    write (srcFile "contract" (routesMod m)) $ T.unlines $
      [ "{-# LANGUAGE DataKinds #-}"
      , "{-# LANGUAGE TypeOperators #-}"
      , "{-# OPTIONS_GHC -Wno-unused-imports #-}"
      , ""
      , "-- Generated by openapi-model-generator: the " <> m <> " module's routes. Do not edit."
      , "module " <> routesMod m <> " where"
      , ""
      , "import Data.Int (Int32, Int64)"
      , "import Data.Text (Text)"
      , "import WebApi.Contract ((://), (:/))"
      , "import " <> appMod
      ]
        ++ [ "import " <> modelMod d | d <- modelModules ]
        ++ [ "" ]
        ++ map (renderDecl . ctDecl) (declsAt (HRoutes m))

  -- the Apis list
  let apisList = listPromotedTy
        [ var "Route" @@ listPromotedTy (var . textToRdrNameStr <$> meths) @@ var (textToRdrNameStr syn)
        | m <- opModules, (syn, ms) <- routeInfoOf m, let meths = map fst ms ]
      webApiInst = instance' (var "WebApi" @@ var (textToRdrNameStr gcApp)) [tyFamInst "Apis" [var (textToRdrNameStr gcApp)] apisList]
  write (srcFile "contract" contractTop) $ T.unlines $
    [ "{-# LANGUAGE DataKinds #-}"
    , "{-# LANGUAGE TypeFamilies #-}"
    , "{-# LANGUAGE TypeOperators #-}"
    , "{-# OPTIONS_GHC -Wno-orphans #-}"
    , ""
    , "-- Generated by openapi-model-generator: every route " <> gcApp <> " serves. Do not edit."
    , "module " <> contractTop <> " (" <> gcApp <> ") where"
    , ""
    , "import WebApi.Contract"
    , "import " <> appMod
    ]
      ++ [ "import " <> routesMod m | m <- opModules ]
      ++ [ "", renderDecl webApiInst ]

  -- contracts
  forM_ opModules $ \m -> do
    let h = HContract m
        contractInsts = concatMap (mkApiContractInstances gcApp) (routeInfoOf m)
    write (srcFile "contract" (contractMod m)) $ T.unlines $
      [ "{-# LANGUAGE DataKinds #-}"
      , "{-# LANGUAGE DeriveGeneric #-}"
      , "{-# LANGUAGE DuplicateRecordFields #-}"
      , "{-# LANGUAGE FlexibleInstances #-}"
      , "{-# LANGUAGE MultiParamTypeClasses #-}"
      , "{-# LANGUAGE NoFieldSelectors #-}"
      , "{-# LANGUAGE OverloadedStrings #-}"
      , "{-# LANGUAGE TypeFamilies #-}"
      , "{-# LANGUAGE TypeOperators #-}"
      , "{-# LANGUAGE TypeSynonymInstances #-}"
      , "{-# OPTIONS_GHC -Wno-orphans -Wno-unused-imports #-}"
      , ""
      , "-- Generated by openapi-model-generator: the " <> m <> " module's contract. Do not edit."
      , "module " <> contractMod m <> " where"
      , ""
      ]
        ++ modelImports
        ++ [ "import Data.CaseInsensitive (mk)", "import WebApi.Contract", "import WebApi.Param", "import " <> appMod, "import " <> contractTop <> " ()", "import " <> routesMod m ]
        ++ [ "import " <> modelMod d | d <- modelModules ]
        ++ [ "" ]
        ++ map (renderDecl . ctDecl) (declsAt h)
        ++ map (renderDecl . instDecl) (instsAt h)
        ++ map renderDecl contractInsts

  -- registry: support
  write (srcFile "registry" supportMod) $ T.unlines
    [ "{-# LANGUAGE OverloadedStrings #-}"
    , ""
    , "-- Generated by openapi-model-generator. Do not edit."
    , "module " <> supportMod <> " (opIdOf, mkFqn) where"
    , ""
    , "import Data.List.NonEmpty (NonEmpty (..))"
    , "import Data.Maybe (fromMaybe)"
    , "import Data.Text (Text)"
    , "import qualified Data.Text as T"
    , "import qualified Data.UUID.Types as UUID"
    , "import Dhall.Do.Api.Id (FQN (..), OperationId, mkOperationId)"
    , "import GHC.Stack (HasCallStack)"
    , ""
    , "opIdOf :: HasCallStack => Text -> OperationId"
    , "opIdOf t = mkOperationId (fromMaybe (error (\"bad uuid literal: \" <> T.unpack t)) (UUID.fromText t))"
    , ""
    , "-- | Every operation is " <> gcQualifier <> "/<name>."
    , "mkFqn :: Text -> FQN"
    , "mkFqn n = FQN {qualifier = \"" <> gcQualifier <> "\" :| [], name = n}"
    ]

  -- registry: bridge instances per model module
  forM_ modelModules $ \m ->
    write (srcFile "registry" (instancesMod m)) $ T.unlines $
      [ "{-# LANGUAGE FlexibleInstances #-}"
      , "{-# LANGUAGE TypeApplications #-}"
      , "{-# OPTIONS_GHC -Wno-orphans -Wno-unused-imports #-}"
      , ""
      , "-- Generated by openapi-model-generator: dhall-do's bridge for the " <> m <> " module's types. Do not edit."
      , "module " <> instancesMod m <> " () where"
      , ""
      , "import Data.Vector (Vector)"
      , "import Dhall.Do.Api.Bridge"
      , "import Dhall.Do.Api.WebApi.Concrete.Binding (ErrorText)"
      , "import " <> modelMod m
      ]
        ++ [ "import " <> instancesMod d <> " ()" | d <- modelDeps m ]
        ++ (if m == commonM then opaqueBridgeImports else [])
        ++ [ "" ]
        ++ (if m == commonM then opaqueBridge else [])
        ++ bridgeFor (HModel m)

  -- registry: operations per module
  forM_ opModules $ \m -> do
    let ops = [ (syn, meth, maybe "()" renderHsType outInfo, om)
              | (syn, ms) <- routeInfoOf m, (meth, (_h,_q,_c,_b,outInfo,_e,_ho,om)) <- ms ]
        regLines = [ "    " <> (if i == 0 then "  " else ". ") <> registrationExpr gcApp op' | (i, op') <- zip [0 :: Int ..] ops ]
    write (srcFile "registry" (opsMod m)) $ T.unlines $
      [ "{-# LANGUAGE DataKinds #-}"
      , "{-# LANGUAGE DisambiguateRecordFields #-}"
      , "{-# LANGUAGE FlexibleInstances #-}"
      , "{-# LANGUAGE DuplicateRecordFields #-}"
      , "{-# LANGUAGE OverloadedStrings #-}"
      , "{-# LANGUAGE ScopedTypeVariables #-}"
      , "{-# LANGUAGE TypeApplications #-}"
      , "{-# LANGUAGE TypeOperators #-}"
      , "{-# OPTIONS_GHC -Wno-orphans -Wno-unused-imports #-}"
      , ""
      , "-- Generated by openapi-model-generator: the " <> m <> " module's operations. Do not edit."
      , "module " <> opsMod m <> " (" <> opsFn m <> ") where"
      , ""
      , "import Data.Int (Int32, Int64)"
      , "import Data.Text (Text)"
      , "import Data.Vector (Vector)"
      , "import Dhall.Do.Api.Bridge"
      , "import Dhall.Do.Api.WebApi.Concrete.Binding"
      , "import WebApi.Client.Session (AppIsElem, getSuccessOut)"
      , "import WebApi.Contract hiding (OperationId)"
      , "import " <> appMod
      , "import " <> contractTop <> " ()"
      , "import " <> routesMod m
      , "import " <> contractMod m
      , "import " <> supportMod
      ]
        ++ [ "import " <> modelMod d | d <- modelModules ]
        ++ [ "import " <> instancesMod d <> " ()" | d <- modelModules ]
        ++ [ "" ]
        ++ bridgeFor (HContract m)
        ++ bridgeFor (HRoutes m)
        ++ [ opsFn m <> " :: forall apps. AppIsElem " <> gcApp <> " apps => ConcreteActions apps -> ConcreteActions apps"
           , opsFn m <> " =" ]
        ++ regLines

  -- registry: the whole
  write (srcFile "registry" registryTop) $ T.unlines $
    [ "{-# LANGUAGE ScopedTypeVariables #-}"
    , ""
    , "-- Generated by openapi-model-generator: every " <> gcApp <> " operation. Do not edit."
    , "module " <> registryTop <> " (" <> gcOpsName <> ") where"
    , ""
    , "import Dhall.Do.Api.WebApi.Concrete.Binding (ConcreteActions)"
    , "import WebApi.Client.Session (AppIsElem)"
    , "import " <> appMod
    ]
      ++ [ "import " <> opsMod m <> " (" <> opsFn m <> ")" | m <- opModules ]
      ++ [ ""
         , "-- | Every operation " <> gcApp <> " serves, added to a registry."
         , gcOpsName <> " :: forall apps. AppIsElem " <> gcApp <> " apps => ConcreteActions apps -> ConcreteActions apps"
         , gcOpsName <> " = " <> (if null opModules then "id" else T.intercalate " . " (map opsFn opModules))
         ]

  -- the package
  let modelLibs = [ "model-" <> m | m <- modelModules ]
      pkg = gcPackage
      stanza name vis dir mods deps =
        [ name
        , "    import:           generated" ]
          ++ [ "    visibility:       public" | vis ]
          ++ [ "    hs-source-dirs:   gen/" <> dir
             , "    exposed-modules:" ]
          ++ [ "        " <> md | md <- mods ]
          ++ [ "    build-depends:" ]
          ++ [ "        " <> (if i == 0 then "  " else ", ") <> d | (i, d) <- zip [0 :: Int ..] deps ]
          ++ [ "" ]
      cabal = T.unlines $
        [ "cabal-version:      3.0"
        , "-- Generated by openapi-model-generator (tools/zbc gen). Do not edit."
        , "name:               " <> pkg
        , "version:            " <> gcVersion
        , "synopsis:           " <> gcSynopsis
        , "build-type:         Simple"
        , ""
        , "-- Generated code is compiled " <> T.unwords gcGhcOptions <> " whatever the consuming"
        , "-- project's optimization: it is boilerplate over thousands of types."
        , "common generated"
        , "    default-language: Haskell2010"
        , "    ghc-options:      " <> T.unwords gcGhcOptions
        , ""
        ]
          ++ concat
            [ stanza ("library model-" <> m) True ("model-" <> m) [modelMod m]
                (["base", "text", "vector", "aeson"] ++ [ pkg <> ":model-" <> d | d <- modelDeps m ])
            | m <- modelModules ]
          ++ stanza "library" False "contract"
               ([appMod, contractTop] ++ map routesMod opModules ++ map contractMod opModules)
               (["base", "text", "vector", "aeson", "case-insensitive", "webapi-contract"] ++ [ pkg <> ":" <> l | l <- modelLibs ])
          ++ stanza "library registry" True "registry"
               ([registryTop, supportMod] ++ map instancesMod modelModules ++ map opsMod opModules)
               (["base", "text", "vector", "aeson", "bytestring", "uuid-types", "webapi-contract", "webapi-session", "dhall", "dhall-do-api", "dhall-do-api-webapi", pkg]
                  ++ [ pkg <> ":" <> l | l <- modelLibs ])
  T.writeFile (gcOutDir </> T.unpack pkg <> ".cabal") $
    if T.null (T.strip gcCabalExtra)
      then cabal
      else cabal <> "-- The package's hand-written stanzas (zbc.yaml: cabal).\n" <> gcCabalExtra

  -- every operation as data: what tests, the ledger and the docs read
  let rawOp opPath meth = case raw of
        Object o
          | Just (Object ps) <- KM.lookup "paths" o
          , Just (Object item) <- KM.lookup (K.fromText opPath) ps ->
              (KM.lookup (K.fromText (T.toLower meth)) item, KM.lookup "x-mcp-group" item)
        _ -> (Nothing, Nothing)
      field k = \case
        Just (Object o) -> KM.lookup k o
        _ -> Nothing
      scopesOf = \case
        Just (Array reqs) -> nub [ sc | Object req <- foldr (:) [] reqs, (_, Array scs) <- KM.toList req, String sc <- foldr (:) [] scs ]
        _ -> []
      strs = \case
        Just (Array a) -> [ t | String t <- foldr (:) [] a ]
        _ -> []
      opRecords =
        [ object
            [ "name" .= finalOpName syn meth om
            , "id" .= registryOpUuid gcApp syn meth om
            , "operationId" .= maybe Null id (field "operationId" rawOperation)
            , "method" .= meth
            , "path" .= T.drop (T.length meth + 1) (omKey om)
            , "module" .= m
            , "route" .= syn
            , "pathParam" .= fmap renderHsType (omPathParam om)
            , "connection" .= omConnParams om
            , "query" .= fmap renderHsType q
            , "header" .= fmap renderHsType h
            , "body" .= fmap (unList . renderHsType) b
            , "result" .= fmap renderHsType o
            , "error" .= fmap renderHsType e
            , "summary" .= omSummary om
            , "scopes" .= scopesOf (field "security" rawOperation)
            , "tags" .= strs (field "tags" rawOperation)
            , "group" .= strs grp
            ]
        | m <- opModules, (syn, ms) <- routeInfoOf m, (meth, (h,q,_c,b,o,e,_ho,om)) <- ms
        , let (rawOperation, grp) = rawOp (T.drop (T.length meth + 1) (omKey om)) meth ]
  BL.writeFile (genDir </> "operations.json") $
    "[\n" <> BL.intercalate ",\n" (map encode opRecords) <> "\n]\n"

  -- what the generator could not type, for review beside the code
  let ws = sort (nub (warnings st2))
  T.writeFile (genDir </> "warnings.txt") (T.unlines ws)
  hPutStrLn stderr $
    "[openapi] " <> show (length modelModules) <> " model modules, " <> show (length opModules) <> " operation modules, "
      <> show (length finalOps) <> " operations, " <> show (S.size dataTypes) <> " data types, "
      <> show (length ws) <> " warnings (gen/warnings.txt)"
  where
    modelImports =
      [ "import Control.Applicative ((<|>))"
      , "import Data.Aeson (FromJSON (..), ToJSON (..), Value (..), object, withObject, withText, (.:), (.:?), (.=))"
      , "import Data.Int (Int32, Int64)"
      , "import Data.Maybe (catMaybes)"
      , "import Data.Text (Text)"
      , "import Data.Vector (Vector)"
      , "import GHC.Generics (Generic)"
      ]
    opaqueDecl = T.unlines
      [ "-- | A value the spec leaves untyped, kept as the JSON it arrived as."
      , "newtype Opaque = Opaque Value"
      , "  deriving (Show, Eq, Generic)"
      , ""
      , "instance FromJSON Opaque where"
      , "  parseJSON = pure . Opaque"
      , ""
      , "instance ToJSON Opaque where"
      , "  toJSON (Opaque v) = v"
      ]
    opaqueBridgeImports =
      [ "import Data.Aeson (eitherDecodeStrict)"
      , "import Data.Aeson.Text (encodeToLazyText)"
      , "import qualified Data.Text as T"
      , "import qualified Data.Text.Encoding as TE"
      , "import qualified Data.Text.Lazy as TL"
      ]
    -- Opaque crosses to Dhall as its JSON text
    opaqueBridge =
      [ "instance TextIso Opaque where"
      , "  toTextIso (Opaque v) = TL.toStrict (encodeToLazyText v)"
      , "  fromTextIso t = either (Left . T.pack) (Right . Opaque) (eitherDecodeStrict (TE.encodeUtf8 t))"
      , ""
      , "instance HsType Opaque where"
      , "  hsType = hsType @(ViaText Opaque)"
      , "  hsNamedTypes = []"
      , ""
      , "instance FromHsVal Opaque where"
      , "  fromHsLit o = fromHsLit (ViaText o)"
      , "  fromHsVal = embedScalar"
      , ""
      , "instance ToHsVal Opaque where"
      , "  toHsLit e = (\\(ViaText o) -> o) <$> toHsLit e"
      , ""
      , "instance HsSelect Opaque where"
      , "  selectField = noSelect"
      , ""
      , "instance OverrideType Opaque where"
      , "  overrideType = optionalOverrideType @Opaque"
      , "  overrideDefault = optionalOverrideDefault @Opaque"
      , ""
      ]

-- | A request body's type-level list, @'[T]@, as @T@.
unList :: Text -> Text
unList t = fromMaybe t (T.stripPrefix "'[" t >>= T.stripSuffix "]")

-- | Write one generated module, formatted when the config asks.
writeGenerated :: GenConfig -> FilePath -> Text -> IO ()
writeGenerated GenConfig {gcFormat} fp contents = do
  txt <-
    if gcFormat
      then ormolu defaultConfig {cfgCheckIdempotence = False} fp contents
             `catch` \(e :: SomeException) -> do
               hPutStrLn stderr ("[openapi] formatter failed for " <> fp <> " (writing unformatted): " <> takeWhile (/= '\n') (show e))
               pure contents
      else pure contents
  createDirectoryIfMissing True (takeDirectory fp)
  T.writeFile fp txt
