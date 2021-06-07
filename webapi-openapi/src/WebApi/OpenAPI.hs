{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}

module WebApi.OpenAPI where

import Data.ByteString.Lazy as B (readFile)
import Data.Aeson ( decode )
import Data.OpenApi
    ( Components(_componentsSchemas),
      OpenApi(_openApiComponents),
      OpenApiItems(OpenApiItemsArray, OpenApiItemsObject),
      OpenApiType(..),
      Reference(Reference),
      Referenced(..),
      Schema(_schemaType, _schemaFormat, _schemaItems, _schemaRequired,
             _schemaProperties) )
import GHC.SourceGen
    ( data',
      field,
      newtype',
      prefixCon,
      recordCon,
      import',
      module',
      occNameToStr,
      tuple,
      putPpr,
      kindedVar,
      listTy,
      stringTy,
      OccNameStr,
      RdrNameStr,
      App((@@), op),
      BVar(bvar),
      Var(var),
      HsDecl',
      HsType',
      exposing )
import GHC.Paths (libdir)
import GHC ( runGhc )
import Data.HashMap.Strict.InsOrd(toList)
import Data.Text as T ( unpack, Text, append, splitAt, toUpper, take )
import GhcPlugins ( mkVarOcc )
import Control.Monad.State.Class ( MonadState )
import Control.Monad.State.Lazy(evalState)
import Data.Set (Set, empty)
import Data.String ( IsString(fromString) )
import Data.Bifunctor ( Bifunctor(bimap) )

data ModelGenState =
    ModelGenState { seenVars :: Set Text
                  , imports :: Set Text
                  }

generateModels ::
    FilePath -> FilePath -> IO ()
generateModels fp _destFp = do
    oApi <- readOpenAPI fp
    let compSchemas = toList . _componentsSchemas . _openApiComponents $ oApi
        modelList = evalState
                        (mapM createModelData compSchemas)
                        (ModelGenState empty empty)
        hsModule = module' (Just "OpenApiModels") Nothing imps (namedTy:modelList)
    runGhc (Just libdir) $ putPpr hsModule

    where imps = [ import' "WebApi"
                 , import' "Data.Int"
                 , exposing (import' "GHC.Types") [var "Symbol"]
                 , exposing (import' "Data.Text") [var "Text"]
                 ]
          namedTy = newtype' ":::" [kindedVar "fld" (var "Symbol") , bvar "a"]
                        (prefixCon "Field" [field $ var "a"]) []


createModelData ::
    (MonadState ModelGenState m) =>
    (Text,Schema) -> m HsDecl'
createModelData (dName,dSchema) = do
    let reqParams =  _schemaRequired dSchema
    rFields <- mapM (\(x,y) -> parseRecordFields (x,y) (x `elem` reqParams)) (toList . _schemaProperties $ dSchema)
    let frFields = bimap textToOccNameStr field <$> rFields
    return (data' (textToOccNameStr dName) [] [recordCon (textToOccNameStr dName) frFields] [])



parseRecordFields ::
    (MonadState ModelGenState m) =>
    (Text, Referenced Schema) -> Bool -> m (Text,HsType')
parseRecordFields (dName,Ref (Reference x)) isReq =
    return (dName,if isReq
                   then var $ textToRdrNameStr x
                   else var "Maybe" @@ var ( textToRdrNameStr x))
parseRecordFields (dName,Inline dSchema) isReq = parseInlineFields (_schemaType dSchema) dName dSchema isReq

parseInlineFields ::
    (MonadState ModelGenState m) =>
    Maybe OpenApiType -> Text -> Schema -> Bool -> m (Text,HsType' )
parseInlineFields (Just OpenApiString) dName _dSchema isReq =
    return (dName, if isReq then var "Text" else var "Maybe" @@ var "Text")
parseInlineFields (Just OpenApiNumber ) dName _dSchema isReq =
    return (dName, if isReq then var "Double" else var "Maybe" @@ var "Double")
parseInlineFields (Just OpenApiInteger) dName dSchema isReq =
    return (dName, if isReq then var $ textToRdrNameStr parsedInt else var "Maybe" @@ var (textToRdrNameStr parsedInt))
    where parsedInt = parseIntegerFld (_schemaFormat dSchema)
parseInlineFields (Just OpenApiBoolean) dName _dSchema isReq =
    return (dName,if isReq then var "Bool" else var "Maybe" @@ var "Bool")
parseInlineFields (Just OpenApiArray ) dName dSchema _isReq = 
    case _schemaItems dSchema of
        Nothing -> error "No _schemaItems value for Array"
        Just (OpenApiItemsObject sch) -> do
            (dName2,dType) <- parseRecordFields (dName,sch) True
            return (dName2,listTy dType)
        Just (OpenApiItemsArray _) -> error "OpenApiItemsArray Array type"
parseInlineFields (Just OpenApiNull ) _dName _dSchema _isReq =
    error "Null OpenApi Type"

parseInlineFields (Just OpenApiObject ) dName dSchema isReq = do
    let reqParams =  _schemaRequired dSchema
    childInlines <- mapM (\(x,y) -> parseRecordFields (x,y) (x `elem` reqParams)) (toList . _schemaProperties $ dSchema)
    let typeTuple =  tuple $ (\(x,y)-> op (stringTy $ T.unpack x) ":::" y) <$> childInlines
    return (dName,if isReq then typeTuple else var "Maybe" @@ typeTuple)
parseInlineFields Nothing _dName _dSchema _isReq = error "No Type Defined"

parseIntegerFld :: Maybe Text -> Text
parseIntegerFld (Just x) = let y = upperFirstChar x
                            in if T.take 3 y == "Int"
                               then y
                               else error "Invalid Integer Format"
parseIntegerFld Nothing = "Int"


textToOccNameStr :: Text -> OccNameStr
textToOccNameStr = occNameToStr . mkVarOcc . unpack

textToRdrNameStr :: Text -> RdrNameStr
textToRdrNameStr = fromString . T.unpack

upperFirstChar :: Text -> Text
upperFirstChar x = let (fir,res) = T.splitAt 1 x in append (toUpper fir) res

retOApi :: Maybe p -> p
retOApi (Just x) = x
retOApi Nothing = error "Can't decode OpenAPI spec file"

readOpenAPI :: FilePath -> IO OpenApi
readOpenAPI fp = do
    fileContent <- B.readFile fp
    return $ retOApi (decode fileContent :: Maybe OpenApi)

runGenerateModels :: IO ()
runGenerateModels = generateModels "petstore.json" ""