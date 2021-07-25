{-# LANGUAGE DuplicateRecordFields         #-}
{-# LANGUAGE NamedFieldPuns                #-}
{-# LANGUAGE FlexibleContexts              #-}

module WebApi.OpenAPI.Core where

import Control.Monad.IO.Class ( MonadIO (..) )
import Data.Aeson
import Data.ByteString.Lazy as B (readFile)
import Data.HashMap.Strict as HM
import WebApi.OpenAPI.Analysis
import WebApi.OpenAPI.Codegen
import qualified Data.Vector as V

processOpenAPI :: FilePath -> {-FilePath -> Maybe FilePath ->-} IO ()
processOpenAPI fp {-destFp pfx-} = do
  bs <- B.readFile fp
  let Just oa = decode bs
      OpenApiAnalysisInfo { _refSchema, _contracts } = runOpenAPIAnalysis oa
  runCodegen oa $ do
    modelDecls <- HM.foldrWithKey go (pure mempty) _refSchema
    writeModels modelDecls

  where
    go k v a = do
      -- liftIO $ putStrLn $ "Processing: " <> show (k, v)
      (V.++) <$> genModel k v <*> a

