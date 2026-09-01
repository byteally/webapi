{-# LANGUAGE RecordWildCards #-}

module Main where

import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as BL
import qualified Data.HashMap.Internal as HM
import Options.Applicative
    ( (<**>),
      fullDesc,
      header,
      info,
      long,
      optional,
      progDesc,
      strOption,
      execParser,
      helper,
      Parser )
import System.Exit (die)
import WebApi.OpenAPI (generateModels, NamingMap)

data CliArgs
  = CliArgs
      { inputJsonFP :: FilePath,
        outDirBaseFp :: FilePath,
        reqFilePathPrefix :: FilePath,
        namingMapFP :: Maybe FilePath
      }

cliParser :: Parser CliArgs
cliParser =
  CliArgs
    <$> strOption (long "inputJsonFP")
    <*> strOption (long "outDirBaseFp")
    <*> strOption (long "reqFilePathPrefix")
    <*> optional (strOption (long "namingMapFP"))

main :: IO ()
main = do
  CliArgs {..} <- execParser opts
  namingMap <- maybe (pure HM.empty) loadNamingMap namingMapFP
  generateModels inputJsonFP outDirBaseFp reqFilePathPrefix namingMap
  where opts =
          info
            (cliParser <**> helper)
            (fullDesc
                 <> progDesc "Generate a webapi contract package (+ concrete registry) from an OpenAPI document"
                 <> header "openapi-model-generator"
            )
        loadNamingMap :: FilePath -> IO NamingMap
        loadNamingMap fp =
          either (\e -> die (fp <> ": bad naming map: " <> e)) pure . A.eitherDecode
            =<< BL.readFile fp

  