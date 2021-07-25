{-# LANGUAGE RecordWildCards #-}

module Main where

import Options.Applicative
    ( (<**>),
      fullDesc,
      header,
      info,
      long,
      progDesc,
      strOption,
      execParser,
      helper,
      Parser )
-- import WebApi.OpenAPI (generateModels)
import WebApi.OpenAPI.Core

data CliArgs
  = CliArgs
      { inputJsonFP :: FilePath,
        outDirBaseFp :: FilePath,
        reqFilePathPrefix :: FilePath
      }

cliParser :: Parser CliArgs
cliParser =
  CliArgs
    <$> strOption (long "inputJsonFP")
    <*> strOption (long "outDirBaseFp")
    <*> strOption (long "reqFilePathPrefix")

main :: IO ()
main = do
  processOpenAPI "/home/sreenidhi/NSOA3.json"
  {-
  CliArgs {..} <- execParser opts
  generateModels inputJsonFP outDirBaseFp reqFilePathPrefix
  where opts =
          info
            (cliParser <**> helper)
            (fullDesc
                 <> progDesc "Print a greeting for TARGET"
                 <> header "hello - a test for optparse-applicative"
            )
  -}
  
