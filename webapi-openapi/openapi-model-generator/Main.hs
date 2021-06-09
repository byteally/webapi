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
import WebApi.OpenAPI (generateModels)

data CliArgs
  = CliArgs
      { inputJsonFP :: FilePath,
        outDirBaseFp :: FilePath
      }

cliParser :: Parser CliArgs
cliParser =
  CliArgs
    <$> strOption (long "inputJsonFP")
    <*> strOption (long "outDirBaseFp")

main :: IO ()
main = do
  CliArgs {..} <- execParser opts
  generateModels inputJsonFP outDirBaseFp
  where opts =
          info
            (cliParser <**> helper)
            (fullDesc
                 <> progDesc "Print a greeting for TARGET"
                 <> header "hello - a test for optparse-applicative"
            )

  