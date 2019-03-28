{-# LANGUAGE OverloadedStrings #-}
module Main where

import ContractGen
import Options.Applicative
import Data.Semigroup ((<>))
import Data.Text (Text)
import qualified Data.Text as T

data CodegenOpts = CodegenOpts { inPath  :: Text
                               , outPath :: Text
                               , name    :: Maybe Text
                               , prefix  :: Maybe Text
                               , verbose :: Bool
                               -- , isUrl   :: Bool
                               } deriving (Show, Eq)

codegenProg :: ParserInfo CodegenOpts
codegenProg = info codegenOpts (fullDesc <>
                                 progDesc "Haskell code generator for swagger"
                               )

codegenOpts :: Parser CodegenOpts
codegenOpts =
  CodegenOpts <$> strArgument  (  metavar "SWAGGER-FILE-LOCATION"
                                 <> help "Location of swagger file"
                               )
              <*> strArgument  (  metavar "OUTPUT-DIR"
                                 <> help "Directory of generated files"
                               )  
              <*> optional
                    (strOption (  long "name"
                               <> short 'n'
                               <> metavar "NAME"
                               <> help "Name of the generated project"
                               )
                    )
              <*> optional
                    ( strOption (  long "prefix"
                                <> short 'f'
                                <> metavar "PREFIX"
                                <> help "Prefix to name of generated project"
                                )
                    )
              <*> switch (  long "verbose"
                         <> short 'v'
                         <> help "See debug output"
                         )
              {-
              <*> switch (  long "url"
                         <> short "u"
                         <> help "Is input location a url"
                         )
              -}

main :: IO ()
main = do
  opts <- execParser codegenProg
  runCodeGen (T.unpack (inPath opts))
             (T.unpack (op opts))
             (T.unpack (pName opts))

  where op opts = case T.last (outPath opts) of
          '/' -> outPath opts
          _   -> outPath opts <> "/"

        pName opts = case name opts of
          Nothing -> case take 2 (reverse (T.splitOn "/" (inPath opts))) of
            ["", ""] -> error "Panic: invalid input path"
            ["", n]  -> withPrefix (prefix opts) (T.toLower (stripExtension n))
            [n, _]   -> withPrefix (prefix opts) (T.toLower (stripExtension n))
            _        -> error "Panic: impossible case triggered"
          Just n  -> withPrefix (prefix opts) (T.toLower n)

        withPrefix Nothing n  = n
        withPrefix (Just p) n = T.toLower p <> "-" <> n

        stripExtension n = head (T.splitOn "." n)
