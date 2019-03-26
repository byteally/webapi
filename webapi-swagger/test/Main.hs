{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.Environment
import Network.HTTP.Client
import qualified Data.Text as T
import Data.Typeable
import Control.Exception
import Network.HTTP.Types
import qualified Data.ByteString.Lazy as BSL
import ContractGen
import System.Directory
import Control.Monad
import System.Process



data InvalidURL = InvalidURL T.Text
                deriving (Show, Typeable)

data InvalidArgs = InvalidArgs
                deriving (Show, Typeable)

instance Exception InvalidURL
instance Exception InvalidArgs

main :: IO ()
main = do
  args <- getArgs
  case args of
    [ url ] -> do
      manager <- newManager defaultManagerSettings
      request <- parseRequest url
      response <- httpLbs request manager
      case responseStatus response == status200 of
        True  -> triggerCodegen (responseBody response)
        False -> throwIO (InvalidURL (T.pack url))
    _ -> throwIO InvalidArgs
        

  where triggerCodegen body = do
          let swPath = "/tmp/swagger.json"
              cgPath = "/tmp/out/"
              cbPath = cgPath <> pkgname
              pkgname = "genswagger"
              cabalCommand = "cabal new-build"
          cgPathExists <- doesPathExist cgPath    
          when cgPathExists (removeDirectoryRecursive cgPath)
          createDirectory cgPath
          BSL.writeFile swPath body
          runCodeGen swPath cgPath pkgname
          _ <- createProcess ((shell cabalCommand) { cwd = Just cbPath })
          return ()
          
          
