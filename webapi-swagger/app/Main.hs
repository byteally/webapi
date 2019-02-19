module Main where

import System.Environment
import ContractGen

main :: IO ()
main = do
  args <- getArgs
  case args of
    [jsonp, outp, projName] -> runCodeGen jsonp outp projName
    _             -> putStrLn "Invalid usage. init-swagger-project <json-path> <output-path> <project-name>"
