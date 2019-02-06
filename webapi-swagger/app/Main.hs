module Main where

import System.Environment
import ContractGen

main :: IO ()
main = do
  args <- getArgs
  case args of
    [jsonp, outp] -> runCodeGen jsonp outp
    _             -> putStrLn "Invalid usage. init-swagger-project <json-path> <output-path>"
