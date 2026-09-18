{-# LANGUAGE LambdaCase #-}
-- | Golden tests: each case runs the generator over a fixture and compares
-- every file it writes with the committed copy under test/golden/<case>.
--
-- A change to the generator's output is reviewed as a diff of these files:
-- run with GOLDEN_ACCEPT=1 to rewrite them, then read the diff.
module Main (main) where

import Control.Monad (forM, unless, when)
import qualified Data.HashMap.Strict as HM
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as BL
import Data.List (sort)
import System.Directory
import System.Environment (lookupEnv)
import System.Exit (exitFailure)
import System.FilePath ((</>), makeRelative)
import WebApi.OpenAPI (NamingMap, generateModels)

data Case = Case
  { caseName :: String
  , caseInput :: FilePath
  , caseNaming :: Maybe FilePath
  , caseSumEnums :: Bool
  }

cases :: [Case]
cases =
  [ Case "ns-currency" "test/fixtures/ns-currency-slice.json" (Just "test/fixtures/ns-currency-naming.json") False
  ]

main :: IO ()
main = do
  accept <- maybe False (not . null) <$> lookupEnv "GOLDEN_ACCEPT"
  tmp <- (</> "webapi-openapi-golden") <$> getTemporaryDirectory
  failures <- forM cases $ \c -> do
    let out = tmp </> caseName c
    removePathForcibly out
    naming <- maybe (pure HM.empty) loadNaming (caseNaming c)
    generateModels (caseInput c) out "/" naming (caseSumEnums c)
    produced <- listFiles out
    let golden = "test/golden" </> caseName c
    if accept
      then do
        removePathForcibly golden
        mapM_ (\f -> copyInto (out </> f) (golden </> f)) produced
        putStrLn ("accepted " <> caseName c <> " (" <> show (length produced) <> " files)")
        pure False
      else do
        expected <- doesDirectoryExist golden >>= \case
          True -> listFiles golden
          False -> pure []
        let missing = [f | f <- expected, f `notElem` produced]
            extra = [f | f <- produced, f `notElem` expected]
        diffs <- fmap concat . forM [f | f <- produced, f `elem` expected] $ \f -> do
          a <- readFile (golden </> f)
          b <- readFile (out </> f)
          length a `seq` length b `seq` pure [f | a /= b]
        let bad = missing ++ extra ++ diffs
        unless (null bad) $ do
          putStrLn ("FAIL " <> caseName c)
          mapM_ (\f -> putStrLn ("  missing  " <> f)) missing
          mapM_ (\f -> putStrLn ("  new      " <> f)) extra
          mapM_ (\f -> putStrLn ("  differs  " <> f <> "   (diff " <> (golden </> f) <> " " <> (out </> f) <> ")")) diffs
        when (null bad) $ putStrLn ("ok   " <> caseName c)
        pure (not (null bad))
  when (or failures) $ do
    putStrLn "golden output changed; review the diff, then rerun with GOLDEN_ACCEPT=1"
    exitFailure
  where
    loadNaming :: FilePath -> IO NamingMap
    loadNaming fp = either (fail . ((fp <> ": ") <>)) pure . A.eitherDecode =<< BL.readFile fp
    copyInto from to = do
      createDirectoryIfMissing True (takeDirectory' to)
      copyFile from to
    takeDirectory' = reverse . drop 1 . dropWhile (/= '/') . reverse

-- | Every regular file under a directory, relative to it, sorted.
listFiles :: FilePath -> IO [FilePath]
listFiles root = sort . map (makeRelative root) <$> go root
  where
    go d = do
      names <- listDirectory d
      fmap concat . forM names $ \n -> do
        let p = d </> n
        isDir <- doesDirectoryExist p
        if isDir then go p else pure [p]
