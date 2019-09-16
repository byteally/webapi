{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module HaskellValidation where


import Data.Char as Char
import Data.List as DL
import Constants

setValidConstructorId :: String -> String
setValidConstructorId str = 
  let (_, validName) = setValidFieldName str 
  in (Char.toUpper $ DL.head validName):(DL.tail validName)


setValidFieldName :: String -> (Bool, String)
setValidFieldName fldName = 
  -- Replace invalidId Chars, check if hs keyword and modify else return
  let (isChanged, invalidsFixed) = fixInvalidId fldName
  in case isHsKeyword invalidsFixed of 
       True -> (True, invalidsFixed ++ "_")
       False -> (isChanged, invalidsFixed)

 where
  isHsKeyword :: String -> Bool
  isHsKeyword str = DL.elem str haskellKeywords

fixInvalidId :: String -> (Bool, String)
fixInvalidId idVal
    | idVal == "" = error "Encountered potential empty Haskell Identifier! Please check the Swagger JSONx" 
    | idVal == "_" = (True, "holeName") -- ?? TODO : Is this allowed? Discuss
    | idVal == "\'" = (True, "singleQuoteId") -- TODO : Is this allowed?
    | DL.length idVal == 1 && isValidHsIdChar (DL.head idVal) = (False, fmap Char.toLower idVal)
    | otherwise = do
      let newVal = replaceInvalidChars ("",DL.tail idVal) (DL.head idVal) 
      let lCaseNewVal = makeFirstCharAlpha $ (Char.toLower $ DL.head newVal):(DL.tail newVal)
      case lCaseNewVal == idVal of
        True -> (False, lCaseNewVal)
        False -> (True, lCaseNewVal)

 where

  replaceInvalidChars :: (String, String) -> Char -> String
  replaceInvalidChars (prev, next) currentChar = 
    if isValidHsIdChar currentChar && (not $ DL.null next) 
    then replaceInvalidChars (prev ++ [currentChar], DL.tail next) (DL.head next)
    else if isValidHsIdChar currentChar
         then prev ++ [currentChar]
         -- check for a prefix of invalid chars and return the rest of the next chars
         else do
          let newNext = snd $ DL.break isValidHsIdChar next
          case DL.null newNext of
            True -> prev ++ "_"
            False -> replaceInvalidChars (prev ++ "_", DL.tail newNext ) (DL.head newNext)

  isValidHsIdChar :: Char -> Bool 
  isValidHsIdChar x = (Char.isAlphaNum x) || x == '_' || x == '\''
  
  makeFirstCharAlpha :: String -> String
  makeFirstCharAlpha inpString = 
    case inpString of
      [] -> error "Encountered potential empty Haskell Identifier! Please check the Swagger JSON!"
      firstChar:_ -> 
        case Char.isAlpha firstChar of
          True -> inpString
          False -> 'h':inpString 
