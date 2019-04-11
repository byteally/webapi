{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}


module Constants where

import ContractGenTypes
import Data.String.Interpolate
import SwaggerGen (Method)


globalTypesModulePath :: String
globalTypesModulePath = "Types/GlobalDefinitions/"

globalTypesHsModuleName :: String
globalTypesHsModuleName = "Types.GlobalDefinitions."

globalDefnsModuleName :: String
globalDefnsModuleName = "TypeDefinitions"

globalRespTypesModuleName :: String
globalRespTypesModuleName = "ResponseDefinitions"

globalParamTypesModuleName :: String
globalParamTypesModuleName = "ParamDefinitions"

hsModuleToFileName :: String -> String
hsModuleToFileName modName = modName ++ ".hs"

localRouteMethodTypesModuleName :: String
localRouteMethodTypesModuleName = "Types"

routeLevelTypesModuleName :: String
routeLevelTypesModuleName = "RouteTypes"

routeLevelTypesPath :: String -> String
routeLevelTypesPath rPath = "Types/" ++ rPath ++ "/" 

routeLevelTypesModName :: String -> String
routeLevelTypesModName rPath = "Types." ++ rPath ++ "."


localRouteMethodTypesPath :: RouteName -> Method -> String
localRouteMethodTypesPath rName stdMethod = "Types/" ++ rName ++ "/" ++ (show stdMethod) ++ "/"

localRouteMethodTypesModName :: RouteName -> Method -> String
localRouteMethodTypesModName rName stdMethod = "Types." ++ rName ++ "." ++ (show stdMethod) ++ "."

globalDefnsQualName :: String
globalDefnsQualName = "Defns"

globalRespTypesQualName :: String
globalRespTypesQualName = "Resps"

globalParamsQualName :: String
globalParamsQualName = "Params"


commonTypesModuleContent :: String
commonTypesModuleContent = [i|
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module CommonTypes where

import Data.ByteString as BS
import qualified Data.ByteString.Char8 as ASCII
import qualified Data.HashMap.Lazy as HM
import WebApi.Param
import qualified Data.Vector as V

import qualified Data.Swagger as SW
import Data.Swagger.Internal.Schema as SW
import Data.Swagger.ParamSchema
import Control.Lens hiding (List)
import Data.Swagger.Internal hiding (Tag, CollectionFormat)

import Data.Text
import Data.Aeson
import GHC.Generics

data CollectionFormat = CSV | SSV | TSV | Pipes 

-- remove all qualifiers (WebApi.)
newtype Collection (format :: CollectionFormat) (t :: *)  = Collection { getCollection :: V.Vector t}
    deriving (Eq, Show)
-- write ToParam instance for Collection


instance (EncodeParam (Collection format t) ) => ToParam 'QueryParam (Collection format t) where
  toParam _ pfx val = [(pfx, Just $ encodeParam val)]

instance (EncodeParam t) => EncodeParam (Collection 'CSV t) where
  encodeParam (Collection innerVector) =  BS.intercalate "," $ fmap (\\singleVal -> encodeParam singleVal ) (V.toList innerVector)


instance (EncodeParam t) => EncodeParam (Collection 'SSV t) where
  encodeParam (Collection innerVector) =  BS.intercalate " " $ fmap (\\singleVal -> encodeParam singleVal ) (V.toList innerVector)

instance (EncodeParam t) => EncodeParam (Collection 'TSV t) where
  encodeParam (Collection innerVector) =  BS.intercalate "\\t" $ fmap (\\singleVal -> encodeParam singleVal ) (V.toList innerVector)

instance (EncodeParam t) => EncodeParam (Collection 'Pipes t) where
  encodeParam (Collection innerVector) =  BS.intercalate "|" $ fmap (\\singleVal -> encodeParam singleVal ) (V.toList innerVector)


instance (DecodeParam (Collection format t) ) => FromParam 'QueryParam (Collection format t) where
  fromParam pt key kvs = case lookupParam pt key kvs of
    Just (Just par) -> case decodeParam par of
          Just v -> Validation $ Right v
          _      -> Validation $ Left [ParseErr key "Unable to cast to Collection"]
    _ ->  Validation $ Left [NotFound key]


instance (DecodeParam t) => DecodeParam (Collection 'CSV t) where
  decodeParam str = 
    case sequenceA $ fmap decodeParam (ASCII.split ',' str) of
      Just parsedList -> Just $ Collection $ V.fromList $ parsedList
      Nothing -> Nothing
    

instance (DecodeParam t) => DecodeParam (Collection 'SSV t) where
  decodeParam str = 
    case sequenceA $ fmap decodeParam (ASCII.split ' ' str) of
      Just parsedList -> Just $ Collection $ V.fromList $ parsedList
      Nothing -> Nothing

    
instance (DecodeParam t) => DecodeParam (Collection 'TSV t) where
  decodeParam str = 
    case sequenceA $ fmap decodeParam (ASCII.split '\\t' str) of
      Just parsedList -> Just $ Collection $ V.fromList $ parsedList
      Nothing -> Nothing


instance (DecodeParam t) => DecodeParam (Collection 'Pipes t) where
  decodeParam str = 
    case sequenceA $ fmap decodeParam (ASCII.split '|' str) of
      Just parsedList -> Just $ Collection $ V.fromList $ parsedList
      Nothing -> Nothing


keyMapping :: HM.HashMap String String -> String -> String
keyMapping hMap k = 
  case HM.lookup k hMap of
    Just foundVal -> foundVal
    Nothing -> k


instance ToSchema (MultiSet Text) where
  declareNamedSchema = plain . paramSchemaToSchema


instance ToParamSchema (MultiSet Text) where
  toParamSchema _ = mempty
      & SW.type_ .~ SwaggerArray
      & SW.items ?~ SwaggerItemsPrimitive Nothing (mempty & SW.type_ .~ SwaggerString)  
      

data SwaggerNumber = IntegerFormat Integer | DoubleFormat Double
  deriving (Eq, Show, Generic)

instance ToJSON SwaggerNumber
instance FromJSON SwaggerNumber
instance ToSchema SwaggerNumber
instance ToParam 'QueryParam SwaggerNumber

instance DecodeParam SwaggerNumber where
  decodeParam bs =
    case decodeParam bs :: Maybe Integer of 
      Just x -> Just $ IntegerFormat x
      Nothing -> 
        case decodeParam bs :: Maybe Double of
          Just y -> Just $ DoubleFormat y
          Nothing -> error $ "Expected SwaggerNumber to coerce into either Integer or Double!"
            ++ "\\nInput param is : " ++ (show bs)
|]




licenseFileContents :: String
licenseFileContents = [i|
Copyright Webapi-Swagger (c) 2019

All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

    * Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

    * Neither the name of Webapi-Swagger nor the names of other
      contributors may be used to endorse or promote products derived
      from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

|]


cabalFileContents :: String -> Bool -> [String] -> String
cabalFileContents projectName webapiXmlNeeded moduleImports = [i|
name:           #{projectName}
version:        0.1.0.0
description:    Generated project with a contract accoriding to the provided Swagger doc.
homepage:       http://byteally.github.io/webapi/
bug-reports:    https://github.com/byteally/webapi/issues
author:         Magesh B
maintainer:     magesh85@gmail.com
copyright:      2018 Byteally
license:        BSD3
license-file:   LICENSE
build-type:     Simple
cabal-version:  >= 1.10

library
  exposed-modules:
      Contract
      CommonTypes
      #{customUnlines moduleImports}
  other-modules:
  hs-source-dirs:
      src
  build-depends:
      base >=4.7 && <5
    , text
    , swagger2
    , aeson
    , bytestring
    , webapi
    , unordered-containers
    , webapi-contract
    , vector
    , time
    #{if webapiXmlNeeded then ", webapi-xml" else ""::String}
  default-language: Haskell2010
  ghc-options: -Wall
  |]
 where
  customUnlines :: [String] -> String
  customUnlines [] = []
  customUnlines (l:ls) = l ++ ("\n      " ++ customUnlines ls)

cabalProjectFileContents :: String 
cabalProjectFileContents = [i|
compiler : ghc
packages : .
source-repository-package
    type: git
    location: https://github.com/byteally/webapi
    tag: 8346e24255a72130ba4097a40aacd7897e0e70de
    subdir: webapi-contract

source-repository-package
    type: git
    location: https://github.com/byteally/webapi
    tag: 8346e24255a72130ba4097a40aacd7897e0e70de
    subdir: webapi

source-repository-package
    type: git
    location: https://github.com/byteally/webapi
    tag: 8346e24255a72130ba4097a40aacd7897e0e70de
    subdir: webapi-xml

source-repository-package
    type: git
    location: https://github.com/capital-match/bytestring-trie.git
    tag: 47526b2ec810239fe824c03c13cf1d81f0741b5c
|]



haskellKeywords :: [String]
haskellKeywords = 
  ["as"
  ,"case"
  ,"of"
  ,"class"
  ,"data"
  ,"data family"
  ,"data instance"
  ,"default"
  ,"deriving"
  ,"deriving instance"
  ,"do"
  ,"forall"
  ,"foreign"
  ,"hiding"
  ,"if"
  ,"then"
  ,"else"
  ,"import"
  ,"infix"
  ,"infixl"
  ,"infixr"
  ,"instance"
  ,"let"
  ,"in"
  ,"mdo"
  ,"module"
  ,"newtype"
  ,"proc"
  ,"qualified"
  ,"rec"
  ,"type"
  ,"type family"
  ,"type instance"
  ,"where"]



languageExtensionsForTypesModule :: [String]
languageExtensionsForTypesModule =  [ 
                                      "TypeFamilies"
                                    , "MultiParamTypeClasses"
                                    , "DeriveGeneric"
                                    , "TypeOperators"
                                    , "DataKinds"
                                    , "TypeSynonymInstances"
                                    , "FlexibleInstances"
                                    , "DuplicateRecordFields"
                                    , "OverloadedStrings"
                                    ]

importsForTypesModule :: [String]
importsForTypesModule = [ 
                          "Prelude ()"
                        ]
                        -- , "Data.Swagger.Schema"
                        -- , "Data.Swagger.Internal.Schema"
                        -- , "Data.Swagger.ParamSchema"
                         -- TODO : This is kind of a hack!
                        -- , "Data.Swagger.Internal hiding (Tag)"

qualifiedImportsForTypesModule :: [(String, String)]  
qualifiedImportsForTypesModule = 
                               [
                                 ("Data.ByteString.Char8", "ASCII")
                               , ("Data.HashMap.Lazy", "HM" )
                               , ("CommonTypes", "P")
                               , ("Data.Text", "P" )
                               , ("Data.Int", "P" )
                               , ("Data.ByteString", "P")
                               , ("Data.Time.Clock", "P" )
                               , ("Data.Time.Calendar", "P" )
                               , ("GHC.Generics", "P" )
                               , ("Data.Aeson", "P" )
                               , ("WebApi.Param", "P" )
                               , ("Data.Text.Encoding", "P" )
                               , ("Prelude", "P" )
                               ]    
                               --  , ("Data.Swagger", (True, Just $ ModuleName noSrcSpan "SW") )

langExtsForContract :: [String]
langExtsForContract = [
                        "TypeFamilies"
                      , "MultiParamTypeClasses"
                      , "DeriveGeneric"
                      , "TypeOperators"
                      , "DataKinds"
                      , "TypeSynonymInstances"
                      , "FlexibleInstances"
                      ]

importsForContract :: [String]
importsForContract = [] --["Data.Int", "Data.Text"]                               

qualImportsForContract :: [(String, String)]
qualImportsForContract = [
                           ("WebApi.Contract", "W")
                         , ("WebApi.Param", "W")
                         , ("Data.Int", "P")
                         , ("Data.Text", "P")
                         ]