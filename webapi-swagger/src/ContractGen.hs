{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE CPP #-}



module ContractGen where

import Control.Lens
import Data.Aeson
import Data.Proxy
import Data.Text as T
import GHC.Generics
import qualified Data.ByteString.Lazy as BSL
import Data.HashMap.Strict.InsOrd as HMSIns
import Language.Haskell.Exts
import Data.Vector.Sized as SV hiding ((++))
import Safe
import Data.Finite.Internal

import Data.Swagger
import Data.Swagger.Declare
import Data.Swagger.Lens
import Data.Swagger.Operation


readSwaggerJSON :: IO()
readSwaggerJSON = do
  petstoreJSONContents <- BSL.readFile "sampleFiles/swagger-petstore-ex.json"
  let decodedVal = eitherDecode petstoreJSONContents -- :: Either String Data.Swagger.Internal.Swagger
  case decodedVal of
    Left errMsg -> putStrLn errMsg
    Right (swaggerData :: Swagger) -> do
      case HMSIns.toList $ _swaggerPaths swaggerData of 
        (filePath, pathInfo):xs -> putStrLn $ show pathInfo
        _ -> putStrLn "Paths are empty? Empty list encountered!" 

  
parseHaskellSrcContract :: IO ()
parseHaskellSrcContract = do
  parseResult <- parseFile "sampleFiles/contract.hs"
  case parseResult of
    ParseOk hModule -> 
      case hModule of
        Module srcLoc (Just _) langPragmas imports declarations -> putStrLn $ show declarations
        _ -> error "Module is not in the correct format?!"
    ParseFailed srcLoc errMsg -> putStrLn $ (show srcLoc) ++ " : " ++ errMsg


instanceTopVec :: Vector 4 String
instanceTopVec = fromJustNote "Expected a list with 4 elements for WebApi instance!" $ SV.fromList ["ApiContract", "EDITranslatorApi", "POST", "EdiToJsonR" ]

instanceTypeVec :: [Vector 4 String]
instanceTypeVec = [
                    ( fromMaybeSV $ SV.fromList ["ApiOut", "POST", "EdiToJsonR", "Value" ])
                  , ( fromMaybeSV $ SV.fromList ["ApiErr", "POST", "EdiToJsonR", "Text" ])
                  , ( fromMaybeSV $ SV.fromList ["FormParam", "POST", "EdiToJsonR", "EdiStr" ])
                  , ( fromMaybeSV $ SV.fromList ["QueryParam", "POST", "EdiToJsonR", "Maybe CharacterSet"]) 
                  ]
 where 
  fromMaybeSV = fromJustNote "Expected a list with 4 elements for WebApi instance! "

fromParamVec :: Vector 3 String
fromParamVec = fromJustNote "Expected a list with 3 elements for WebApi instance!" $ SV.fromList ["FromParam", "FormParam", "EdiStr"]

printHaskellModule :: IO()
printHaskellModule = 
  let hModule = Module noSrcSpan (Just $ ModuleHead noSrcSpan (ModuleName noSrcSpan "Contract") Nothing Nothing)
        (fmap languageExtension ["TypeFamilies", "MultiParamTypeClasses", "DeriveGeneric", "TypeOperators", "DataKinds"])
        (fmap (moduleImport (False, "")) [ "WebApi",  "Data.Aeson",  "Data.ByteString",  "Data.Text as T",  "GHC.Generics"])
        [
        emptyDataDeclaration "EDITranslatorApi",
        dataDeclaration (NewType noSrcSpan) "EdiStr" [("ediStr", "ByteString")] ["Show", "Generic"],
        dataDeclaration (NewType noSrcSpan) "CharacterSet" [("characterSet", "ByteString")] ["Show", "Generic"],
        dataDeclaration (DataType noSrcSpan) "EdiJsonWithDelims" [("ediJson", "ByteString"), ("segmentDelimiter", "Char"), ("elementDelimiter", "Char")] ["Show", "Generic"],
        apiInstanceDeclaration instanceTopVec instanceTypeVec, 
        fromParamInstanceDecl fromParamVec
            
        ]
  in writeFile "webapi-swagger/sampleFiles/codeGen.hs" $ prettyPrint hModule

-- Support multiple versions of GHC (Use ifndef )
-- for LTS 9.0 -> 1.18.2

type InnerRecords = [(String, String)]
type DerivingClass = String  

dataDeclaration :: (DataOrNew SrcSpanInfo) -> String -> InnerRecords -> [DerivingClass] -> Decl SrcSpanInfo
dataDeclaration dataOrNew dataName innerRecords derivingList = 
  DataDecl noSrcSpan  
    dataOrNew 
    Nothing 
    (declarationHead dataName)
    (constructorDeclaration dataName innerRecords)
    (Just $ derivingDecl  derivingList)


-- fullDataDeclaration :: String -> 

declarationHead :: String -> DeclHead SrcSpanInfo
declarationHead declHeadName = (DHead noSrcSpan (Ident noSrcSpan declHeadName) )

constructorDeclaration :: String -> InnerRecords -> [QualConDecl SrcSpanInfo]
constructorDeclaration constructorName innerRecords = 
  [QualConDecl noSrcSpan Nothing Nothing (RecDecl noSrcSpan (nameDecl constructorName) (fmap fieldDecl innerRecords) )] 

nameDecl :: String -> Name SrcSpanInfo
nameDecl = Ident noSrcSpan 

fieldDecl :: (String, String) -> FieldDecl SrcSpanInfo
fieldDecl (fieldName, fieldType) = 
  FieldDecl noSrcSpan [nameDecl fieldName] (TyCon noSrcSpan (UnQual noSrcSpan (nameDecl fieldType)))

derivingDecl :: [String] -> Deriving SrcSpanInfo
derivingDecl derivingList = Deriving noSrcSpan $ fmap iRule derivingList
 where 
  iRule tClass = IRule noSrcSpan Nothing Nothing (IHCon noSrcSpan (UnQual noSrcSpan (nameDecl tClass)))

emptyDataDeclaration :: String -> Decl SrcSpanInfo
emptyDataDeclaration declName = 
  DataDecl noSrcSpan 
    (DataType noSrcSpan) 
    Nothing
    (declarationHead declName) 
    []
    Nothing

languageExtension :: String -> ModulePragma SrcSpanInfo
languageExtension langExtName = LanguagePragma noSrcSpan [nameDecl langExtName]


-- Modules imported as *NOT qualified* by default for now
moduleImport :: (Bool, String) -> String -> ImportDecl SrcSpanInfo
moduleImport (isQualified, qualifiedName) moduleName  = ImportDecl {importAnn = noSrcSpan, importModule = ModuleName noSrcSpan moduleName, importQualified = False, importSrc = False, importSafe = False, importPkg = Nothing, importAs = Nothing, importSpecs = Nothing}


apiInstanceDeclaration :: Vector 4 String -> [Vector 4 String] -> Decl SrcSpanInfo
apiInstanceDeclaration topLevelDecl innerTypesInstList = 
  InstDecl noSrcSpan Nothing 
    (IRule noSrcSpan Nothing Nothing
      (IHApp noSrcSpan 
        (IHApp noSrcSpan 
          (IHApp noSrcSpan 
            (instanceHead (SV.index topLevelDecl (Finite 0) ) )
            (typeConstructor $ SV.index topLevelDecl (Finite 1)  )
          )
          (typeConstructor $ SV.index topLevelDecl (Finite 2)  )
        ) 
        (typeConstructor $ SV.index topLevelDecl (Finite 3) )
      ) 
    ) (Just $ fmap apiInstanceTypeDecl innerTypesInstList)


apiInstanceTypeDecl :: Vector 4 String -> InstDecl SrcSpanInfo 
apiInstanceTypeDecl innerTypes =
  InsType noSrcSpan
    (TyApp noSrcSpan
        (TyApp noSrcSpan
          (typeConstructor (SV.index innerTypes (Finite 0) ) )
          (typeConstructor (SV.index innerTypes (Finite 1) ) )
        )
      (typeConstructor (SV.index innerTypes (Finite 2) ) )
    )
    (typeConstructor (SV.index innerTypes (Finite 3) ) )

instanceHead :: String -> InstHead SrcSpanInfo
instanceHead instName = (IHCon noSrcSpan
                          (UnQual noSrcSpan $ nameDecl instName)
                        ) 


typeConstructor :: String -> Type SrcSpanInfo
typeConstructor typeConName = (TyCon noSrcSpan  
                                (UnQual noSrcSpan $ nameDecl typeConName)
                              )

fromParamInstanceDecl :: Vector 3 String -> Decl SrcSpanInfo 
fromParamInstanceDecl instTypes = 
  InstDecl noSrcSpan Nothing 
      (IRule noSrcSpan Nothing Nothing 
        (IHApp noSrcSpan 
          (IHApp noSrcSpan 
            (instanceHead $ SV.index instTypes (Finite 0) )
            (TyPromoted noSrcSpan (PromotedCon noSrcSpan True (UnQual noSrcSpan (nameDecl $ SV.index instTypes (Finite 1) ))))
          )
          (typeConstructor $ SV.index instTypes (Finite 2) )
          )
        ) 
      Nothing


---------------------------------------------------------------------------------------
#if MIN_VERSION_haskell_src_exts(1,20,0)
-- for haskell-src-exts 1.20.x
dataDeclaration :: Decl SrcSpanInfo
dataDeclaration = 
    DataDecl noSrcSpan  
      (NewType noSrcSpan) 
      Nothing 
      (DHead noSrcSpan (Ident noSrcSpan "CharacterSet") )
      [QualConDecl noSrcSpan Nothing Nothing (RecDecl noSrcSpan (Ident noSrcSpan "CharacterSet") [FieldDecl noSrcSpan [Ident noSrcSpan "characterSet"] (TyCon noSrcSpan (UnQual noSrcSpan (Ident noSrcSpan "ByteString")))])] 
      [Deriving noSrcSpan Nothing [IRule noSrcSpan Nothing Nothing (IHCon noSrcSpan (UnQual noSrcSpan (Ident noSrcSpan "Show"))),IRule noSrcSpan Nothing Nothing (IHCon noSrcSpan (UnQual noSrcSpan (Ident noSrcSpan "Generic")))]]
#endif
