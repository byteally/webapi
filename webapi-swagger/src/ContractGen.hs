{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
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


printHaskellModule :: IO()
printHaskellModule = 
  let hModule = Module noSrcSpan (Just $ ModuleHead noSrcSpan (ModuleName noSrcSpan "Contract") Nothing Nothing)
        [LanguagePragma noSrcSpan [Ident noSrcSpan "TypeFamilies"],
        LanguagePragma noSrcSpan [Ident noSrcSpan "MultiParamTypeClasses"],
        LanguagePragma noSrcSpan [Ident noSrcSpan "DeriveGeneric"],
        LanguagePragma noSrcSpan [Ident noSrcSpan "TypeOperators"],
        LanguagePragma noSrcSpan [Ident noSrcSpan "DataKinds"]        
        ]
        [ImportDecl {importAnn = noSrcSpan, 
          importModule = ModuleName noSrcSpan "WebApi", importQualified = False, importSrc = False, importSafe = False, importPkg = Nothing, importAs = Nothing, importSpecs = Nothing}
        , ImportDecl {importAnn = noSrcSpan, 
        importModule = ModuleName noSrcSpan "Data.Aeson", importQualified = False, importSrc = False, importSafe = False, importPkg = Nothing, importAs = Nothing, importSpecs = Nothing}
        , ImportDecl {importAnn = noSrcSpan, 
          importModule = ModuleName noSrcSpan "Data.ByteString", importQualified = False, importSrc = False, importSafe = False, importPkg = Nothing, importAs = Nothing, importSpecs = Nothing}
        , ImportDecl {importAnn = noSrcSpan, 
        importModule = ModuleName noSrcSpan "Data.Text as T", importQualified = False, importSrc = False, importSafe = False, importPkg = Nothing, importAs = Nothing, importSpecs = Nothing}
        , ImportDecl {importAnn = noSrcSpan, 
        importModule = ModuleName noSrcSpan "GHC.Generics", importQualified = False, importSrc = False, importSafe = False, importPkg = Nothing, importAs = Nothing, importSpecs = Nothing}
        ]
        [
        emptyDataDeclaration "EDITranslatorApi",
        dataDeclaration (NewType noSrcSpan) "EdiStr" [("ediStr", "ByteString")] ["Show", "Generic"],
        dataDeclaration (NewType noSrcSpan) "CharacterSet" [("characterSet", "ByteString")] ["Show", "Generic"],
        dataDeclaration (DataType noSrcSpan) "EdiJsonWithDelims" [("ediJson", "ByteString"), ("segmentDelimiter", "Char"), ("elementDelimiter", "Char")] ["Show", "Generic"],
        
        InstDecl noSrcSpan Nothing 
            (IRule noSrcSpan Nothing Nothing 
              (IHApp noSrcSpan
                  (IHApp noSrcSpan
                      (IHApp noSrcSpan
                          (IHCon noSrcSpan
                              (UnQual noSrcSpan
                                  (Ident noSrcSpan "ApiContract")
                              )
                          ) 
                          (TyCon noSrcSpan  
                              (UnQual noSrcSpan
                                  (Ident noSrcSpan "EDITranslatorApi")
                              )
                          )
                      ) 
                      (TyCon noSrcSpan  
                          (UnQual noSrcSpan
                              (Ident noSrcSpan "POST")
                          )
                      )
                  ) 
                  (TyCon noSrcSpan 
                      (UnQual noSrcSpan
                          (Ident noSrcSpan "EdiToJsonR")
                      )
                  )
              ) )
            (Just 
                [InsType noSrcSpan
                    (TyApp noSrcSpan
                        (TyApp noSrcSpan
                            (TyCon noSrcSpan
                                (UnQual noSrcSpan
                                    (Ident noSrcSpan "ApiOut")
                                )
                            ) 
                            (TyCon noSrcSpan 
                                (UnQual noSrcSpan 
                                    (Ident noSrcSpan "POST")
                                )
                            )
                        ) 
                        (TyCon noSrcSpan 
                            (UnQual noSrcSpan 
                                (Ident noSrcSpan "EdiToJsonR")
                            )
                        )
                    ) 
                    
                    (TyCon noSrcSpan 
                        (UnQual noSrcSpan 
                            (Ident noSrcSpan "Value")
                        )
                    ),
                InsType noSrcSpan (TyApp noSrcSpan (TyApp noSrcSpan (TyCon noSrcSpan (UnQual noSrcSpan (Ident noSrcSpan"ApiErr"))) (TyCon noSrcSpan (UnQual noSrcSpan  (Ident noSrcSpan "POST")))) (TyCon noSrcSpan (UnQual noSrcSpan  (Ident noSrcSpan "EdiToJsonR")))) (TyCon noSrcSpan (UnQual noSrcSpan  (Ident noSrcSpan "Text"))),
                InsType noSrcSpan (TyApp noSrcSpan (TyApp noSrcSpan (TyCon noSrcSpan (UnQual noSrcSpan (Ident noSrcSpan"FormParam"))) (TyCon noSrcSpan (UnQual noSrcSpan  (Ident noSrcSpan "POST")))) (TyCon noSrcSpan (UnQual noSrcSpan  (Ident noSrcSpan "EdiToJsonR")))) (TyCon noSrcSpan (UnQual noSrcSpan  (Ident noSrcSpan "EdiStr"))),
                InsType noSrcSpan (TyApp noSrcSpan (TyApp noSrcSpan (TyCon noSrcSpan (UnQual noSrcSpan (Ident noSrcSpan"QueryParam"))) (TyCon noSrcSpan (UnQual noSrcSpan  (Ident noSrcSpan "POST")))) (TyCon noSrcSpan (UnQual noSrcSpan  (Ident noSrcSpan "EdiToJsonR")))) (TyApp noSrcSpan (TyCon noSrcSpan  (UnQual noSrcSpan (Ident noSrcSpan "Maybe"))) (TyCon noSrcSpan (UnQual noSrcSpan (Ident noSrcSpan "CharacterSet"))))])
        ]
  in writeFile "sampleFiles/codeGen.hs" $ prettyPrint hModule

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
nameDecl name = Ident noSrcSpan name

fieldDecl :: (String, String) -> FieldDecl SrcSpanInfo
fieldDecl (fieldName, fieldType) = 
  FieldDecl noSrcSpan [nameDecl fieldName] (TyCon noSrcSpan (UnQual noSrcSpan (nameDecl fieldType)))

derivingDecl :: [String] -> Deriving SrcSpanInfo
derivingDecl derivingList = Deriving noSrcSpan $ fmap iRule derivingList
 where 
  iRule tClass = IRule noSrcSpan Nothing Nothing (IHCon noSrcSpan (UnQual noSrcSpan (nameDecl tClass)))

emptyDataDeclaration :: String -> Decl SrcSpanInfo
emptyDataDeclaration name = 
  DataDecl noSrcSpan 
    (DataType noSrcSpan) 
    Nothing
    (declarationHead name) 
    []
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
