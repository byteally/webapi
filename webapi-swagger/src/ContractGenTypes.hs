{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}


module ContractGenTypes where

-- import Data.Aeson 
-- import Data.Text as T
import GHC.Generics
-- import Network.HTTP.Types.Method
import Data.HashMap.Strict
import Control.Monad.Trans.State.Strict
import qualified Data.Map.Lazy as Map
import Data.Hashable
import SwaggerGen


type StateConfig a = StateT (HashMap LevelInfo [TypeInfo]) IO (a)


type RouteName = String

type RouteAndMethod = (RouteName, Method)

-- data StdMethod
--     = GET
--     | POST
--     | HEAD
--     | PUT
--     | DELETE
--     | TRACE
--     | CONNECT
--     | OPTIONS
--     | PATCH
--     deriving (Show, Eq, Generic, Ord)

-- instance Hashable StdMethod


type DerivingClass = String  
                      -- old name, new name
type ModifiedRecords = [(String, String)]


data JsonDirection = ToJson | FromJson deriving (Eq)

instance Show JsonDirection where
  show ToJson = "P.ToJSON"  
  show FromJson = "P.FromJSON" 

data SumTypeCreation = CreateSumType CreateDataType | ExistingType String 
  deriving (Eq, Show)


data PathComponent = PathComp String | PathParamType String
  deriving (Eq, Show)
   
data SwPathComponent = PathPiece String | PathParamName String
  deriving (Eq, Show)


  -- Phase 1 : Keep global Definitions as `Global DefinitionTy`, Keep all others as Local
  --           Need to add argument to getTypeFromSwaggerType function specifying whether Global or Local and next argument also (location/routeMethod)
  --  
  --           If too many errors, add all TypeInfo as `DefinitionType CreateDataType`

  --           Replace all CreateNewType with CreateDataType
  --           Replace all TypeAlias with NewType
  --           Replace the TypeAlias generation with NewType generation

          
-- Phase 2 : Add retrieval from Global and add Global where applicable
--           Add LevelInfo to ApiTypeDetails and include the new type, remove the old type.
--           Add proper (appropriate) constructors for `TypeInfo`.


data LevelInfo = Global GlobalLocalType | Local GlobalLocalType (RouteName, Method)
  deriving (Eq, Show, Generic)

instance Hashable LevelInfo 

data GlobalLocalType = DefinitionTy | ResponseTy | ParamTy
  deriving (Eq, Show, Generic)

instance Hashable GlobalLocalType


data TypeInfo = ApiErrTy CreateDataType NamingCounter        -- Depends on Content Type : JSON/XML/PlainText (XML left out for now) Including Nested Type.
              | ApiOutTy CreateDataType NamingCounter         -- Depends on Content Type : JSON/XML/PlainText (XML left out for now)
              | FormParamTy CreateDataType NamingCounter      -- FromParam FormParam / ToParam FormParam  
              | QueryParamTy CreateDataType NamingCounter     --   
              | FileParamTy CreateDataType NamingCounter      --  
              | HeaderInTy CreateDataType NamingCounter       --  
              | ReqBodyTy CreateDataType NamingCounter        --
              | ContentTypesTy CreateDataType NamingCounter   --
              | HeaderOutTy CreateDataType NamingCounter      -- 
              | DefinitionType CreateDataType NamingCounter   -- 
  deriving (Eq, Show, Generic)              


data TInfo = ApiErrI 
           | ApiOutI
           | FormParamI
           | QueryParamI
           | FileParamI
           | HeaderInI
           | ReqBodyI
           | ContentTypesI
           | HeaderOutI
           | DefinitionI
  deriving (Eq, Show, Generic)        

data CreateDataType = SumType DualSumType | ProductType NewData OgName | HNewType String String OgName
  deriving (Eq, Show, Generic)

-- The original name of the types as in the Swagger Doc.
type OgName = String

type NamingCounter = Maybe Int

--                                                                         constructor, actual type
data DualSumType = BasicEnum String [String] [String] | ComplexSumType String [(String, String)]
  deriving (Eq, Show, Generic)
-- TODO : Verify if we need to store OgName for the types of ComplexSumType

type InnerRecords = [(String, String)]

data NewData = NewData
  {
    mName :: String                 -- Type/Data Constructor Name
  , mRecordTypes :: InnerRecords
  } deriving (Eq, Show, Generic)


data ParamType = FormParam 
               | QueryParam
               | FileParam
               | HeaderParam
               | BodyParam
  deriving (Eq, Show)

data ContractDetails = ContractDetails
  {
    routeId :: Int 
  , routeName :: String
  , routePath :: [PathComponent]
  , methodData :: Map.Map Method ApiTypeDetails
  } deriving (Eq, Show)


data ApiTypeDetails = ApiTypeDetails
  {
    apiOut :: String
  , apiErr :: Maybe String
  , formParam :: Maybe String
  , queryParam :: Maybe String
  , fileParam :: Maybe String
  , headerIn :: Maybe String
  , requestBody :: Maybe String
  , contentTypes :: Maybe String
  , hasXML :: Bool
  -- TODO: cookie in/out and header out need to be added when we encounter them
  } deriving (Eq, Show)


-- data ApiTypeDetails = ApiTypeDetails
--   {
--     apiOut :: (LevelInfo, String)
--   , apiErr :: (LevelInfo, Maybe String)
--   , formParam :: (LevelInfo, Maybe String)
--   , queryParam :: (LevelInfo, Maybe String)
--   , fileParam :: (LevelInfo, Maybe String)
--   , headerIn :: (LevelInfo, Maybe String)
--   , requestBody :: (LevelInfo, Maybe String)
--   , contentTypes :: (LevelInfo, Maybe String)
--   , headerOut :: (LevelInfo, Maybe String)
--   , hasXML :: Bool
--   } deriving (Eq, Show, Generic)