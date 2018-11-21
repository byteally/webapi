{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module SwaggerJSONGen where
  
import Data.Aeson as A
import Data.Text as T
import Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import GHC.Generics
import Data.Proxy
import qualified Data.HashMap.Strict.InsOrd as HMSIns 
import qualified Data.Set as Set
import Safe

import Network.HTTP.Types.Method
import Contract
import Types
import Data.Swagger
import Data.Swagger.Declare
import Data.Swagger.Lens as SwaggerLens
import Data.Swagger.Operation

import Control.Arrow
import Control.Lens



swaggerJSON :: BSL.ByteString
swaggerJSON = do
  let api = (mempty :: Swagger) & paths .~ (HMSIns.fromList [("/user", mempty & post ?~ (mempty 
        & SwaggerLens.tags .~ (Set.singleton "user")   
        & responses .~ (mempty & default_ .~ (Just $ Inline (Response "successful operation" Nothing (HMSIns.fromList []) Nothing ) )
                               & responses .~ (HMSIns.fromList []) ) 
        & summary ?~ "Create user"
        & description ?~ "This can only be done by the logged in user."
        & operationId ?~ "createUser" 
        & produces ?~ MimeList ["application/json", "application/xml"]
        & parameters .~ [ Inline $ mempty
                & SwaggerLens.name .~ "body"
                & description ?~ "Created user object"
                & required ?~ True
                & schema .~ (ParamBody $ Ref $ Reference "User") ] ) ),userUserNamePath, petFindByTagsPath] )
  encode api

userUserNamePath :: (FilePath, PathItem)
userUserNamePath = ("/user", mempty 
  & get ?~ (mempty 
        & SwaggerLens.tags .~ (Set.singleton "user")   
        & responses .~ (mempty & default_ .~ (Just $ Inline (Response "successful operation" Nothing (HMSIns.fromList []) Nothing ) )
                               & responses .~ (HMSIns.fromList [(200, Inline $ mempty & description .~ "successful operation" 
                                                                               & schema ?~ (Ref $ Reference "User") ),
                                                         (400, Inline $ mempty & description .~ "Invalid Username supplied" ), 
                                                         (404, Inline $ mempty & description .~ "User Not Found")]) ) 
        & summary ?~ "Get user by user name"
        & description ?~ "This can only be done by the logged in user."
        & operationId ?~ "getUserByName" 
        & produces ?~ MimeList ["application/json", "application/xml"]
        & parameters .~ [ Inline $ mempty
                & SwaggerLens.name .~ "username"
                & description ?~ "The name that needs to be fetched. User User1 for testing"
                & required ?~ True
                & schema .~  (ParamOther (ParamOtherSchema ParamPath Nothing (mempty & type_ .~ SwaggerString ) ) ) ] ) 
  & put ?~ (mempty 
        & SwaggerLens.tags .~ (Set.singleton "user")   
        & responses .~ (mempty & default_ .~ (Just $ Inline (Response "successful operation" Nothing (HMSIns.fromList []) Nothing ) )
                               & responses .~ (HMSIns.fromList [(400, Inline $ mempty & description .~ "Invalid Username supplied" ), 
                                                         (404, Inline $ mempty & description .~ "User Not Found")]) ) 
        & summary ?~ "Updated user"
        & description ?~ "This can only be done by the logged in user."
        & operationId ?~ "updateUser" 
        & produces ?~ MimeList ["application/json", "application/xml"]
        & parameters .~ [ Inline $ mempty
                                  & SwaggerLens.name .~ "username"
                                  & description ?~ "The name that needs to be updated"
                                  & required ?~ True
                                  & schema .~  (ParamOther (ParamOtherSchema ParamPath Nothing (mempty & type_ .~ SwaggerString ) ) ),
                          Inline $ mempty
                                  & SwaggerLens.name .~ "body"
                                  & description ?~ "Updated User Object"
                                  & required ?~ True
                                  & schema .~ (ParamBody $ Ref $ Reference "User") ] ) 
  & SwaggerLens.delete ?~ (mempty
    & SwaggerLens.tags .~ (Set.singleton "user")
    & responses .~ (mempty & responses .~ (HMSIns.fromList [(400, Inline $ mempty & description .~ "Invalid Username supplied" ), 
                                                      (404, Inline $ mempty & description .~ "User Not Found")]) )
    & summary ?~ " Delete user"
    & description ?~ "This can only be done by the logged in user."
    & operationId ?~ "deleteUser" 
    & produces ?~ MimeList ["application/json", "application/xml"]
    & parameters .~ [Inline $ mempty 
        & SwaggerLens.name .~ "username"
        & description ?~ "The name that needs to be deleted"
        & required ?~ True
        & schema .~ (ParamOther (ParamOtherSchema ParamPath Nothing (mempty & type_ .~ SwaggerString ) ) )]

  ) ) 
  
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
  -- TODO: cookie in/out and header out need to be added when we encounter them
  } deriving (Eq, Show)

-- For each Route type (e.g. UserUsernameR) we will have a [(StdMethod, ApiTypeDetails)] and we will parse that in order to generate the arguments required by constructPathOperation function
getParamsAndResponsesFromRoute :: [(StdMethod, ApiTypeDetails)] -> FilePath -> [String] -- [(FilePath, PathItem)]
getParamsAndResponsesFromRoute methodWithApiDetails routeName = do
  flip fmap methodWithApiDetails (\(stdMethod, apiTypeDetails) -> 
    -- for anything other than `Text`, `Int` and other primitive types, return a Ref (use a function for this and call it at all places -> out, err, params)
      case apiOut apiTypeDetails of 
        "()" -> ""
        x -> "" 
      -- case formParam apiTypeDetails of -- consider using fmap here
      --   Just x -> 
      --   Nothing -> 
        )
 where 
  -- for Params, check if body or not, then construct value of ParamAnySchema.
  -- If Body Param then first argument will be `Nothing`
  constructParamSchema :: Maybe ParamLocation -> Text -> ParamAnySchema
  constructParamSchema mParamLocation paramType = 
    case mParamLocation of 
      Just otherLocation -> paramOtherSchema otherLocation paramType
          -- ParamHeader -> 
          -- ParamPath -> 
          -- ParamFormData ->   
      Nothing -> (ParamBody $ Ref $ Reference paramType )
  paramOtherSchema paramLocation pType = 
    case pType `Prelude.elem` primitiveTypes of
      True -> let (swaggerType, swFormat) = (getSwaggerTypeFromHType pType)
              in (ParamOther $ mempty & in_ .~ paramLocation 
                                      & paramSchema .~ (mempty & (type_ .~ swaggerType)
                                                               & format .~ swFormat ) )
      False -> 
        case T.isPrefixOf "[" pType of
          True -> do
            let prefixStripped = fromJustNote "Type is Array. But no [ found" $ T.stripPrefix "[" pType
                listBracketsRemovedType = fromJustNote "Type is Array. But no ] found" $ T.stripSuffix "]" prefixStripped
            case listBracketsRemovedType `Prelude.elem` primitiveTypes of
              True -> do -- construct ParamAnySchema with Array of Primitive SwaggerType
                let (swaggerType, swFormat) = getSwaggerTypeFromHType listBracketsRemovedType
                (ParamOther $ mempty & in_ .~ paramLocation 
                                        & paramSchema .~ (mempty & (type_ .~ SwaggerArray) 
                                                                 & items ?~ SwaggerItemsPrimitive Nothing (mempty & type_ .~ swaggerType
                                                                                                                  & format .~ swFormat )  ) )
                                        -- Just (SwaggerItemsObject (Inline (Schema 
                                                                            -- _schemaParamSchema = ParamSchema {
                                                                                                    -- _paramSchemaType = SwaggerString and set Format also
              False -> error "Encountered list of custom data type for Param. This needs to be handled!"
                -- (ParamOther $ mempty & in_ .~ Param 
                --                      & paramSchema .~ (mempty & (type_ .~ SwaggerArray) 
                --                                               & items ?~ (review _SwaggerItemsObject $ Ref $ Reference listBracketsRemovedType)) )
              -- _paramSchemaItems = Just (SwaggerItemsObject (Ref (Reference {getReference = "Pet"})))
          -- False ->  -- construct ParamAnySchema with Ref type (unless it's a QueryParam)
          -- check for Set or Collection here. for MultiCollection
          

        
  -- ParamOther $ ParamOtherSchema ($depends on `in` value) (Nothing) (paramSchema -> type and format here)
  constructRefResponse :: Text -> Referenced Response
  constructRefResponse typeStr =
    -- edgecase : when it's an array, we need to check if it's an array of primitive. Usually would not be the case.
    case  typeStr `Prelude.elem` primitiveTypes of
      True -> 
        let (swaggerType, swaggerFormat) = getSwaggerTypeFromHType typeStr
        in Inline $ mempty & schema ?~ (Inline $ mempty & paramSchema .~ (mempty & type_ .~ swaggerType 
                                                                                 & format .~ swaggerFormat) )
      False -> Ref $ Reference typeStr -- TODO: take care of arrays of custom types and then primitives
  getSwaggerTypeFromHType haskellType = 
    case haskellType of
      "Text" -> (SwaggerString, Nothing)
      "Bool" -> (SwaggerBoolean, Nothing)
      "Day" -> (SwaggerString, Just "date")
      "UTCTime" ->  (SwaggerString, Just "date-time")
      "ByteString" ->  (SwaggerString, Just "byte")
      "Float" ->  (SwaggerNumber, Just "float")
      "Double" -> (SwaggerNumber, Just "double")
      "Int32" -> (SwaggerInteger, Just "int32")
      "Int64" -> (SwaggerInteger, Just "int64")



primitiveTypes = ["Day", "UTCTime", "ByteString", "Text", "Float", "Double", "Int32", "Int64", "Bool"]


--                                                  RespCode                       ParamName (not present?)
constructPathOperation :: StdMethod -> FilePath -> [(Int, Referenced Response)] -> [(Text, ParamAnySchema)] -> (FilePath, PathItem)
constructPathOperation stdMethod routeName respCodeWithTypes paramNameWithTypes = do
  case stdMethod of
    GET -> (routeName, mempty & get ?~ constructPathItem)
    POST -> (routeName, mempty & post ?~ constructPathItem)
    PUT -> (routeName, mempty & put ?~ constructPathItem)
    PATCH -> (routeName, mempty & patch ?~ constructPathItem)
    DELETE ->(routeName, mempty & delete ?~ constructPathItem)
 where
  constructPathItem = (mempty & responses .~ (mempty & responses .~ HMSIns.fromList respCodeWithTypes ) 
                                               & produces ?~ MimeList ["application/json", "application/xml"]
                                               & parameters .~ (processParams paramNameWithTypes) ) 
  processParams paramList = flip fmap paramList (\(paramName, paramInfo) -> Inline $ mempty & SwaggerLens.name .~ paramName
                                                                              --  & required ?~ True
                                                                                            & schema .~ paramInfo )


petFindByTagsPath :: (FilePath, PathItem)
petFindByTagsPath = ("/pet/findByTags", mempty
    & get ?~ (mempty 
        & SwaggerLens.tags .~ (Set.singleton "pet")   
        & responses .~ (mempty & responses .~ (HMSIns.fromList [(200, Inline $ mempty & description .~ "successful operation" 
                                                                               & schema ?~ (Inline $ mempty & (paramSchema .~ (mempty & type_ .~ SwaggerArray   
                                                                                                                                      & items ?~ (SwaggerItemsObject (Ref $ Reference "Pet") ) ) ) ) ), 
                                                         (400, Inline $ mempty & description .~ "Invalid Tag Value" )]) ) 
        & summary ?~ "Finds Pets by tags"
        & description ?~ "Muliple tags can be provided with comma separated strings. Use         tag1, tag2, tag3 for testing."
        & operationId ?~ "findPetsByTags" 
        & produces ?~ MimeList ["application/json", "application/xml"]
        & parameters .~ [ Inline $ mempty
                & SwaggerLens.name .~ "tags"
                & description ?~ "Tags to filter by"
                & required ?~ True
                & schema .~  (ParamOther (ParamOtherSchema ParamQuery Nothing (mempty & type_ .~ SwaggerArray 
                                                                                      & items ?~ SwaggerItemsPrimitive (Just CollectionMulti) (mempty &  type_ .~ SwaggerString)  ) ) ) ] ) )


declTestSwagger :: Declare (Definitions Schema) Swagger
declTestSwagger = do
  userResp <- declareResponse (Proxy :: Proxy User)
  stsResponse <- declareResponse (Proxy :: Proxy Status) 
  -- exRoute <- declareResponse (Proxy :: Proxy UserR)
  pure $ mempty
 
userSwagger :: Swagger
userSwagger =
  let (defs, spec) = runDeclare declTestSwagger mempty
  in spec { _swaggerDefinitions = defs }


getRecordNamesForQueryParam :: [Text]
getRecordNamesForQueryParam = do
  let (defs, _) = runDeclare (declareResponse (Proxy :: Proxy UserLoginRGETQueryParam)) mempty
  case HMSIns.toList defs of
    (dataName, dataSchema):[] -> do
      let schemaPropertyList = HMSIns.toList $ _schemaProperties dataSchema 
      let (recordNames, refSchemaList) = Prelude.unzip schemaPropertyList
      recordNames
    _ -> error "Expecting only one element in the definitions list"
