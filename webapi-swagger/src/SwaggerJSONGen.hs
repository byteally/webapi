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
  


--                                                  RespCode          ParamName
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