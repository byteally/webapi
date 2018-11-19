{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module SwaggerJSONGen where
  
import Data.Aeson as A
import Data.List as DL
import Data.Text as T
import Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import GHC.Generics
import Data.Proxy
import Data.HashMap.Strict.InsOrd as HMSIns
import qualified Data.Set as Set


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
  let api = (mempty :: Swagger) & paths .~ (fromList [("/user", mempty & post ?~ (mempty 
        & SwaggerLens.tags .~ (Set.singleton "user")   
        & responses .~ (mempty & default_ .~ (Just $ Inline (Response "successful operation" Nothing (fromList []) Nothing ) )
                               & responses .~ (fromList []) ) 
        & summary ?~ "Create user"
        & description ?~ "This can only be done by the logged in user."
        & operationId ?~ "createUser" 
        & produces ?~ MimeList ["application/json", "application/xml"]
        & parameters .~ [ Inline $ mempty
                & SwaggerLens.name .~ "body"
                & description ?~ "Created user object"
                & required ?~ True
                & schema .~ (ParamBody $ Ref $ Reference "User") ] ) ),userUserNamePath] )
  encode api

userUserNamePath :: (FilePath, PathItem)
userUserNamePath = ("/user", mempty 
  & get ?~ (mempty 
        & SwaggerLens.tags .~ (Set.singleton "user")   
        & responses .~ (mempty & default_ .~ (Just $ Inline (Response "successful operation" Nothing (fromList []) Nothing ) )
                               & responses .~ (fromList [(200, Inline $ mempty & description .~ "successful operation" 
                                                                               & schema ?~ (Ref $ Reference "User") ),
                                                         (400, Inline $ mempty & description .~ "Invalid Username supplied" ), 
                                                         (404, Inline $ mempty & description .~ "User Not Found")]) ) 
        & summary ?~ "Create user"
        & description ?~ "This can only be done by the logged in user."
        & operationId ?~ "createUser" 
        & produces ?~ MimeList ["application/json", "application/xml"]
        & parameters .~ [ Inline $ mempty
                & SwaggerLens.name .~ "body"
                & description ?~ "Created user object"
                & required ?~ True
                & schema .~ (ParamBody $ Ref $ Reference "User") ] ) 
  & SwaggerLens.delete ?~ (mempty
    & SwaggerLens.tags .~ (Set.singleton "user")
    & responses .~ (mempty & responses .~ (fromList [(400, Inline $ mempty & description .~ "Invalid Username supplied" ), 
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