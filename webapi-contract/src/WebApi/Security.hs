{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE DataKinds                  #-}
module WebApi.Security where

import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics
import Data.Aeson
import GHC.TypeLits

newtype URL = URL { getUrl :: Text }
  deriving (Eq, Ord, Show, ToJSON, FromJSON)

-- | The location of the API key.
data ApiKeyLocation
  = ApiKeyQuery
  | ApiKeyHeader
  | ApiKeyCookie
  deriving (Eq, Show, Generic)

data ApiKeyParams str = ApiKeyParams
  { -- | The name of the header or query parameter to be used.
    apiKeyName :: str

    -- | The location of the API key.
  , apiKeyIn :: ApiKeyLocation
  } deriving (Eq, Show, Generic)

-- | The authorization URL to be used for OAuth2 flow. This SHOULD be in the form of a URL.
type AuthorizationURL = Text

-- | The token URL to be used for OAuth2 flow. This SHOULD be in the form of a URL.
type TokenURL = Text

newtype OAuth2ImplicitFlow
  = OAuth2ImplicitFlow {oAuth2ImplicitFlowAuthorizationUrl :: AuthorizationURL}
  deriving (Eq, Show, Generic)

newtype OAuth2PasswordFlow
  = OAuth2PasswordFlow {oAuth2PasswordFlowTokenUrl :: TokenURL}
  deriving (Eq, Show, Generic)

newtype OAuth2ClientCredentialsFlow
  = OAuth2ClientCredentialsFlow {oAuth2ClientCredentialsFlowTokenUrl :: TokenURL}
  deriving (Eq, Show, Generic)

data OAuth2AuthorizationCodeFlow = OAuth2AuthorizationCodeFlow
  { oAuth2AuthorizationCodeFlowAuthorizationUrl :: AuthorizationURL
  , oAuth2AuthorizationCodeFlowTokenUrl :: TokenURL
  } deriving (Eq, Show, Generic)

data OAuth2Flow p = OAuth2Flow
  { oAuth2Params :: p

    -- | The URL to be used for obtaining refresh tokens.
  , oAath2RefreshUrl :: Maybe URL

    -- | The available scopes for the OAuth2 security scheme.
    -- A map between the scope name and a short description for it.
    -- The map MAY be empty.
  , oAuth2Scopes :: [(Text, Text)]
  } deriving (Eq, Show, Generic)

data OAuth2Flows = OAuth2Flows
  { -- | Configuration for the OAuth Implicit flow
    oAuth2FlowsImplicit :: Maybe (OAuth2Flow OAuth2ImplicitFlow)

    -- | Configuration for the OAuth Resource Owner Password flow
  , oAuth2FlowsPassword :: Maybe (OAuth2Flow OAuth2PasswordFlow)

    -- | Configuration for the OAuth Client Credentials flow
  , oAuth2FlowsClientCredentials :: Maybe (OAuth2Flow OAuth2ClientCredentialsFlow)

    -- | Configuration for the OAuth Authorization Code flow
  , oAuth2FlowsAuthorizationCode :: Maybe (OAuth2Flow OAuth2AuthorizationCodeFlow)
  } deriving (Eq, Show, Generic)

type BearerFormat = Text

data HttpSchemeType
  = HttpSchemeBearer (Maybe BearerFormat)
  | HttpSchemeBasic
  | HttpSchemeCustom Text
  deriving (Eq, Show, Generic)

data SecuritySchemeType str
  = SecuritySchemeHttp HttpSchemeType
  | SecuritySchemeApiKey (ApiKeyParams str)
  | SecuritySchemeOAuth2 OAuth2Flows
  | SecuritySchemeOpenIdConnect URL
  deriving (Eq, Show, Generic)

data SecurityScheme = SecurityScheme
  { -- | The type of the security scheme.
    securitySchemeType :: SecuritySchemeType Text

    -- | A short description for security scheme.
  , securitySchemeDescription :: Maybe Text
  } deriving (Eq, Show, Generic)

newtype SecurityDefinitions
  = SecurityDefinitions [(Text, SecurityScheme)]
  deriving (Eq, Show, Generic)

-- | Lists the required security schemes to execute this operation.
-- The object can have multiple security schemes declared in it which are all required
-- (that is, there is a logical AND between the schemes).
newtype SecurityRequirement str = SecurityRequirement
  { getSecurityRequirement :: [(str, [str])]
  } deriving (Eq, Read, Show, Semigroup, Monoid, ToJSON, FromJSON)


type ApiKeyInCookie (k :: Symbol) = 'SecuritySchemeApiKey ('ApiKeyParams k 'ApiKeyCookie)

type ApiKeyInHeader (k :: Symbol) = 'SecuritySchemeApiKey ('ApiKeyParams k 'ApiKeyHeader)

type EmptySecurityReq = '[ 'SecurityRequirement '[]]

type family NeedCSRFCheck' (sec :: SecuritySchemeType Symbol) :: Bool where
  NeedCSRFCheck' ('SecuritySchemeApiKey ('ApiKeyParams _ 'ApiKeyCookie)) = 'True
  NeedCSRFCheck' _ = 'False

type family NeedCSRFCheck (sreqs :: [SecurityRequirement Symbol]) (schs :: [(Symbol, SecuritySchemeType Symbol)]) :: Bool where
  NeedCSRFCheck '[] _ = 'False
  NeedCSRFCheck ( _ ': sreqs) schs = 'False


needCSRFCheck :: [SecurityRequirement Text] -> [(Text, SecuritySchemeType Text)] -> Bool
needCSRFCheck  [] _ = False
needCSRFCheck (sreq : sreqs) schs
  | any checkSch $ getSecurityRequirement sreq = True
  | otherwise = needCSRFCheck sreqs schs
  where checkSch (name, _) = case lookup name schs of
          Nothing -> error $ "Panic: Invalid security schema: " <> (T.unpack name)
          Just (SecuritySchemeApiKey (ApiKeyParams {apiKeyIn=ApiKeyCookie})) -> True
          Just _ -> False
