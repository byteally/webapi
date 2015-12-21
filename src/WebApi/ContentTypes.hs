{-|
Module      : WebApi.ContentTypes
License     : BSD3
Stability   : experimental
-}

{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}
module WebApi.ContentTypes
       (
       -- * Predefined Content Types.
         JSON
       -- * Creating custom Content Types. 
       , Accept (..)
       , Encode (..)
       , Decode (..)
       -- * Internal classes.
       , Encodings (..)
       , Decodings (..)
       ) where

import           Blaze.ByteString.Builder           (Builder)
import qualified Blaze.ByteString.Builder.Char.Utf8 as Utf8 (fromText)
import           Data.Aeson                         (ToJSON (..), FromJSON (..), eitherDecodeStrict)
import           Data.Aeson.Encode                  (encodeToByteStringBuilder)
import           Data.ByteString                    (ByteString)
import           Data.Proxy
import qualified Data.Text                          as TextS
import           Data.Text.Encoding                 (decodeUtf8)
import           Network.HTTP.Media.MediaType

-- | Type representing "application/json" Content-Type
data JSON

-- | Type representing the "text/plain" Content-Type  
data PlainText

-- | Type representing the "application/octetstream" Content-Type  
data OctetStream

class Encodings (ctypes :: [*]) a where
  encodings :: Proxy ctypes -> a -> [(MediaType, Builder)]

instance ( Accept ctype
         , Encode ctype a
         , Encodings ctypes a
         ) => Encodings (ctype ': ctypes) a where
  encodings _ a =  (contentType (Proxy :: Proxy ctype), encode (Proxy :: Proxy ctype) a) : encodings (Proxy :: Proxy ctypes) a

instance Encodings '[] a where
  encodings _ _ = []

class Decodings (ctypes :: [*]) a where
  decodings :: Proxy ctypes -> ByteString -> [(MediaType, Either String a)]

instance ( Accept ctype
         , Decode ctype a
         , Decodings ctypes a
         ) => Decodings (ctype ': ctypes) a where
  decodings _ bs =  (contentType (Proxy :: Proxy ctype), decode (Proxy :: Proxy ctype) bs) : decodings (Proxy :: Proxy ctypes) bs

instance Decodings '[] a where
  decodings _ _ = []

-- | Singleton class for Content Type. 
class Accept ctype where
  contentType :: Proxy ctype -> MediaType

instance Accept JSON where
  contentType _ = "application" // "json"

instance Accept PlainText where
  contentType _ = "text" // "plain" /: ("charset", "utf-8")

instance Accept OctetStream where
  contentType _ = "application" // "octet-stream"

-- | Encode a type into a specific Content Type.
class (Accept a) => Encode a c where
  encode :: Proxy a -> c -> Builder

instance (ToJSON c) => Encode JSON c where
  encode _ = encodeToByteStringBuilder . toJSON

instance (ToText a) => Encode PlainText a where
  encode _ = Utf8.fromText . toText

-- | (Attempts to) Decode a type from a specific Content Type.
class (Accept c) => Decode c a where
  decode :: Proxy c -> ByteString -> Either String a

instance (FromJSON a) => Decode JSON a where
  decode _ = eitherDecodeStrict

instance (FromText a) => Decode PlainText a where
  decode _ = maybe (Left "Couldn't parse: ") Right . fromText . decodeUtf8

class ToText a where
  toText :: a -> TextS.Text

instance ToText TextS.Text where
  toText = id

class FromText a where
  fromText :: TextS.Text -> Maybe a

instance FromText TextS.Text where
  fromText = Just . id
