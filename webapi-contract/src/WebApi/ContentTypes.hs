{-|
Module      : WebApi.ContentTypes
License     : BSD3
Stability   : experimental
-}

{-# LANGUAGE CPP                   #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE TupleSections         #-}

module WebApi.ContentTypes
       (
       -- * Predefined Content Types.
         JSON
       , PlainText
       , HTML
       , OctetStream
       , MultipartFormData
       , UrlEncoded

       -- * Creating custom Content Types.
       , Content
       , Accept (..)
       , Encode (..)
       , Decode (..)

       -- * Converting from and to 'Text'
       , FromText (..)
       , ToText (..)

       -- * Html
       , Html
       , html

       -- * Representation for request body and response.
       , RequestBodyType (..)
       , ResponseType (..)

       -- * Internal classes.
       , Encodings (..)
       , Decodings (..)
       , PartEncodings (..)
       , PartDecodings (..)
       , StripContents
       , StatusCode
       ) where

import           Data.Aeson (ToJSON (..), FromJSON (..), eitherDecode)
import           Data.Text.Lazy.Encoding (encodeUtf8Builder)
#if MIN_VERSION_aeson(1,0,0)
import           Data.Aeson.Encoding (fromEncoding)
#else
import           Data.Aeson.Encode (encodeToBuilder)
#endif
import qualified Data.ByteString as SB
import qualified Data.ByteString.Lazy as LBS
import           Data.ByteString.Lazy (ByteString)
import           Data.Maybe (fromMaybe)
import           Data.Proxy
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import           Data.Text.Lazy.Encoding (decodeUtf8, encodeUtf8)
import           Network.HTTP.Media.MediaType
import           Network.HTTP.Media (matchContent)
import           WebApi.Util
import           Data.ByteString.Builder (lazyByteString, Builder)
import           Data.Functor.Identity
import           Data.Kind
import           Data.Dependent.Sum
import           GHC.TypeLits
import           Type.Reflection

-- | Type representing content type of @text/html@.
data HTML

-- | Type representing content type of @application/octetstream@.
data OctetStream

-- | Type representing content type of @multipart/form-data@.
data MultipartFormData

-- | Type representing content type of @application/x-www-form-urlencoded@.
data UrlEncoded

-- | Encodings of type for all content types `ctypes`.
class Encodings (ctypes :: [*]) a where
  encodings :: Proxy ctypes -> a -> [(MediaType, Builder)]

instance ( Accept ctype
         , Encode ctype a
         , Encodings ctypes a
         ) => Encodings (ctype ': ctypes) a where
  encodings _ a =  (contentType (Proxy :: Proxy ctype), encode (Proxy :: Proxy ctype) a) : encodings (Proxy :: Proxy ctypes) a

instance Encodings '[] a where
  encodings _ _ = []

-- | Decodings of type for all content types `ctypes`.
class Decodings (ctypes :: [*]) a where
  decodings :: Proxy ctypes -> ByteString -> [(MediaType, Either String a)]

instance ( Accept ctype
         , Decode ctype a
         , Decodings ctypes a
         ) => Decodings (ctype ': ctypes) a where
  decodings _ bs =  (contentType (Proxy :: Proxy ctype), decode (Proxy :: Proxy ctype) bs) : decodings (Proxy :: Proxy ctypes) bs

instance Decodings '[] a where
  decodings _ _ = []

-- | Singleton class for content type.
class Accept ctype where
  contentType :: Proxy ctype -> MediaType

instance Accept PlainText where
  contentType _ = "text" // "plain" /: ("charset", "utf-8")

instance Accept JSON where
  contentType _ = "application" // "json"

instance Accept HTML where
  contentType _ = "text" // "html" /: ("charset", "utf-8")

instance Accept OctetStream where
  contentType _ = "application" // "octet-stream"

instance Accept MultipartFormData where
  contentType _ = "multipart" // "form-data"

instance Accept UrlEncoded where
  contentType _ = "application" // "x-www-form-urlencoded"

-- | Encode a type into a specific content type.
class (Accept a) => Encode a c where
  encode :: Proxy a -> c -> Builder

instance (ToJSON c) => Encode JSON c where
#if MIN_VERSION_aeson(1,0,0)
  encode _ = fromEncoding . toEncoding
#else
  encode _ = encodeToBuilder . toJSON
#endif

instance (ToText a) => Encode PlainText a where
  encode _ = encodeUtf8Builder . toText

-- | (Try to) Decode a type from a specific content type.
class (Accept c) => Decode c a where
  decode :: Proxy c -> ByteString -> Either String a

instance (FromJSON a) => Decode JSON a where
  decode _ = eitherDecode

instance (FromText a) => Decode PlainText a where
  decode _ = maybe (Left "Couldn't parse: ") Right . fromText . decodeUtf8

class ToText a where
  toText :: a -> LT.Text

instance ToText T.Text where
  toText = LT.fromStrict
  
instance ToText LT.Text where
  toText = id

instance ToText ByteString where
  toText = decodeUtf8

instance ToText SB.ByteString where
  toText = decodeUtf8 . LBS.fromStrict

instance ToText () where
  toText _ = ""

class FromText a where
  fromText :: LT.Text -> Maybe a

instance FromText T.Text where
  fromText = Just . LT.toStrict
  
instance FromText LT.Text where
  fromText = Just

instance FromText () where
  fromText "" = Just ()
  fromText _  = Nothing

instance FromText ByteString where
  fromText = Just . encodeUtf8

instance FromText SB.ByteString where
  fromText = Just . LBS.toStrict . encodeUtf8  

newtype Html = Html ByteString

instance Encode HTML Html where
  encode _ (Html b) = lazyByteString b

instance Decode HTML Html where
  decode _ = return . Html

html :: ByteString -> Html
html = Html

-- TODO: partEncodings & partDecodings only seem to touch request body.
class PartEncodings (xs :: [*]) where
  partEncodings :: Proxy xs
                  -> HListToRecTuple (StripContents xs)
                  -> [[(MediaType, Builder)]]

instance (PartEncodings ts, Encodings ctypes (StripContent t), MkContent t ~ Content ctypes a) => PartEncodings (t ': ts) where
  partEncodings _ (t, ts) = encodings (Proxy :: Proxy ctypes) t : partEncodings (Proxy :: Proxy ts) ts

instance PartEncodings '[] where
  partEncodings _ () = []

class PartDecodings (xs :: [RequestBodyType]) where
  partDecodings :: Proxy xs -> SB.ByteString -> ByteString -> Either String (DSum TypeRep Identity)

instance
  ( PartDecodings ts
  , Decode ct t
  , Typeable t
  ) => PartDecodings ('RequestBodyType ct t ': ts) where
  partDecodings _ clientCType partBody =
    case matchContent (pure (contentType (Proxy :: Proxy ct))) clientCType of
      Just _  -> fmap inject dec
      Nothing -> partDecodings (Proxy :: Proxy ts) clientCType partBody

    where
      dec :: Either String t
      dec = decode (Proxy :: Proxy ct) partBody

instance PartDecodings '[] where
  partDecodings _ _ _ = Left "Error 415: No Matching Content Type" -- Right ()

type family MkContent a where
  MkContent (Content ctypes a) = Content ctypes a
  MkContent a                  = Content '[JSON] a

-- | Type representing content type of @application/json@.
data JSON

-- | Type representing content type of @text/plain@.
data PlainText


type StatusCode = Nat

data RequestBodyType =
  RequestBodyType Type -- | Content type for this response.
                  Type -- | Request body type for this response.

data ResponseType =
  ResponseType Type -- | Content type for this response.
               Type -- | Output for this response.
               Type -- | Cookie for this response.
               Type -- | Header for this response.


