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

       -- * Internal classes.
       , Encodings (..)
       , Decodings (..)
       , PartEncodings (..)
       , PartDecodings (..)
       , StripContents
       ) where

import           Data.Text.Lazy.Encoding (encodeUtf8Builder)
import           Data.Aeson                         (ToJSON (..), FromJSON (..), eitherDecode)
#if MIN_VERSION_aeson(1,0,0)
import           Data.Aeson.Encoding                (fromEncoding)
#else
import           Data.Aeson.Encode                  (encodeToBuilder)
#endif
import qualified Data.ByteString                    as SB
import           Data.ByteString.Lazy               (ByteString)
import           Data.Maybe                         (fromMaybe)
import           Data.Proxy
import qualified Data.Text                          as T
import qualified Data.Text.Lazy                     as LT
import           Data.Text.Lazy.Encoding            (decodeUtf8)
import           Network.HTTP.Media.MediaType
import           Network.HTTP.Media                 (mapContentMedia)
import           WebApi.Util
import           WebApi.Contract                    (JSON, PlainText)
import           Data.ByteString.Builder (lazyByteString, Builder)


-- | Type representing content type of @text/html@.
data HTML

-- | Type representing content type of @application/octetstream@.
data OctetStream

-- | Type representing content type of @multipart/form-data@.
data MultipartFormData

-- | Type representing content type of @application/x-www-form-urlencoded@.
data UrlEncoded

-- | Type representing content type of @application/xml@.
data XML

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

instance Accept JSON where
  contentType _ = "application" // "json"

instance Accept PlainText where
  contentType _ = "text" // "plain" /: ("charset", "utf-8")

instance Accept HTML where
  contentType _ = "text" // "html" /: ("charset", "utf-8")

instance Accept OctetStream where
  contentType _ = "application" // "octet-stream"

instance Accept MultipartFormData where
  contentType _ = "multipart" // "form-data"

instance Accept UrlEncoded where
  contentType _ = "application" // "x-www-form-urlencoded"

instance Accept XML where
  contentType _ = "application" // "xml"

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

newtype Html = Html ByteString

instance Encode HTML Html where
  encode _ (Html b) = lazyByteString b

instance Decode HTML Html where
  decode _ = return . Html

html :: ByteString -> Html
html = Html

class PartEncodings (xs :: [*]) where
  partEncodings :: Proxy xs
                  -> HListToRecTuple (StripContents xs)
                  -> [[(MediaType, Builder)]]

instance (PartEncodings ts, Encodings ctypes (StripContent t), MkContent t ~ Content ctypes a) => PartEncodings (t ': ts) where
  partEncodings _ (t, ts) = encodings (Proxy :: Proxy ctypes) t : partEncodings (Proxy :: Proxy ts) ts

instance PartEncodings '[] where
  partEncodings _ () = []

class PartDecodings (xs :: [*]) where
  partDecodings :: Proxy xs -> [(SB.ByteString, ByteString)] -> Either String (HListToRecTuple (StripContents xs))

instance (PartDecodings ts, Decodings ctypes (StripContent t), MkContent t ~ Content ctypes a) => PartDecodings (t ': ts) where
  partDecodings _ ((ctype, partBody) : xs) = do
    let decs = decodings (Proxy :: Proxy ctypes) partBody
        (decValE :: Maybe (Either String (StripContent t))) = mapContentMedia decs ctype
    decVal <- fromMaybe (Left "Error 415: No Matching Content Type") decValE
    (decVal, ) <$> partDecodings (Proxy :: Proxy ts) xs
  partDecodings _ [] = error "Error!: This shouldn't have happened"

instance PartDecodings '[] where
  partDecodings _ _ = Right ()

type family MkContent a where
  MkContent (Content ctypes a) = Content ctypes a
  MkContent a                  = Content '[JSON] a
