{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}
module WebApi.ContentTypes where

import           Blaze.ByteString.Builder           (Builder)
import qualified Blaze.ByteString.Builder.Char.Utf8 as Utf8 (fromText)
import           Data.Aeson
import           Data.Aeson.Encode                  (encodeToByteStringBuilder)
import           Data.ByteString                    (ByteString)
import           Data.Proxy
import qualified Data.Text                          as TextS
import           Data.Text.Encoding                 (decodeUtf8)
import           GHC.Exts                           (Constraint)
import           Network.HTTP.Media.MediaType

data JSON
data PlainText
data OctetStream

type family All1 (ctx :: * -> Constraint) (xs :: [*]) :: Constraint where
  All1 f (x ': xs) = (f x,  All1 f xs)
  All1 f '[]       = ()

type family MimeRenderCtx ms a :: Constraint where
  MimeRenderCtx (m ': ms) a = (MimeRender m a, MimeRenderCtx ms a)
  MimeRenderCtx '[] a       = ()

class Encodings (ctypes :: [*]) a where
  encodings :: Proxy ctypes -> a -> [(MediaType, Builder)]

instance ( Accept ctype
         , MimeRender ctype a
         , Encodings ctypes a
         ) => Encodings (ctype ': ctypes) a where
  encodings _ a =  (contentType (Proxy :: Proxy ctype), mimeRender (Proxy :: Proxy ctype) a) : encodings (Proxy :: Proxy ctypes) a

instance Encodings '[] a where
  encodings _ _ = []

class Decodings (ctypes :: [*]) a where
  decodings :: Proxy ctypes -> ByteString -> [(MediaType, Either String a)]

instance ( Accept ctype
         , MimeUnRender ctype a
         , Decodings ctypes a
         ) => Decodings (ctype ': ctypes) a where
  decodings _ bs =  (contentType (Proxy :: Proxy ctype), mimeUnRender (Proxy :: Proxy ctype) bs) : decodings (Proxy :: Proxy ctypes) bs

instance Decodings '[] a where
  decodings _ _ = []

class Accept ctype where
  contentType :: Proxy ctype -> MediaType

instance Accept JSON where
  contentType _ = "application" // "json"

instance Accept PlainText where
  contentType _ = "text" // "plain" /: ("charset", "utf-8")

instance Accept OctetStream where
  contentType _ = "application" // "octet-stream"

class (Accept a) => MimeRender a c where
  mimeRender :: Proxy a -> c -> Builder

instance (ToJSON c) => MimeRender JSON c where
  mimeRender _ = encodeToByteStringBuilder . toJSON

instance (ToText a) => MimeRender PlainText a where
  mimeRender _ = Utf8.fromText . toText

class (Accept c) => MimeUnRender c a where
  mimeUnRender :: Proxy c -> ByteString -> Either String a

instance (FromJSON a) => MimeUnRender JSON a where
  mimeUnRender _ = eitherDecodeStrict

instance (FromText a) => MimeUnRender PlainText a where
  mimeUnRender _ = maybe (Left "Couldn't parse: ") Right . fromText . decodeUtf8

{-
instance MimeRender OctetStream LB.ByteString where
  mimeRender _ = fromLazyByteString

instance MimeRender OctetStream SB.ByteString where
  mimeRender _ = fromByteString
-}

class ToText a where
  toText :: a -> TextS.Text

class FromText a where
  fromText :: TextS.Text -> Maybe a
