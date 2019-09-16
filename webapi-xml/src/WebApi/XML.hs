{-|
Module      : WebApi.XML
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
{-# LANGUAGE OverloadedStrings     #-}

module WebApi.XML (XML) where

import           Data.Text.Lazy.Encoding (encodeUtf8Builder)
-- import qualified Data.ByteString                    as SB
-- import           Data.ByteString.Lazy               (ByteString)
import           Data.Maybe                         (fromMaybe)
import           Data.Proxy
import qualified Data.Text                          as T
-- import qualified Data.Text.Lazy                     as LT
import           Data.Text.Lazy.Encoding            (decodeUtf8)
import           Network.HTTP.Media.MediaType
import           Network.HTTP.Media                 (mapContentMedia)
import           WebApi.Util
import           WebApi.Contract                    (JSON, PlainText)
import           WebApi.ContentTypes
import           Data.ByteString.Builder (lazyByteString, Builder)
import           Text.XML                           (Element, renderLBS, parseLBS, def)
-- | Type representing content type of @application/xml@.
data XML

instance Accept XML where
  contentType _ = "application" // "xml"

instance (ToXML a) => Encode XML a where
  encode _ = lazyByteString . renderLBS def . mkDoc . toXML
    where mkDoc = undefined

instance (FromXML a) => Decode XML a where
  decode _ = either (Left . show) Right . fromXML . fromDoc . parseLBS def
    where fromDoc = undefined

class ToXML a where
  toXML :: a -> Element

class FromXML a where
  fromXML :: Element -> Either T.Text a

-- class (ToXML a) => Encode a where
--   encode 

--   Define few types and try to write the ToXML and FromXML instances, from there we will try to write GToXML and GFromXML which gives us an idea on how to write the generic instances
