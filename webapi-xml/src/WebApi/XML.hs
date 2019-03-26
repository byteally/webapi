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

module WebApi.XML (XML) where

import           Data.Text.Lazy.Encoding (encodeUtf8Builder)
import           Data.Aeson                         (ToJSON (..), FromJSON (..), eitherDecode)
#if MIN_VERSION_aeson(1,0,0)
import           Data.Aeson.Encoding                (fromEncoding)
#else
import           Data.Aeson.Encode                  (encodeToBuilder)
#endif
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
import           Data.ByteString.Builder (lazyByteString, Builder)

-- | Singleton class for content type.
class Accept ctype where
    contentType :: Proxy ctype -> MediaType

-- | Type representing content type of @application/xml@.
data XML

instance Accept XML where
  contentType _ = "application" // "xml"

-- class ToXML a where
--   toXML :: a -> Element

-- class FromXML a where
--   fromXML :: Element -> Either T.Text a


-- class (ToXML a) => Encode a where
--   encode 

--   Define few types and try to write the ToXML and FromXML instances, from there we will try to write GToXML and GFromXML which gives us an idea on how to write the generic instances