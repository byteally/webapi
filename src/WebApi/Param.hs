{-|
Module      : WebApi.Param
License     : BSD3
Stability   : experimental

Param serialization and deserialization. 'ToParam' and 'EncodeParam' are responsible for serialization part.
'EncodeParam' converts the value into a wire format. 'ToParam' is responsible for creating (nested) key value pairs, which can be then used to deserialize to original type. For example

@
encodeParam 5 == "5"

data Foo = Foo { foo :: Int }
         deriving (Show, Eq, Generic)

data Bar = Bar { bar :: Foo }
         deriving (Show, Eq, Generic)

instance ToParam Foo 'FormParam
instance ToParam Bar 'FormParam

toParam (Proxy :: Proxy 'FormParam) "" (Bar (Foo 5)) == [("bar.foo","5")]
@

Deserialization works analogously, 'FromParam' and 'DecodeParam' are counterparts to 'ToParam' and 'EncodeParam' respectively. Generic instances are provided for all of them. This means that the user only need to derive Generic in their type, and provide instance with an empty body. Note that for headers 'FromHeader' and 'ToHeader' is being used in place of 'FromParam' and 'ToParam'. Nesting is not supported for headers.
-}

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DefaultSignatures     #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}
module WebApi.Param
       ( -- * Serialization
         ToParam (..)
       , EncodeParam (..)
       , ToHeader (..)
       , SerializedData
       , toQueryParam
       , toFormParam
       , toFileParam
       , toPathParam
       , toHeader
       , toCookie
       , toNonNestedParam

       -- * Deserialization
       , FromParam (..)
       , DecodeParam (..)
       , FromHeader (..)
       , Validation (..)
       , ParamErr (..)
       , ParamErrToApiErr (..)
       , DeSerializedData
       , fromQueryParam
       , fromFormParam
       , fromFileParam
       , fromHeader
       , fromCookie
       , lookupParam
       , fromNonNestedParam

       -- * Wrappers
       , JsonOf (..)
       , OptValue (..)
       , FileInfo (..)
       , NonNested (..)

       -- * Helpers  
       , ParamK (..)
       , filePath  
       , nest
       ) where


import           Blaze.ByteString.Builder           (toByteString)
import           Blaze.ByteString.Builder.Char.Utf8 (fromChar)
import           Data.Aeson                         (FromJSON (..), ToJSON (..))
import qualified Data.Aeson                         as A
import           Data.ByteString                    as SB hiding (index, isPrefixOf)
import qualified Data.ByteString                    as SB (isPrefixOf)
import           Data.ByteString.Builder            (byteString, char7,
                                                     toLazyByteString)
import           Data.ByteString.Char8              as ASCII (pack, readInteger,
                                                              split, unpack)
import           Data.ByteString.Lazy               (toStrict)
import qualified Data.ByteString.Lex.Fractional     as LexF
import           Data.ByteString.Lex.Integral
import           Data.CaseInsensitive               as CI
import           Data.Foldable                      as Fold (foldl')
import           Data.Int
import qualified Data.List                          as L (find)          
import           Data.Monoid                        ((<>))
import           Data.Proxy
import qualified Data.Text                          as T (Text, pack, uncons)
import           Data.Text.Encoding                 (decodeUtf8', encodeUtf8)
import           Data.Time.Calendar                 (Day)
import           Data.Time.Clock                    (UTCTime)
import           Data.Time.Format                   (FormatTime,
                                                     defaultTimeLocale,
                                                     formatTime, parseTimeM)
import           Data.Trie                          as Trie
import           Data.Typeable
import           Data.Vector                        (Vector)
import qualified Data.Vector                        as V
import           Data.Word
import           GHC.Generics
import           Network.HTTP.Types
import           Network.HTTP.Types                 as Http (Header, QueryItem)
import qualified Network.Wai.Parse                  as Wai (FileInfo (..))

-- | A type for holding a file. 
newtype FileInfo = FileInfo { fileInfo :: Wai.FileInfo FilePath }
                 deriving (Eq, Show)

-- | Obtain the file path from 'FileInfo'.
filePath :: FileInfo -> FilePath
filePath = Wai.fileContent . fileInfo

-- | (Kind) Describes the various types of Param.
data ParamK = QueryParam
            | FormParam
            | FileParam
            | PathParam
            | Cookie

-- | Use this type if a key is required but the value is optional.
newtype OptValue a = OptValue { toMaybe :: Maybe a}
                   deriving (Show, Read, Eq, Ord)

-- | Serializing 'JsonOf' will produce a JSON representation of the value contained within. This is useful if params has to be sent as JSON.  
newtype JsonOf a = JsonOf {getValue :: a}
                    deriving (Show, Read, Eq, Ord)

data Unit = Unit
          deriving (Show, Eq)

instance ToJSON a => ToJSON (JsonOf a) where
  toJSON (JsonOf a) = toJSON a

instance FromJSON a => FromJSON (JsonOf a) where
  parseJSON jval = JsonOf `fmap` parseJSON jval

-- | Define result of serialization of a type of kind 'ParamK'.
type family SerializedData (par :: ParamK) where
  SerializedData 'QueryParam = Http.QueryItem
  SerializedData 'FormParam  = (ByteString, ByteString)
  SerializedData 'FileParam  = (ByteString, Wai.FileInfo FilePath)
  SerializedData 'PathParam  = ByteString
  SerializedData 'Cookie     = (ByteString, ByteString)

-- | Define result of deserialization of a type of kind 'ParamK'.
type family DeSerializedData (par :: ParamK) where
  DeSerializedData 'QueryParam = Maybe ByteString
  DeSerializedData 'FormParam  = ByteString
  DeSerializedData 'FileParam  = Wai.FileInfo FilePath
  DeSerializedData 'Cookie     = ByteString

-- | Datatype representing the parsed result of params.  
newtype Validation e a = Validation { getValidation :: Either e a }
                       deriving (Eq, Functor, Show)

instance Monoid e => Applicative (Validation e) where
  pure = Validation . Right
  Validation a <*> Validation b = Validation $
    case a of
      Right va -> fmap va b
      Left ea -> either (Left . mappend ea) (const $ Left ea) b

-- | Serialize a type into QueryParams.
toQueryParam :: (ToParam a 'QueryParam) => a -> Query
toQueryParam = toParam (Proxy :: Proxy 'QueryParam) ""

-- | Serialize a type into FormParams.                                 
toFormParam :: (ToParam a 'FormParam) => a -> [(ByteString, ByteString)]
toFormParam = toParam (Proxy :: Proxy 'FormParam) ""

-- | Serialize a type into FormParams.                                
toFileParam :: (ToParam a 'FileParam) => a -> [(ByteString, Wai.FileInfo FilePath)]
toFileParam = toParam (Proxy :: Proxy 'FileParam) ""

-- | Serialize a type into PathParams.                                
toPathParam :: (ToParam a 'PathParam) => a -> [ByteString]
toPathParam = toParam (Proxy :: Proxy 'PathParam) ""

-- | Serialize a type into Header.                                
toHeader :: (ToHeader a) => a -> RequestHeaders
toHeader = toHeader'

-- | Serialize a type into Cookie.
toCookie :: (ToParam a 'Cookie) => a -> [(ByteString, ByteString)]
toCookie = toParam (Proxy :: Proxy 'Cookie) ""

-- | (Try to) Deserialize a type from QueryParams.                             
fromQueryParam :: (FromParam a 'QueryParam) => Query -> Validation [ParamErr] a
fromQueryParam par = fromParam (Proxy :: Proxy 'QueryParam) "" $ Trie.fromList par

-- | (Try to) Deserialize a type from FormParams.
fromFormParam :: (FromParam a 'FormParam) => [(ByteString, ByteString)] -> Validation [ParamErr] a
fromFormParam par = fromParam (Proxy :: Proxy 'FormParam) "" $ Trie.fromList par

-- | (Try to) Deserialize a type from FileParams.                                        
fromFileParam :: (FromParam a 'FileParam) => [(ByteString, Wai.FileInfo FilePath)] -> Validation [ParamErr] a
fromFileParam par = fromParam (Proxy :: Proxy 'FileParam) "" $ Trie.fromList par

-- | (Try to) Deserialize a type from Headers.                                       
fromHeader :: (FromHeader a) => [Http.Header] -> Validation [ParamErr] a
fromHeader = fromHeader'

-- | (Try to) Deserialize a type from Cookie.
fromCookie :: (FromParam a 'Cookie) => [(ByteString, ByteString)] -> Validation [ParamErr] a
fromCookie par = fromParam (Proxy :: Proxy 'Cookie) "" $ Trie.fromList par

-- | Serialize a type to a given type of kind 'ParamK'.
class ToParam a (parK :: ParamK) where
  toParam :: Proxy (parK :: ParamK) -> ByteString -> a -> [SerializedData parK]

  default toParam :: (Generic a, GToParam (Rep a) parK) => Proxy (parK :: ParamK) -> ByteString -> a -> [SerializedData parK]
  toParam pt pfx = gtoParam pt pfx (ParamAcc 0 False) ParamSettings . from

-- | (Try to) Deserialize a type from a given type of kind 'ParamK'.
class FromParam a (parK :: ParamK) where
  fromParam :: Proxy (parK :: ParamK) -> ByteString -> Trie (DeSerializedData parK) -> Validation [ParamErr] a

  default fromParam :: (Generic a, GFromParam (Rep a) parK) => Proxy (parK :: ParamK) -> ByteString -> Trie (DeSerializedData parK) -> Validation [ParamErr] a
  fromParam pt pfx = (fmap to) . gfromParam pt pfx (ParamAcc 0 False) ParamSettings

-- | Serialize a type to ByteString.
class EncodeParam (t :: *) where
  encodeParam :: t -> ByteString

  default encodeParam :: (Generic t, GHttpParam (Rep t)) => t -> ByteString
  encodeParam = gEncodeParam . from

-- | (Try to) Deserialize a type from ByteString.
class DecodeParam (t :: *) where
  decodeParam :: ByteString -> Maybe t

  default decodeParam :: (Generic t, GHttpParam (Rep t)) => ByteString -> Maybe t
  decodeParam = (fmap to) . gDecodeParam

instance EncodeParam ByteString where
  encodeParam   = id

instance DecodeParam ByteString where
  decodeParam = Just

instance EncodeParam Int where
  encodeParam i = ASCII.pack $ show i

instance DecodeParam Int where
  decodeParam str = case readSigned readDecimal str of
    Just (v, "") -> Just v
    _            -> Nothing

instance EncodeParam Int8 where
  encodeParam i = ASCII.pack $ show i

instance DecodeParam Int8 where
  decodeParam str = case readSigned readDecimal str of
    Just (v, "") -> Just v
    _            -> Nothing

instance EncodeParam Int16 where
  encodeParam i = ASCII.pack $ show i

instance DecodeParam Int16 where
  decodeParam str = case readSigned readDecimal str of
    Just (v, "") -> Just v
    _            -> Nothing

instance EncodeParam Int32 where
  encodeParam i = ASCII.pack $ show i

instance DecodeParam Int32 where
  decodeParam str = case readSigned readDecimal str of
    Just (v, "") -> Just v
    _            -> Nothing

instance EncodeParam Int64 where
  encodeParam i = ASCII.pack $ show i

instance DecodeParam Int64 where
  decodeParam str = case readSigned readDecimal str of
    Just (v, "") -> Just v
    _            -> Nothing

instance EncodeParam Word where
  encodeParam i = ASCII.pack $ show i

instance DecodeParam Word where
  decodeParam str = case readDecimal str of
    Just (v, "") -> Just v
    _            -> Nothing

instance EncodeParam Word8 where
  encodeParam i = ASCII.pack $ show i

instance DecodeParam Word8 where
  decodeParam str = case readDecimal str of
    Just (v, "") -> Just v
    _            -> Nothing

instance EncodeParam Word16 where
  encodeParam i = ASCII.pack $ show i

instance DecodeParam Word16 where
  decodeParam str = case readDecimal str of
    Just (v, "") -> Just v
    _            -> Nothing

instance EncodeParam Word32 where
  encodeParam i = ASCII.pack $ show i

instance DecodeParam Word32 where
  decodeParam str = case readDecimal str of
    Just (v, "") -> Just v
    _            -> Nothing

instance EncodeParam Word64 where
  encodeParam i = ASCII.pack $ show i

instance DecodeParam Word64 where
  decodeParam str = case readDecimal str of
    Just (v, "") -> Just v
    _            -> Nothing

instance EncodeParam Float where
  encodeParam d = ASCII.pack $ show d

instance DecodeParam Float where
  decodeParam str = case readSigned LexF.readExponential str of
    Just (v, "") -> Just v
    _            -> Nothing

instance EncodeParam Double where
  encodeParam d = ASCII.pack $ show d

instance DecodeParam Double where
  decodeParam str = case readSigned LexF.readExponential str of
    Just (v, "") -> Just v
    _            -> Nothing

instance EncodeParam Char where
  encodeParam       = toByteString . fromChar

instance DecodeParam Char where
  decodeParam str = case decodeUtf8' str of
    Right txt -> maybe Nothing (Just . fst) (T.uncons txt)
    Left _    -> Nothing

instance EncodeParam T.Text where
  encodeParam = encodeUtf8

instance DecodeParam T.Text where
  decodeParam str = case decodeUtf8' str of
    Right txt -> Just txt
    Left _    -> Nothing

instance EncodeParam Day where
  encodeParam day = ASCII.pack $ show day

instance DecodeParam Day where
  decodeParam str = case reads $ ASCII.unpack str of
    [(a,"")] -> Just a
    _        -> Nothing

instance EncodeParam UTCTime where
  encodeParam t = ASCII.pack $ formatTime defaultTimeLocale format t
    where
      format = "%FT%T." ++ formatSubseconds t ++ "Z"

instance DecodeParam UTCTime where
  decodeParam str = case parseTimeM True defaultTimeLocale "%FT%T%QZ" (ASCII.unpack str) of
    Just d -> Just d
    _      -> Nothing

formatSubseconds :: (FormatTime t) => t -> String
formatSubseconds = formatTime defaultTimeLocale "%q"

instance EncodeParam Unit where
  encodeParam _ = "()"

instance DecodeParam Unit where
  decodeParam str = case str of
    "()" -> Just Unit
    _    -> Nothing

instance (EncodeParam a, EncodeParam b) => EncodeParam (a,b) where
  encodeParam (a,b) = toStrict $ toLazyByteString $ byteString (encodeParam a)
                                                  <> char7 ','
                                                  <> byteString (encodeParam b)

instance (DecodeParam a, DecodeParam b) => DecodeParam (a,b) where
  decodeParam str = case ASCII.split ',' str of
    [str1, str2] -> (,) <$> decodeParam str1 <*> decodeParam str2
    _            -> Nothing

instance EncodeParam Bool where
  encodeParam i = ASCII.pack $ show i

instance DecodeParam Bool where
  decodeParam str | str == "True"  = Just True
  decodeParam str | str == "False" = Just False
                    | otherwise     = Nothing

instance EncodeParam Integer where
  encodeParam i = ASCII.pack $ show i

instance DecodeParam Integer where
  decodeParam str = case ASCII.readInteger str of
    Just (i, "") -> Just i
    _            -> Nothing

instance (ToJSON a) => EncodeParam (JsonOf a) where
  encodeParam (JsonOf a) = toStrict $ A.encode a

instance (FromJSON a) => DecodeParam (JsonOf a) where
  decodeParam str = A.decodeStrict' str

class GHttpParam f where
  gEncodeParam   :: f a -> ByteString
  gDecodeParam :: ByteString -> Maybe (f a)

instance (GHttpParam f) => GHttpParam (D1 c f) where
  gEncodeParam (M1 c) = gEncodeParam c
  gDecodeParam str  = M1 <$> (gDecodeParam str)

instance (GHttpParam f, GHttpParam g) => GHttpParam (f :+: g) where
  gEncodeParam (L1 l) = gEncodeParam l
  gEncodeParam (R1 r) = gEncodeParam r
  gDecodeParam str  = case L1 <$> gDecodeParam str of
    l1@(Just _) -> l1
    _           -> R1 <$> gDecodeParam str

instance (GHttpParam f, Constructor c) => GHttpParam (C1 c f) where
  gEncodeParam con@(M1 c) = const (ASCII.pack $ conName con) $ gEncodeParam c
  gDecodeParam str = if (ASCII.pack $ conName (undefined :: (C1 c f) a)) == str
                       then M1 <$> gDecodeParam str
                       else Nothing

instance GHttpParam U1 where
  gEncodeParam U1    = error "Panic! Unreacheable code @ GHttpParam U1"
  gDecodeParam _   = Just U1

-- | Use this type if for serialization / deserialization nesting is not required. The type contained within most likely requires 'EncodeParam' / 'DecodeParam'.
newtype NonNested a = NonNested { getNonNestedParam :: a }
                    deriving (Show, Eq, Read)

-- | Serialize a type without nesting
toNonNestedParam :: (ToParam (NonNested a) parK, EncodeParam a) => Proxy (parK :: ParamK) -> ByteString -> a -> [SerializedData parK]
toNonNestedParam par pfx a = toParam par pfx (NonNested a)

-- | (Try to) Deserialize a type without nesting
fromNonNestedParam :: (FromParam (NonNested a) parK, DecodeParam a) => Proxy (parK :: ParamK) -> ByteString -> Trie (DeSerializedData parK) -> Validation [ParamErr] a
fromNonNestedParam par pfx kvs = getNonNestedParam <$> fromParam par pfx kvs

instance (EncodeParam a) => ToParam (NonNested a) 'QueryParam where
  toParam _ pfx (NonNested val) = [(pfx, Just $ encodeParam val)]

instance (EncodeParam a) => ToParam (NonNested a) 'FormParam where
  toParam _ pfx (NonNested val) = [(pfx, encodeParam val)]

instance (EncodeParam a) => ToParam (NonNested a) 'Cookie where
  toParam _ pfx (NonNested val) = [(pfx, encodeParam val)]

instance (DecodeParam a, Typeable a) => FromParam (NonNested a) 'QueryParam where
  fromParam pt key kvs = case lookupParam pt key kvs of
   Just (Just par) -> case decodeParam par of
         Just v -> Validation $ Right $ NonNested v
         _      -> Validation $ Left [ParseErr key $ T.pack $ "Unable to cast to " ++ (show $ typeOf (Proxy :: Proxy a))]
   _ ->  Validation $ Left [NotFound key]

instance (DecodeParam a, Typeable a) => FromParam (NonNested a) 'FormParam where
  fromParam pt key kvs = case lookupParam pt key kvs of
   Just par -> case decodeParam par of
         Just v -> Validation $ Right $ NonNested v
         _      -> Validation $ Left [ParseErr key $ T.pack $ "Unable to cast to " ++ (show $ typeOf (Proxy :: Proxy a))]
   _ ->  Validation $ Left [NotFound key]

instance (DecodeParam a, Typeable a) => FromParam (NonNested a) 'Cookie where
  fromParam pt key kvs = case lookupParam pt key kvs of
   Just par -> case decodeParam par of
         Just v -> Validation $ Right $ NonNested v
         _      -> Validation $ Left [ParseErr key $ T.pack $ "Unable to cast to " ++ (show $ typeOf (Proxy :: Proxy a))]
   _ ->  Validation $ Left [NotFound key]

instance ToParam () parK where
  toParam _ _ _ = []

instance ToHeader () where
  toHeader' _ = []

instance ToParam Unit 'QueryParam where
  toParam _ pfx val = [(pfx, Just $ encodeParam val)]

instance ToParam Unit 'FormParam where
  toParam _ pfx val = [(pfx, encodeParam val)]

instance ToParam Unit 'Cookie where
  toParam _ pfx val = [(pfx, encodeParam val)]

instance ToParam Int 'QueryParam where
  toParam _ pfx val = [(pfx, Just $ encodeParam val)]

instance ToParam Int 'FormParam where
  toParam _ pfx val = [(pfx, encodeParam val)]

instance ToParam Int 'Cookie where
  toParam _ pfx val = [(pfx, encodeParam val)]

instance ToParam Int8 'QueryParam where
  toParam _ pfx val = [(pfx, Just $ encodeParam val)]

instance ToParam Int8 'FormParam where
  toParam _ pfx val = [(pfx, encodeParam val)]

instance ToParam Int8 'Cookie where
  toParam _ pfx val = [(pfx, encodeParam val)]

instance ToParam Int16 'QueryParam where
  toParam _ pfx val = [(pfx, Just $ encodeParam val)]

instance ToParam Int16 'FormParam where
  toParam _ pfx val = [(pfx, encodeParam val)]

instance ToParam Int16 'Cookie where
  toParam _ pfx val = [(pfx, encodeParam val)]

instance ToParam Int32 'QueryParam where
  toParam _ pfx val = [(pfx, Just $ encodeParam val)]

instance ToParam Int32 'FormParam where
  toParam _ pfx val = [(pfx, encodeParam val)]

instance ToParam Int32 'Cookie where
  toParam _ pfx val = [(pfx, encodeParam val)]

instance ToParam Int64 'QueryParam where
  toParam _ pfx val = [(pfx, Just $ encodeParam val)]

instance ToParam Int64 'FormParam where
  toParam _ pfx val = [(pfx, encodeParam val)]

instance ToParam Int64 'Cookie where
  toParam _ pfx val = [(pfx, encodeParam val)]

instance ToParam Word 'QueryParam where
  toParam _ pfx val = [(pfx, Just $ encodeParam val)]

instance ToParam Word 'FormParam where
  toParam _ pfx val = [(pfx, encodeParam val)]

instance ToParam Word 'Cookie where
  toParam _ pfx val = [(pfx, encodeParam val)]

instance ToParam Word8 'QueryParam where
  toParam _ pfx val = [(pfx, Just $ encodeParam val)]

instance ToParam Word8 'FormParam where
  toParam _ pfx val = [(pfx, encodeParam val)]

instance ToParam Word8 'Cookie where
  toParam _ pfx val = [(pfx, encodeParam val)]

instance ToParam Word16 'QueryParam where
  toParam _ pfx val = [(pfx, Just $ encodeParam val)]

instance ToParam Word16 'FormParam where
  toParam _ pfx val = [(pfx, encodeParam val)]

instance ToParam Word16 'Cookie where
  toParam _ pfx val = [(pfx, encodeParam val)]

instance ToParam Word32 'QueryParam where
  toParam _ pfx val = [(pfx, Just $ encodeParam val)]

instance ToParam Word32 'FormParam where
  toParam _ pfx val = [(pfx, encodeParam val)]

instance ToParam Word32 'Cookie where
  toParam _ pfx val = [(pfx, encodeParam val)]

instance ToParam Word64 'QueryParam where
  toParam _ pfx val = [(pfx, Just $ encodeParam val)]

instance ToParam Word64 'FormParam where
  toParam _ pfx val = [(pfx, encodeParam val)]

instance ToParam Word64 'Cookie where
  toParam _ pfx val = [(pfx, encodeParam val)]

instance ToParam Integer 'QueryParam where
  toParam _ pfx val = [(pfx, Just $ encodeParam val)]

instance ToParam Integer 'FormParam where
  toParam _ pfx val = [(pfx, encodeParam val)]

instance ToParam Integer 'Cookie where
  toParam _ pfx val = [(pfx, encodeParam val)]

instance ToParam Bool 'QueryParam where
  toParam _ pfx val = [(pfx, Just $ encodeParam val)]

instance ToParam Bool 'FormParam where
  toParam _ pfx val = [(pfx, encodeParam val)]

instance ToParam Bool 'Cookie where
  toParam _ pfx val = [(pfx, encodeParam val)]

instance ToParam Double 'QueryParam where
  toParam _ pfx val = [(pfx, Just $ encodeParam val)]

instance ToParam Double 'FormParam where
  toParam _ pfx val = [(pfx, encodeParam val)]

instance ToParam Double 'Cookie where
  toParam _ pfx val = [(pfx, encodeParam val)]

instance ToParam Float 'QueryParam where
  toParam _ pfx val = [(pfx, Just $ encodeParam val)]

instance ToParam Float 'FormParam where
  toParam _ pfx val = [(pfx, encodeParam val)]

instance ToParam Float 'Cookie where
  toParam _ pfx val = [(pfx, encodeParam val)]

instance ToParam Char 'QueryParam where
  toParam _ pfx val = [(pfx, Just $ encodeParam val)]

instance ToParam Char 'FormParam where
  toParam _ pfx val = [(pfx, encodeParam val)]

instance ToParam Char 'Cookie where
  toParam _ pfx val = [(pfx, encodeParam val)]

instance ToParam T.Text 'QueryParam where
  toParam _ pfx val = [(pfx, Just $ encodeParam val)]

instance ToParam T.Text 'FormParam where
  toParam _ pfx val = [(pfx, encodeParam val)]

instance ToParam T.Text 'Cookie where
  toParam _ pfx val = [(pfx, encodeParam val)]

instance ToParam ByteString 'QueryParam where
  toParam _ pfx val = [(pfx, Just $ val)]

instance ToParam ByteString 'FormParam where
  toParam _ pfx val = [(pfx, val)]

instance ToParam ByteString 'Cookie where
  toParam _ pfx val = [(pfx, val)]

instance ToParam Day 'QueryParam where
  toParam _ pfx val = [(pfx, Just $ encodeParam val)]

instance ToParam Day 'FormParam where
  toParam _ pfx val = [(pfx, encodeParam val)]

instance ToParam Day 'Cookie where
  toParam _ pfx val = [(pfx, encodeParam val)]

instance ToParam UTCTime 'QueryParam where
  toParam _ pfx val = [(pfx, Just $ encodeParam val)]

instance ToParam UTCTime 'FormParam where
  toParam _ pfx val = [(pfx, encodeParam val)]

instance ToParam UTCTime 'Cookie where
  toParam _ pfx val = [(pfx, encodeParam val)]

instance (EncodeParam a) => ToParam (OptValue a) 'QueryParam where
  toParam _ pfx (OptValue (Just val)) = [(pfx, Just $ encodeParam val)]
  toParam _ pfx (OptValue Nothing)    = [(pfx, Nothing)]

instance (EncodeParam a) => ToParam (OptValue a) 'FormParam where
  toParam _ pfx (OptValue (Just val)) = [(pfx, encodeParam val)]
  toParam _ _ (OptValue Nothing)     = []

instance (EncodeParam a) => ToParam (OptValue a) 'Cookie where
  toParam _ pfx (OptValue (Just val)) = [(pfx, encodeParam val)]
  toParam _ _ (OptValue Nothing)     = []

instance (ToJSON a, FromJSON a) => ToParam (JsonOf a) 'QueryParam where
  toParam _ pfx val = [(pfx, Just $ encodeParam val)]

instance (ToJSON a, FromJSON a) => ToParam (JsonOf a) 'FormParam where
  toParam _ pfx val = [(pfx, encodeParam val)]

instance (ToJSON a, FromJSON a) => ToParam (JsonOf a) 'Cookie where
  toParam _ pfx val = [(pfx, encodeParam val)]

instance ToParam a par => ToParam (Maybe a) par where
  toParam pt pfx (Just val) = toParam pt pfx val
  toParam _ _ Nothing      = []

instance (ToParam a par, ToParam b par) => ToParam (Either a b) par where
  toParam pt pfx (Left e)  = toParam pt (pfx `nest` "Left") e
  toParam pt pfx (Right v) = toParam pt (pfx `nest` "Right") v

instance ToParam a par => ToParam [a] par where
  toParam pt pfx vals = Prelude.concatMap (\(ix, v) -> toParam pt (pfx `nest` (ASCII.pack $ show ix)) v) $ Prelude.zip [(0 :: Word)..] vals

instance ToParam a par => ToParam (Vector a) par where
  toParam pt pfx vals = toParam pt pfx (V.toList vals)

instance FromParam () parK where
  fromParam _ _ _ = pure ()

instance FromHeader () where
  fromHeader' _ = pure ()

instance FromParam Unit 'QueryParam where
  fromParam pt key kvs = case lookupParam pt key kvs of
   Just (Just par) -> case decodeParam par of
         Just v -> Validation $ Right v
         _      -> Validation $ Left [ParseErr key "Unable to cast to NullaryConstructor"]
   _ ->  Validation $ Left [NotFound key]

instance FromParam Unit 'FormParam where
  fromParam pt key kvs = case lookupParam pt key kvs of
   Just par -> case decodeParam par of
         Just v -> Validation $ Right v
         _      -> Validation $ Left [ParseErr key "Unable to cast to NullaryConstructor"]
   _ ->  Validation $ Left [NotFound key]

instance FromParam Unit 'Cookie where
  fromParam pt key kvs = case lookupParam pt key kvs of
   Just par -> case decodeParam par of
         Just v -> Validation $ Right v
         _      -> Validation $ Left [ParseErr key "Unable to cast to NullaryConstructor"]
   _ ->  Validation $ Left [NotFound key]

instance FromParam Bool 'QueryParam where
  fromParam pt key kvs = case lookupParam pt key kvs of
   Just (Just par) -> case decodeParam par of
         Just v -> Validation $ Right v
         _      -> Validation $ Left [ParseErr key "Unable to cast to Bool"]
   _ ->  Validation $ Left [NotFound key]

instance FromParam Bool 'FormParam where
  fromParam pt key kvs = case lookupParam pt key kvs of
   Just par -> case decodeParam par of
         Just v -> Validation $ Right v
         _      -> Validation $ Left [ParseErr key "Unable to cast to Bool"]
   _ ->  Validation $ Left [NotFound key]

instance FromParam Bool 'Cookie where
  fromParam pt key kvs = case lookupParam pt key kvs of
   Just par -> case decodeParam par of
         Just v -> Validation $ Right v
         _      -> Validation $ Left [ParseErr key "Unable to cast to Bool"]
   _ ->  Validation $ Left [NotFound key]

instance FromParam Char 'QueryParam where
  fromParam pt key kvs = case lookupParam pt key kvs of
   Just (Just par) -> case decodeParam par of
         Just v -> Validation $ Right v
         _      -> Validation $ Left [ParseErr key "Unable to cast to Char"]
   _ ->  Validation $ Left [NotFound key]

instance FromParam Char 'FormParam where
  fromParam pt key kvs = case lookupParam pt key kvs of
   Just par -> case decodeParam par of
         Just v -> Validation $ Right v
         _      -> Validation $ Left [ParseErr key "Unable to cast to Char"]
   _ ->  Validation $ Left [NotFound key]

instance FromParam Char 'Cookie where
  fromParam pt key kvs = case lookupParam pt key kvs of
   Just par -> case decodeParam par of
         Just v -> Validation $ Right v
         _      -> Validation $ Left [ParseErr key "Unable to cast to Char"]
   _ ->  Validation $ Left [NotFound key]

instance FromParam UTCTime 'QueryParam where
  fromParam pt key kvs = case lookupParam pt key kvs of
   Just (Just par) -> case decodeParam par of
         Just v -> Validation $ Right v
         _      -> Validation $ Left [ParseErr key "Unable to cast to UTCTime (ISO-8601)"]
   _ ->  Validation $ Left [NotFound key]

instance FromParam UTCTime 'FormParam where
  fromParam pt key kvs = case lookupParam pt key kvs of
   Just par -> case decodeParam par of
         Just v -> Validation $ Right v
         _      -> Validation $ Left [ParseErr key "Unable to cast to UTCTime (ISO-8601)"]
   _ ->  Validation $ Left [NotFound key]

instance FromParam UTCTime 'Cookie where
  fromParam pt key kvs = case lookupParam pt key kvs of
   Just par -> case decodeParam par of
         Just v -> Validation $ Right v
         _      -> Validation $ Left [ParseErr key "Unable to cast to UTCTime (ISO-8601)"]
   _ ->  Validation $ Left [NotFound key]

instance FromParam Int 'QueryParam where
  fromParam pt key kvs = case lookupParam pt key kvs of
   Just (Just par) -> case decodeParam par of
     Just v -> Validation $ Right v
     _      -> Validation $ Left [ParseErr key "Unable to cast to Int"]
   _ ->  Validation $ Left [NotFound key]

instance FromParam Int 'FormParam where
  fromParam pt key kvs = case lookupParam pt key kvs of
   Just par -> case decodeParam par of
     Just v -> Validation $ Right v
     _      -> Validation $ Left [ParseErr key "Unable to cast to Int"]
   _ ->  Validation $ Left [NotFound key]

instance FromParam Int 'Cookie where
  fromParam pt key kvs = case lookupParam pt key kvs of
   Just par -> case decodeParam par of
     Just v -> Validation $ Right v
     _      -> Validation $ Left [ParseErr key "Unable to cast to Int"]
   _ ->  Validation $ Left [NotFound key]

instance FromParam Int8 'QueryParam where
  fromParam pt key kvs = case lookupParam pt key kvs of
   Just (Just par) -> case decodeParam par of
     Just v -> Validation $ Right v
     _      -> Validation $ Left [ParseErr key "Unable to cast to Int8"]
   _ ->  Validation $ Left [NotFound key]

instance FromParam Int8 'FormParam where
  fromParam pt key kvs = case lookupParam pt key kvs of
   Just par -> case decodeParam par of
     Just v -> Validation $ Right v
     _      -> Validation $ Left [ParseErr key "Unable to cast to Int8"]
   _ ->  Validation $ Left [NotFound key]

instance FromParam Int8 'Cookie where
  fromParam pt key kvs = case lookupParam pt key kvs of
   Just par -> case decodeParam par of
     Just v -> Validation $ Right v
     _      -> Validation $ Left [ParseErr key "Unable to cast to Int8"]
   _ ->  Validation $ Left [NotFound key]

instance FromParam Int16 'QueryParam where
  fromParam pt key kvs = case lookupParam pt key kvs of
   Just (Just par) -> case decodeParam par of
     Just v -> Validation $ Right v
     _      -> Validation $ Left [ParseErr key "Unable to cast to Int16"]
   _ ->  Validation $ Left [NotFound key]

instance FromParam Int16 'FormParam where
  fromParam pt key kvs = case lookupParam pt key kvs of
   Just par -> case decodeParam par of
     Just v -> Validation $ Right v
     _      -> Validation $ Left [ParseErr key "Unable to cast to Int16"]
   _ ->  Validation $ Left [NotFound key]

instance FromParam Int16 'Cookie where
  fromParam pt key kvs = case lookupParam pt key kvs of
   Just par -> case decodeParam par of
     Just v -> Validation $ Right v
     _      -> Validation $ Left [ParseErr key "Unable to cast to Int16"]
   _ ->  Validation $ Left [NotFound key]

instance FromParam Int32 'QueryParam where
  fromParam pt key kvs = case lookupParam pt key kvs of
   Just (Just par) -> case decodeParam par of
     Just v -> Validation $ Right v
     _      -> Validation $ Left [ParseErr key "Unable to cast to Int32"]
   _ ->  Validation $ Left [NotFound key]

instance FromParam Int32 'FormParam where
  fromParam pt key kvs = case lookupParam pt key kvs of
   Just par -> case decodeParam par of
     Just v -> Validation $ Right v
     _      -> Validation $ Left [ParseErr key "Unable to cast to Int32"]
   _ ->  Validation $ Left [NotFound key]

instance FromParam Int32 'Cookie where
  fromParam pt key kvs = case lookupParam pt key kvs of
   Just par -> case decodeParam par of
     Just v -> Validation $ Right v
     _      -> Validation $ Left [ParseErr key "Unable to cast to Int32"]
   _ ->  Validation $ Left [NotFound key]

instance FromParam Int64 'QueryParam where
  fromParam pt key kvs = case lookupParam pt key kvs of
   Just (Just par) -> case decodeParam par of
     Just v -> Validation $ Right v
     _      -> Validation $ Left [ParseErr key "Unable to cast to Int64"]
   _ ->  Validation $ Left [NotFound key]

instance FromParam Int64 'FormParam where
  fromParam pt key kvs = case lookupParam pt key kvs of
   Just par -> case decodeParam par of
     Just v -> Validation $ Right v
     _      -> Validation $ Left [ParseErr key "Unable to cast to Int64"]
   _ ->  Validation $ Left [NotFound key]

instance FromParam Int64 'Cookie where
  fromParam pt key kvs = case lookupParam pt key kvs of
   Just par -> case decodeParam par of
     Just v -> Validation $ Right v
     _      -> Validation $ Left [ParseErr key "Unable to cast to Int64"]
   _ ->  Validation $ Left [NotFound key]

instance FromParam Integer 'QueryParam where
  fromParam pt key kvs = case lookupParam pt key kvs of
   Just (Just par) -> case decodeParam par of
     Just v -> Validation $ Right v
     _      -> Validation $ Left [ParseErr key "Unable to cast to Int64"]
   _ ->  Validation $ Left [NotFound key]

instance FromParam Integer 'FormParam where
  fromParam pt key kvs = case lookupParam pt key kvs of
   Just par -> case decodeParam par of
     Just v -> Validation $ Right v
     _      -> Validation $ Left [ParseErr key "Unable to cast to Int64"]
   _ ->  Validation $ Left [NotFound key]

instance FromParam Integer 'Cookie where
  fromParam pt key kvs = case lookupParam pt key kvs of
   Just par -> case decodeParam par of
     Just v -> Validation $ Right v
     _      -> Validation $ Left [ParseErr key "Unable to cast to Int64"]
   _ ->  Validation $ Left [NotFound key]

instance FromParam Word 'QueryParam where
  fromParam pt key kvs = case lookupParam pt key kvs of
   Just (Just par) -> case decodeParam par of
         Just v -> Validation $ Right v
         _      -> Validation $ Left [ParseErr key "Unable to cast to Word"]
   _ ->  Validation $ Left [NotFound key]

instance FromParam Word 'FormParam where
  fromParam pt key kvs = case lookupParam pt key kvs of
   Just par -> case decodeParam par of
         Just v -> Validation $ Right v
         _      -> Validation $ Left [ParseErr key "Unable to cast to Word"]
   _ ->  Validation $ Left [NotFound key]

instance FromParam Word 'Cookie where
  fromParam pt key kvs = case lookupParam pt key kvs of
   Just par -> case decodeParam par of
         Just v -> Validation $ Right v
         _      -> Validation $ Left [ParseErr key "Unable to cast to Word"]
   _ ->  Validation $ Left [NotFound key]

instance FromParam Word8 'QueryParam where
  fromParam pt key kvs = case lookupParam pt key kvs of
   Just (Just par) -> case decodeParam par of
     Just v -> Validation $ Right v
     _      -> Validation $ Left [ParseErr key "Unable to cast to Word8"]
   _ ->  Validation $ Left [NotFound key]

instance FromParam Word8 'FormParam where
  fromParam pt key kvs = case lookupParam pt key kvs of
   Just par -> case decodeParam par of
     Just v -> Validation $ Right v
     _      -> Validation $ Left [ParseErr key "Unable to cast to Word8"]
   _ ->  Validation $ Left [NotFound key]

instance FromParam Word8 'Cookie where
  fromParam pt key kvs = case lookupParam pt key kvs of
   Just par -> case decodeParam par of
     Just v -> Validation $ Right v
     _      -> Validation $ Left [ParseErr key "Unable to cast to Word8"]
   _ ->  Validation $ Left [NotFound key]

instance FromParam Word16 'QueryParam where
  fromParam pt key kvs = case lookupParam pt key kvs of
   Just (Just par) -> case decodeParam par of
     Just v -> Validation $ Right v
     _      -> Validation $ Left [ParseErr key "Unable to cast to Word16"]
   _ ->  Validation $ Left [NotFound key]

instance FromParam Word16 'FormParam where
  fromParam pt key kvs = case lookupParam pt key kvs of
   Just par -> case decodeParam par of
     Just v -> Validation $ Right v
     _      -> Validation $ Left [ParseErr key "Unable to cast to Word16"]
   _ ->  Validation $ Left [NotFound key]

instance FromParam Word16 'Cookie where
  fromParam pt key kvs = case lookupParam pt key kvs of
   Just par -> case decodeParam par of
     Just v -> Validation $ Right v
     _      -> Validation $ Left [ParseErr key "Unable to cast to Word16"]
   _ ->  Validation $ Left [NotFound key]

instance FromParam Word32 'QueryParam where
  fromParam pt key kvs = case lookupParam pt key kvs of
   Just (Just par) -> case decodeParam par of
     Just v -> Validation $ Right v
     _      -> Validation $ Left [ParseErr key "Unable to cast to Word32"]
   _ ->  Validation $ Left [NotFound key]

instance FromParam Word32 'FormParam where
  fromParam pt key kvs = case lookupParam pt key kvs of
   Just par -> case decodeParam par of
     Just v -> Validation $ Right v
     _      -> Validation $ Left [ParseErr key "Unable to cast to Word32"]
   _ ->  Validation $ Left [NotFound key]

instance FromParam Word32 'Cookie where
  fromParam pt key kvs = case lookupParam pt key kvs of
   Just par -> case decodeParam par of
     Just v -> Validation $ Right v
     _      -> Validation $ Left [ParseErr key "Unable to cast to Word32"]
   _ ->  Validation $ Left [NotFound key]

instance FromParam Word64 'QueryParam where
  fromParam pt key kvs = case lookupParam pt key kvs of
   Just (Just par) -> case decodeParam par of
     Just v -> Validation $ Right v
     _      -> Validation $ Left [ParseErr key "Unable to cast to Word64"]
   _ ->  Validation $ Left [NotFound key]

instance FromParam Word64 'FormParam where
  fromParam pt key kvs = case lookupParam pt key kvs of
   Just par -> case decodeParam par of
     Just v -> Validation $ Right v
     _      -> Validation $ Left [ParseErr key "Unable to cast to Word64"]
   _ ->  Validation $ Left [NotFound key]

instance FromParam Word64 'Cookie where
  fromParam pt key kvs = case lookupParam pt key kvs of
   Just par -> case decodeParam par of
     Just v -> Validation $ Right v
     _      -> Validation $ Left [ParseErr key "Unable to cast to Word64"]
   _ ->  Validation $ Left [NotFound key]

instance FromParam Double 'QueryParam where
  fromParam pt key kvs = case lookupParam pt key kvs of
   Just (Just par) -> case decodeParam par of
         Just v -> Validation $ Right v
         _      -> Validation $ Left [ParseErr key "Unable to cast to Double"]
   _ ->  Validation $ Left [NotFound key]

instance FromParam Double 'FormParam where
  fromParam pt key kvs = case lookupParam pt key kvs of
   Just par -> case decodeParam par of
         Just v -> Validation $ Right v
         _      -> Validation $ Left [ParseErr key "Unable to cast to Double"]
   _ ->  Validation $ Left [NotFound key]

instance FromParam Double 'Cookie where
  fromParam pt key kvs = case lookupParam pt key kvs of
   Just par -> case decodeParam par of
         Just v -> Validation $ Right v
         _      -> Validation $ Left [ParseErr key "Unable to cast to Double"]
   _ ->  Validation $ Left [NotFound key]

instance FromParam Float 'QueryParam where
  fromParam pt key kvs = case lookupParam pt key kvs of
   Just (Just par) -> case decodeParam par of
         Just v -> Validation $ Right v
         _      -> Validation $ Left [ParseErr key "Unable to cast to Float"]
   _ ->  Validation $ Left [NotFound key]

instance FromParam Float 'FormParam where
  fromParam pt key kvs = case lookupParam pt key kvs of
   Just par -> case decodeParam par of
         Just v -> Validation $ Right v
         _      -> Validation $ Left [ParseErr key "Unable to cast to Float"]
   _ ->  Validation $ Left [NotFound key]

instance FromParam Float 'Cookie where
  fromParam pt key kvs = case lookupParam pt key kvs of
   Just par -> case decodeParam par of
         Just v -> Validation $ Right v
         _      -> Validation $ Left [ParseErr key "Unable to cast to Float"]
   _ ->  Validation $ Left [NotFound key]

instance FromParam ByteString 'QueryParam where
  fromParam pt key kvs = case lookupParam pt key kvs of
   Just (Just par) -> case decodeParam par of
         Just v -> Validation $ Right v
         _      -> Validation $ Left [ParseErr key "Unable to cast to ByteString"]
   _ ->  Validation $ Left [NotFound key]

instance FromParam ByteString 'FormParam where
  fromParam pt key kvs = case lookupParam pt key kvs of
   Just par -> case decodeParam par of
         Just v -> Validation $ Right v
         _      -> Validation $ Left [ParseErr key "Unable to cast to ByteString"]
   _ ->  Validation $ Left [NotFound key]

instance FromParam ByteString 'Cookie where
  fromParam pt key kvs = case lookupParam pt key kvs of
   Just par -> case decodeParam par of
         Just v -> Validation $ Right v
         _      -> Validation $ Left [ParseErr key "Unable to cast to ByteString"]
   _ ->  Validation $ Left [NotFound key]

instance FromParam a par => FromParam (Maybe a) par where
  fromParam pt key kvs = case Trie.null kvs' of
    True  ->  Validation $ Right Nothing
    False -> case (fromParam pt key kvs' :: Validation [ParamErr] a) of
      Validation (Right val) -> Validation $ Right $ Just val
      Validation (Left errs) -> Validation $ Left errs
    where kvs' = submap key kvs

instance (FromParam a par, FromParam b par) => FromParam (Either a b) par where
  fromParam pt key kvs = case Trie.null kvsL of
    True -> case Trie.null kvsR of
      True -> Validation $ Left [ParseErr key "Unable to cast to Either"]
      False -> Right <$> fromParam pt keyR kvsR
    False -> Left <$> fromParam pt keyL kvsL
    where kvsL = submap keyL kvs
          kvsR = submap keyR kvs
          keyL = (key `nest` "Left")
          keyR = (key `nest` "Right")

instance FromParam T.Text 'QueryParam where
  fromParam pt key kvs = case lookupParam pt key kvs of
   Just (Just par) -> case decodeParam par of
     Just v -> Validation $ Right v
     _      -> Validation $ Left [ParseErr key "Unable to cast to Text"]
   _ ->  Validation $ Left [NotFound key]

instance FromParam T.Text 'FormParam where
  fromParam pt key kvs = case lookupParam pt key kvs of
   Just par -> case decodeParam par of
     Just v -> Validation $ Right v
     _      -> Validation $ Left [ParseErr key "Unable to cast to Text"]
   _ ->  Validation $ Left [NotFound key]

instance FromParam T.Text 'Cookie where
  fromParam pt key kvs = case lookupParam pt key kvs of
   Just par -> case decodeParam par of
     Just v -> Validation $ Right v
     _      -> Validation $ Left [ParseErr key "Unable to cast to Text"]
   _ ->  Validation $ Left [NotFound key]

instance FromParam Day 'QueryParam where
  fromParam pt key kvs = case lookupParam pt key kvs of
   Just (Just par) -> case decodeParam par of
     Just v -> Validation $ Right v
     _      -> Validation $ Left [ParseErr key "Unable to cast to Day"]
   _ ->  Validation $ Left [NotFound key]

instance FromParam Day 'FormParam where
  fromParam pt key kvs = case lookupParam pt key kvs of
   Just par -> case decodeParam par of
     Just v -> Validation $ Right v
     _      -> Validation $ Left [ParseErr key "Unable to cast to Day"]
   _ ->  Validation $ Left [NotFound key]

instance FromParam Day 'Cookie where
  fromParam pt key kvs = case lookupParam pt key kvs of
   Just par -> case decodeParam par of
     Just v -> Validation $ Right v
     _      -> Validation $ Left [ParseErr key "Unable to cast to Day"]
   _ ->  Validation $ Left [NotFound key]

instance (Show (DeSerializedData par), FromParam a par) => FromParam [a] par where
  fromParam pt key kvs = case Trie.null kvs' of
    True  ->  Validation $ Right []
    False ->
      let pars = Prelude.map (\(nkey, kv) -> fromParam pt nkey kv :: Validation [ParamErr] a) kvitems
      in (Prelude.reverse) <$> Fold.foldl' accRes (Validation $ Right []) pars
    where kvs' = submap key kvs
          kvitems = Prelude.takeWhile (not . Prelude.null . snd)  (Prelude.map (\ix ->
            let ixkey = key `nest` (ASCII.pack $ show ix)
            in (ixkey, submap ixkey kvs')) [(0 :: Word) .. 2000])
          accRes acc elemt = case (acc, elemt) of
            (Validation (Right as), Validation (Right e)) -> Validation $ Right (e:as)
            (Validation (Left as), Validation (Right _)) -> Validation $ Left as
            (Validation (Right _), Validation (Left es)) -> Validation $ Left es
            (Validation (Left as), Validation (Left es)) -> Validation $ Left (es ++ as)

instance (Show (DeSerializedData par), FromParam a par) => FromParam (Vector a) par where
  fromParam pt key kvs = case fromParam pt key kvs of
    Validation (Right v)  -> Validation $ Right (V.fromList v)
    Validation (Left err) -> Validation (Left err)

instance (DecodeParam a) => FromParam (OptValue a) 'QueryParam where
  fromParam pt key kvs = case lookupParam pt key kvs of
   Just (Just par) -> case decodeParam par of
     Just v     -> Validation $ Right $ OptValue $ Just v
     _          -> Validation $ Left [ParseErr key "Unable to cast to OptValue"]
   Just Nothing -> Validation $ Right $ OptValue Nothing
   _            -> Validation $ Left [NotFound key]

instance (DecodeParam a) => FromParam (OptValue a) 'FormParam where
  fromParam pt key kvs = case lookupParam pt key kvs of
   Just par -> case decodeParam par of
     Just v -> Validation $ Right $ OptValue $ Just v
     _      -> Validation $ Left [ParseErr key "Unable to cast to OptValue"]
   _        -> Validation $ Left [NotFound key]

instance (DecodeParam a) => FromParam (OptValue a) 'Cookie where
  fromParam pt key kvs = case lookupParam pt key kvs of
   Just par -> case decodeParam par of
     Just v -> Validation $ Right $ OptValue $ Just v
     _      -> Validation $ Left [ParseErr key "Unable to cast to OptValue"]
   _        -> Validation $ Left [NotFound key]

instance ToParam FileInfo 'FileParam where
  toParam _ key (FileInfo val) = [(key, val)]

instance FromParam FileInfo 'FileParam where
  fromParam pt key kvs = case lookupParam pt key kvs of
    Just par -> Validation $ Right (FileInfo par)
    Nothing  -> Validation $ Left [NotFound key]

instance ToParam ByteString 'PathParam where
  toParam _ _ v = [encodeParam v]

instance ToParam Int 'PathParam where
  toParam _ _ v = [encodeParam v]

instance ToParam Int8 'PathParam where
  toParam _ _ v = [encodeParam v]

instance ToParam Int16 'PathParam where
  toParam _ _ v = [encodeParam v]

instance ToParam Int32 'PathParam where
  toParam _ _ v = [encodeParam v]

instance ToParam Int64 'PathParam where
  toParam _ _ v = [encodeParam v]

instance ToParam Word 'PathParam where
  toParam _ _ v = [encodeParam v]

instance ToParam Word8 'PathParam where
  toParam _ _ v = [encodeParam v]

instance ToParam Word16 'PathParam where
  toParam _ _ v = [encodeParam v]

instance ToParam Word32 'PathParam where
  toParam _ _ v = [encodeParam v]

instance ToParam Word64 'PathParam where
  toParam _ _ v = [encodeParam v]

instance ToParam Float 'PathParam where
  toParam _ _ v = [encodeParam v]

instance ToParam Double 'PathParam where
  toParam _ _ v = [encodeParam v]

instance ToParam Char 'PathParam where
  toParam _ _ v = [encodeParam v]

instance ToParam T.Text 'PathParam where
  toParam _ _ v = [encodeParam v]

instance ToParam Day 'PathParam where
  toParam _ _ v = [encodeParam v]

instance ToParam UTCTime 'PathParam where
  toParam _ _ v = [encodeParam v]

instance ToParam Bool 'PathParam where
  toParam _ _ v = [encodeParam v]

instance ToParam Integer 'PathParam where
  toParam _ _ v = [encodeParam v]

instance (ToJSON a) => ToParam (JsonOf a) 'PathParam where
  toParam _ _ v = [encodeParam v]

instance ( EncodeParam a
         , EncodeParam b
         ) => ToParam (a, b) 'PathParam where
  toParam _ _ (a, b) = [encodeParam a, encodeParam b]

instance ( EncodeParam a
         , EncodeParam b
         , EncodeParam c
         ) => ToParam (a, b, c) 'PathParam where
  toParam _ _ (a, b, c) = [ encodeParam a
                          , encodeParam b
                          , encodeParam c
                          ]

instance ( EncodeParam a
         , EncodeParam b
         , EncodeParam c
         , EncodeParam d
         ) => ToParam (a, b, c, d) 'PathParam where
  toParam _ _ (a, b, c, d)
    = [ encodeParam a
      , encodeParam b
      , encodeParam c
      , encodeParam d
      ]

instance ( EncodeParam a
         , EncodeParam b
         , EncodeParam c
         , EncodeParam d
         , EncodeParam e
         ) => ToParam (a, b, c, d, e) 'PathParam where
  toParam _ _ (a, b, c, d, e)
    = [ encodeParam a
      , encodeParam b
      , encodeParam c
      , encodeParam d
      , encodeParam e
      ]

instance ( EncodeParam a
         , EncodeParam b
         , EncodeParam c
         , EncodeParam d
         , EncodeParam e
         , EncodeParam f
         ) => ToParam (a, b, c, d, e, f) 'PathParam where
  toParam _ _ (a, b, c, d, e, f)
    = [ encodeParam a
      , encodeParam b
      , encodeParam c
      , encodeParam d
      , encodeParam e
      , encodeParam f
      ]

instance ( EncodeParam a
         , EncodeParam b
         , EncodeParam c
         , EncodeParam d
         , EncodeParam e
         , EncodeParam f
         , EncodeParam g
         , EncodeParam h
         ) => ToParam (a, b, c, d, e, f, g, h) 'PathParam where
  toParam _ _ (a, b, c, d, e, f, g, h)
    = [ encodeParam a
      , encodeParam b
      , encodeParam c
      , encodeParam d
      , encodeParam e
      , encodeParam f
      , encodeParam g
      , encodeParam h
      ]

instance ( EncodeParam a
         , EncodeParam b
         , EncodeParam c
         , EncodeParam d
         , EncodeParam e
         , EncodeParam f
         , EncodeParam g
         , EncodeParam h
         , EncodeParam i
         ) => ToParam (a, b, c, d, e, f, g, h, i) 'PathParam where
  toParam _ _ (a, b, c, d, e, f, g, h, i)
    = [ encodeParam a
      , encodeParam b
      , encodeParam c
      , encodeParam d
      , encodeParam e
      , encodeParam f
      , encodeParam g
      , encodeParam h
      , encodeParam i
      ]

instance ( EncodeParam a
         , EncodeParam b
         , EncodeParam c
         , EncodeParam d
         , EncodeParam e
         , EncodeParam f
         , EncodeParam g
         , EncodeParam h
         , EncodeParam i
         , EncodeParam j
         ) => ToParam (a, b, c, d, e, f, g, h, i, j) 'PathParam where
  toParam _ _ (a, b, c, d, e, f, g, h, i, j)
    = [ encodeParam a
      , encodeParam b
      , encodeParam c
      , encodeParam d
      , encodeParam e
      , encodeParam f
      , encodeParam g
      , encodeParam h
      , encodeParam i
      , encodeParam j
      ]

-- | Errors that occured during deserialization.
data ParamErr = NotFound ByteString -- ^ The key was not found.
              | ParseErr ByteString T.Text -- ^ A parse error occured while deserializing the type.
                deriving (Show, Eq)

utf8DecodeError :: String -> String -> a
utf8DecodeError src msg = error $ "Error decoding Bytes into UTF8 string at: " ++ src ++ " Message: " ++ msg

instance ToJSON ParamErr where
  toJSON (NotFound bs) = case decodeUtf8' bs of
    Left ex   -> utf8DecodeError "ToJSON ParamErr" (show ex)
    Right bs' -> A.object ["NotFound" A..= bs']
  toJSON (ParseErr bs msg) = case decodeUtf8' bs of
    Left ex -> utf8DecodeError "ToJSON ParamErr" (show ex)
    Right bs' -> A.object ["ParseErr" A..= [bs', msg]]

-- | This class de
class ParamErrToApiErr apiErr where
  toApiErr :: [ParamErr] -> apiErr

instance ParamErrToApiErr () where
  toApiErr = const ()

instance ParamErrToApiErr T.Text where
  toApiErr errs = T.pack (show errs)

instance ParamErrToApiErr A.Value where
  toApiErr errs = toJSON errs

-- | Nest the key with a prefix.
--
-- > nest "pfx" "key" == "pfx.key"
-- > nest "" "key" == "key"
nest :: ByteString -> ByteString -> ByteString
nest s1 s2 | SB.null s1 = s2
           | otherwise = SB.concat [s1, ".", s2]

-- | Lookup a value from the trie using the given key.
lookupParam :: Proxy (parK :: ParamK) -> ByteString -> Trie (DeSerializedData parK) -> Maybe (DeSerializedData parK)
lookupParam _ key kvs = Trie.lookup key kvs

data ParamAcc = ParamAcc { index :: Int, isSum :: Bool }
              deriving (Show, Eq)

data ParamSettings = ParamSettings
                   deriving (Show, Eq)

-- | Serialize a type to the header params
class ToHeader a where
  toHeader' :: a -> [Http.Header]

  default toHeader' :: (Generic a, GToHeader (Rep a)) => a -> [Http.Header]
  toHeader' = gtoHeader "" (ParamAcc 0 False) ParamSettings . from

-- | (Try to) Deserialize a type from the header params
class FromHeader a where
  fromHeader' :: [Http.Header] -> Validation [ParamErr] a

  default fromHeader' :: (Generic a, GFromHeader (Rep a)) => [Http.Header] -> Validation [ParamErr] a
  fromHeader' = (fmap to) . gfromHeader "" (ParamAcc 0 False) ParamSettings

class GToHeader f where
  gtoHeader :: ByteString -> ParamAcc -> ParamSettings -> f a -> [Http.Header]

instance (GToHeader f, GToHeader g) => GToHeader (f :+: g) where
  gtoHeader pfx pa psett (L1 x) = gtoHeader pfx (pa { isSum = True }) psett x
  gtoHeader pfx pa psett (R1 y) = gtoHeader pfx (pa { isSum = True }) psett y

instance (GToHeader f, GToHeader g) => GToHeader (f :*: g) where
  gtoHeader pfx pa psett (x :*: y) = gtoHeader pfx pa psett x ++ gtoHeader pfx (pa { index = index pa + 1 }) psett y

instance (EncodeParam c) => GToHeader (K1 i c) where
  gtoHeader pfx _ _ (K1 x) = [(mk pfx, encodeParam x)]

instance (GToHeader f, Constructor t) => GToHeader (M1 C t f) where
  gtoHeader pfx pa psett con@(M1 x) = case isSum pa of
    True  -> gtoHeader (pfx `nest` ASCII.pack (conName con)) (pa { index = 0 }) psett x
    False -> gtoHeader pfx (pa { index = 0 }) psett x

instance (GToHeader f) => GToHeader (M1 D t f) where
  gtoHeader pfx pa psett (M1 x) = gtoHeader pfx pa psett x

instance (GToHeader f, Selector t) => GToHeader (M1 S t f) where
  gtoHeader pfx pa psett m@(M1 x) = let fldN = ASCII.pack (selName m)
                                    in case fldN of
                                      "" -> gtoHeader (pfx `nest` numberedFld pa) pa psett x
                                      _  -> gtoHeader (pfx `nest` fldN) pa psett x

instance GToHeader U1 where
  gtoHeader pfx _ _ _ = [(mk pfx, encodeParam Unit)]

class GFromHeader f where
  gfromHeader :: ByteString -> ParamAcc -> ParamSettings -> [Http.Header] -> Validation [ParamErr] (f a)

instance (GFromHeader f, GFromHeader g) => GFromHeader (f :*: g) where
  gfromHeader pfx pa psett kvs = (:*:) <$> gfromHeader pfx pa psett kvs
                                 <*> gfromHeader pfx (pa { index = index pa + 1 }) psett kvs

instance (GFromHeader f, GFromHeader g) => GFromHeader (f :+: g) where
  gfromHeader pfx pa psett kvs = case L1 <$> gfromHeader pfx (pa { isSum = True }) psett kvs of
    l1@(Validation (Right _)) -> l1
    Validation (Left []) -> R1 <$> gfromHeader pfx (pa { isSum = True }) psett kvs
    l1 -> l1

instance (GFromHeader f, Constructor t) => GFromHeader (M1 C t f) where
  gfromHeader pfx pa psett kvs =
    let conN = ASCII.pack (conName (undefined :: (M1 C t f) a))
    in case isSum pa of
      True -> case isMemberH (pfx `nest` conN) kvs of
        True -> M1 <$> gfromHeader (pfx `nest` conN) pa psett kvs
        False -> Validation $ Left []
      False -> M1 <$> gfromHeader pfx pa psett kvs

instance (GFromHeader f, Datatype t) => GFromHeader (M1 D t f) where
  gfromHeader pfx pa psett kvs = case M1 <$> gfromHeader pfx pa psett kvs of
    Validation (Left []) -> Validation (Left [ParseErr pfx ("Unable to cast to SumType: " <> dtN)])
    v                    -> v

    where dtN = T.pack $ datatypeName (undefined :: (M1 D t f) a)

instance (GFromHeader f, Selector t) => GFromHeader (M1 S t f) where
  gfromHeader pfx pa psett kvs = let fldN = (ASCII.pack $ (selName (undefined :: (M1 S t f) a)))
                                 in case fldN of
                                   "" -> M1 <$> gfromHeader (pfx `nest` numberedFld pa) pa psett kvs
                                   _  -> M1 <$> gfromHeader (pfx `nest` fldN) pa psett kvs

instance (DecodeParam c) => GFromHeader (K1 i c) where
  gfromHeader key _ _ kvs = case lookupH key kvs of
    Just v -> case decodeParam v of
      Just v' -> Validation (Right $ K1 v') -- K1 <$> fromParam pt pfx kvs
      Nothing -> Validation $ Left [ParseErr key "Unable to cast to <Type>"]
    _ ->  Validation $ Left [NotFound key]

instance GFromHeader U1 where
  gfromHeader key _ _ kvs = case lookupH key kvs of
    Just v -> case (decodeParam v :: Maybe Unit) of
      Just _ -> Validation (Right U1)
      Nothing -> Validation $ Left [ParseErr key "Unable to cast to <NullaryType>"]
    _ ->  Validation $ Left [NotFound key]

class GFromParam f (parK :: ParamK) where
  gfromParam :: Proxy (parK :: ParamK) -> ByteString -> ParamAcc -> ParamSettings -> Trie (DeSerializedData parK) -> Validation [ParamErr] (f a)

instance (GFromParam f parK, GFromParam g parK) => GFromParam (f :*: g) parK where
  gfromParam pt pfx pa psett kvs = (:*:) <$> gfromParam pt pfx pa psett kvs
                                         <*> gfromParam pt pfx (pa { index = index pa + 1 }) psett kvs

instance (GFromParam f parK, GFromParam g parK) => GFromParam (f :+: g) parK where
  gfromParam pt pfx pa psett kvs = case L1 <$> gfromParam pt pfx (pa { isSum = True }) psett kvs of
    l1@(Validation (Right _)) -> l1
    Validation (Left []) -> R1 <$> gfromParam pt pfx (pa { isSum = True }) psett kvs
    l1 -> l1

instance (GFromParam f parK, Constructor t) => GFromParam (M1 C t f) parK where
  gfromParam pt pfx pa psett kvs =
    let conN = ASCII.pack (conName (undefined :: (M1 C t f) a))
    in case isSum pa of
      True -> case Trie.null $ submap (pfx `nest` conN) kvs of
        False  -> M1 <$> gfromParam pt (pfx `nest` conN) pa psett kvs
        True -> Validation $ Left []
      False -> M1 <$> gfromParam pt pfx pa psett kvs

instance (GFromParam f parK, Datatype t) => GFromParam (M1 D t f) parK where
  gfromParam pt pfx pa psett kvs = case M1 <$> gfromParam pt pfx pa psett kvs of
    Validation (Left []) -> Validation (Left [ParseErr pfx ("Unable to cast to SumType: " <> dtN)])
    v                    -> v

    where dtN = T.pack $ datatypeName (undefined :: (M1 D t f) a)

instance (GFromParam f parK, Selector t) => GFromParam (M1 S t f) parK where
  gfromParam pt pfx pa psett kvs = let fldN = (ASCII.pack $ (selName (undefined :: (M1 S t f) a)))
                                   in case fldN of
                                     "" -> M1 <$> gfromParam pt (pfx `nest` numberedFld pa) pa psett (submap pfx kvs)
                                     _  -> M1 <$> gfromParam pt (pfx `nest` fldN) pa psett (submap pfx kvs)

instance (FromParam c parK) => GFromParam (K1 i c) parK where
  gfromParam pt pfx _ _ kvs = K1 <$> fromParam pt pfx kvs

instance (FromParam Unit parK) => GFromParam U1 parK where
  gfromParam pt key _ _ kvs = const U1 <$> (fromParam pt key kvs :: Validation [ParamErr] Unit)

class GToParam f (parK :: ParamK) where
  gtoParam :: Proxy (parK :: ParamK) -> ByteString -> ParamAcc -> ParamSettings -> f a -> [SerializedData parK]

instance (GToParam f parK, GToParam g parK) => GToParam (f :*: g) parK where
  gtoParam pt pfx pa psett (x :*: y) = gtoParam pt pfx pa psett x ++ gtoParam pt pfx (pa { index = index pa + 1 }) psett y

instance (GToParam f parK, GToParam g parK) => GToParam (f :+: g) parK where
  gtoParam pt pfx pa psett(L1 x) = gtoParam pt pfx (pa { isSum = True }) psett x
  gtoParam pt pfx pa psett (R1 y) = gtoParam pt pfx (pa { isSum = True }) psett y

instance (ToParam c parK) => GToParam (K1 i c) parK where
  gtoParam pt pfx _ _ (K1 x) = toParam pt pfx x

instance (GToParam f parK, Constructor t) => GToParam (M1 C t f) parK where
  gtoParam pt pfx pa psett con@(M1 x) = case isSum pa of
    True  -> gtoParam pt (pfx `nest` ASCII.pack (conName con)) (pa { index = 0 }) psett x
    False -> gtoParam pt pfx (pa { index = 0 }) psett x

instance (GToParam f parK) => GToParam (M1 D t f) parK where
  gtoParam pt pfx pa psett (M1 x) = gtoParam pt pfx pa psett x

instance (GToParam f parK, Selector t) => GToParam (M1 S t f) parK where
  gtoParam pt pfx pa psett  m@(M1 x) = let fldN = ASCII.pack (selName m)
                                       in case fldN of
                                         "" -> gtoParam pt (pfx `nest` numberedFld pa) pa psett x
                                         _  -> gtoParam pt (pfx `nest` fldN) pa psett x

instance (ToParam Unit parK) => GToParam U1 parK where
  gtoParam pt pfx _ _ _ = toParam pt pfx Unit

numberedFld :: ParamAcc -> ByteString
numberedFld pa = ASCII.pack $ show (index pa)

isMemberH :: ByteString -> [Header] -> Bool
isMemberH k = maybe False (const True) . lookupH' isPrefixOf k

lookupH :: ByteString -> [Header] -> Maybe ByteString
lookupH = lookupH' (==)

lookupH' :: (CI ByteString -> CI ByteString -> Bool) -> ByteString -> [Header] -> Maybe ByteString
lookupH' f k = fmap snd . L.find ((f $ mk k) . fst)

isPrefixOf :: CI ByteString -> CI ByteString -> Bool
isPrefixOf n h = foldedCase n `SB.isPrefixOf` foldedCase h
