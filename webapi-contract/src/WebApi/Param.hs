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
       , fromCookie
       , lookupParam
       , fromNonNestedParam

       -- * Wrappers
       , CookieInfo (..)
       , JsonOf (..)
       , OptValue (..)
       , FileInfo (..)
       , NonNested (..)
       , DelimitedCollection (..)

       -- * Helpers
       , ParamK (..)
       , ParamSettings, fieldModifier
       , filePath
       , nest
       , defaultParamSettings
       , link
       , renderUriPath
         
       -- * Generic (De)Serialization fn
       , genericToQueryParam
       , genericFromQueryParam
       , genericToFormParam
       , genericFromFormParam
       , genericToFileParam
       , genericFromFileParam
       , genericToPathParam
       , genericFromPathParam
       , genericToCookie
       , genericFromCookie
       ) where


import           Data.Aeson                         (FromJSON (..), ToJSON (..), (.:))
import qualified Data.Aeson                         as A
import           Data.ByteString                    (ByteString)
import qualified Data.ByteString                    as SB
import           Data.ByteString.Builder            (byteString, char7,
                                                     toLazyByteString)
import           Data.ByteString.Char8              as ASCII (pack, readInteger,
                                                              split, unpack,
                                                              singleton)
import           Data.ByteString.Lazy               (toStrict)
import qualified Data.ByteString.Lex.Fractional     as LexF
import           Data.ByteString.Lex.Integral
import           Data.CaseInsensitive               as CI
import           Data.Foldable                      as Fold (foldl')
import           Data.Int
import qualified Data.List                          as L (find)
import           Data.Maybe                         (catMaybes)
import           Data.Monoid                        ((<>))
import           Data.Proxy
import           Data.Text                          (Text)
import qualified Data.Text                          as T (Text, pack, uncons)
import qualified Data.Text.Lazy                     as LT
import           Data.Text.Encoding                 (decodeUtf8, decodeUtf8', encodeUtf8)
import           Data.Time.Calendar                 (Day)
import           Data.Time.Clock                    (UTCTime, DiffTime)
import           Data.Time.LocalTime                (LocalTime, TimeOfDay)
import           Data.Time.Format                   (FormatTime,
                                                     defaultTimeLocale,
                                                     formatTime, parseTimeM)
import           Data.Trie                          as Trie
import           Data.Typeable
import           Data.Vector                        (Vector)
import qualified Data.Vector                        as V
import           Data.Word
import           GHC.Generics
import           GHC.TypeLits
import           Network.HTTP.Types
import           Network.HTTP.Types                 as Http (Header, QueryItem)
import           Control.Applicative
import           WebApi.Util
import           WebApi.Contract
import           Data.Set                           (Set)
import qualified Data.Set                           as Set
import           Data.MultiSet                      (MultiSet)
import qualified Data.MultiSet                      as MultiSet

-- | A type for holding a file.
data FileInfo = FileInfo
  { fileName        :: SB.ByteString
  , fileContentType :: SB.ByteString
  , fileContent     :: FilePath
  }
    deriving (Eq, Show)

-- | Obtain the file path from 'FileInfo'.
filePath :: FileInfo -> FilePath
filePath = fileContent

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

newtype DelimitedCollection (delim :: Symbol) (t :: *) = DelimitedCollection {getCollection :: Vector t}
                                                         deriving (Show)

data CookieInfo a = CookieInfo
  { cookieValue    :: a
  , cookiePath     :: Maybe ByteString
  , cookieExpires  :: Maybe UTCTime
  , cookieMaxAge   :: Maybe DiffTime
  , cookieDomain   :: Maybe ByteString
  , cookieHttpOnly :: Maybe Bool
  , cookieSecure   :: Maybe Bool
  }

defCookieInfo :: a -> CookieInfo a
defCookieInfo val = CookieInfo
  { cookieValue    = val
  , cookiePath     = Nothing
  , cookieExpires  = Nothing
  , cookieMaxAge   = Nothing
  , cookieDomain   = Nothing
  , cookieHttpOnly = Nothing
  , cookieSecure   = Nothing
  }

-- | Define result of serialization of a type of kind 'ParamK'.
type family SerializedData (par :: ParamK) where
  SerializedData 'QueryParam = Http.QueryItem
  SerializedData 'FormParam  = (ByteString, ByteString)
  SerializedData 'FileParam  = (ByteString, FileInfo)
  SerializedData 'PathParam  = ByteString
  SerializedData 'Cookie     = (ByteString, CookieInfo ByteString)

-- | Define result of deserialization of a type of kind 'ParamK'.
type family DeSerializedData (par :: ParamK) where
  DeSerializedData 'QueryParam = Maybe ByteString
  DeSerializedData 'FormParam  = ByteString
  DeSerializedData 'FileParam  = FileInfo
  DeSerializedData 'PathParam  = ByteString
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

-- | Generate a type safe URL for a given route type. The URI can be used for setting a base URL if required.
link :: ( ToParam 'QueryParam (QueryParam m r)
        , MkPathFormatString r
        , ToParam 'PathParam (PathParam m r)
        ) =>
          route (m :: *) (r :: *)
        -> ByteString
        -> PathParam m r
        -> Maybe (QueryParam m r)
        -> ByteString
link r basePath paths query = toStrict $ toLazyByteString $ (byteString base) <> uriPath
  where
    uriPath = encodePath (routePaths paths r) (toQueryParam query)
    base    = case basePath of
      "/" -> ""
      _   -> basePath

renderUriPath ::  ( ToParam 'PathParam path
                   , MkPathFormatString r
                   ) => ByteString -> path -> route m r -> ByteString
renderUriPath basePath p r = toStrict $ toLazyByteString $ (byteString base) <> (encodePathSegments $ routePaths p r)
  where
    base    = case basePath of
      "/" -> ""
      _   -> basePath

routePaths :: ( ToParam 'PathParam path
                , MkPathFormatString r
                ) => path -> route m r -> [Text]
routePaths p r = uriPathPieces (toPathParam p)

  where uriPathPieces :: [ByteString] -> [Text]
        uriPathPieces dynVs = reverse $ fst $ foldl' (flip fillHoles) ([], dynVs) (mkPathFormatString (toRoute r))

        fillHoles :: PathSegment -> ([Text], [ByteString]) -> ([Text], [ByteString])
        fillHoles (StaticSegment t) (segs, dynVs)    = (t : segs, dynVs)
        fillHoles  Hole             (segs, dynV: xs) = (decodeUtf8 dynV : segs, xs)
        fillHoles  Hole             (_segs, [])      = error "Panic: fewer pathparams than holes"

        toRoute :: route m r -> Proxy r
        toRoute = const Proxy
      

-- | Serialize a type into query params.
toQueryParam :: (ToParam 'QueryParam a) => a -> Query
toQueryParam = toParam (Proxy :: Proxy 'QueryParam) ""

-- | Serialize a type into form params.
toFormParam :: (ToParam 'FormParam a) => a -> [(ByteString, ByteString)]
toFormParam = toParam (Proxy :: Proxy 'FormParam) ""

-- | Serialize a type into file params.
toFileParam :: (ToParam 'FileParam a) => a -> [(ByteString, FileInfo)]
toFileParam = toParam (Proxy :: Proxy 'FileParam) ""

-- | Serialize a type into path params.
toPathParam :: (ToParam 'PathParam a) => a -> [ByteString]
toPathParam = toParam (Proxy :: Proxy 'PathParam) ""

-- | Serialize a type into cookie.
toCookie :: (ToParam 'Cookie a) => a -> [(ByteString, CookieInfo ByteString)]
toCookie = toParam (Proxy :: Proxy 'Cookie) ""

-- | (Try to) Deserialize a type from query params.
fromQueryParam :: (FromParam 'QueryParam a) => Query -> Validation [ParamErr] a
fromQueryParam par = fromParam (Proxy :: Proxy 'QueryParam) "" $ Trie.fromList par

-- | (Try to) Deserialize a type from form params.
fromFormParam :: (FromParam 'FormParam a) => [(ByteString, ByteString)] -> Validation [ParamErr] a
fromFormParam par = fromParam (Proxy :: Proxy 'FormParam) "" $ Trie.fromList par

-- | (Try to) Deserialize a type from file params.
fromFileParam :: (FromParam 'FileParam a) => [(ByteString, FileInfo)] -> Validation [ParamErr] a
fromFileParam par = fromParam (Proxy :: Proxy 'FileParam) "" $ Trie.fromList par

-- | (Try to) Deserialize a type from cookie.
fromCookie :: (FromParam 'Cookie a) => [(ByteString, ByteString)] -> Validation [ParamErr] a
fromCookie par = fromParam (Proxy :: Proxy 'Cookie) "" $ Trie.fromList par

genericToQueryParam :: (Generic a, GToParam (Rep a) 'QueryParam) => ParamSettings -> ByteString -> a -> [Http.QueryItem]
genericToQueryParam opts pfx = gtoParam (Proxy :: Proxy 'QueryParam) pfx (ParamAcc 0 False) opts . from

genericFromQueryParam :: (Generic a, GFromParam (Rep a) 'QueryParam) => ParamSettings -> ByteString -> Trie (Maybe ByteString) -> Validation [ParamErr] a
genericFromQueryParam opts pfx = (fmap to) . gfromParam (Proxy :: Proxy 'QueryParam) pfx (ParamAcc 0 False) opts

genericToFormParam :: (Generic a, GToParam (Rep a) 'FormParam) => ParamSettings -> ByteString -> a -> [(ByteString, ByteString)]
genericToFormParam opts pfx = gtoParam (Proxy :: Proxy 'FormParam) pfx (ParamAcc 0 False) opts . from

genericFromFormParam :: (Generic a, GFromParam (Rep a) 'FormParam) => ParamSettings -> ByteString -> Trie ByteString -> Validation [ParamErr] a
genericFromFormParam opts pfx = (fmap to) . gfromParam (Proxy :: Proxy 'FormParam) pfx (ParamAcc 0 False) opts

genericToFileParam :: (Generic a, GToParam (Rep a) 'FileParam) => ParamSettings -> ByteString -> a -> [(ByteString, FileInfo)]
genericToFileParam opts pfx = gtoParam (Proxy :: Proxy 'FileParam) pfx (ParamAcc 0 False) opts . from

genericFromFileParam :: (Generic a, GFromParam (Rep a) 'FileParam) => ParamSettings -> ByteString -> Trie (FileInfo) -> Validation [ParamErr] a
genericFromFileParam opts pfx = (fmap to) . gfromParam (Proxy :: Proxy 'FileParam) pfx (ParamAcc 0 False) opts

genericToPathParam :: (Generic a, GToParam (Rep a) 'PathParam) => ParamSettings -> ByteString -> a -> [ByteString]
genericToPathParam opts pfx = gtoParam (Proxy :: Proxy 'PathParam) pfx (ParamAcc 0 False) opts . from

genericFromPathParam :: (Generic a, GFromParam (Rep a) 'PathParam) => ParamSettings -> ByteString -> Trie ByteString -> Validation [ParamErr] a
genericFromPathParam opts pfx = (fmap to) . gfromParam (Proxy :: Proxy 'PathParam) pfx (ParamAcc 0 False) opts

genericToCookie :: (Generic a, GToParam (Rep a) 'Cookie) => ParamSettings -> ByteString -> a -> [(ByteString, CookieInfo ByteString)]
genericToCookie opts pfx = gtoParam (Proxy :: Proxy 'Cookie) pfx (ParamAcc 0 False) opts . from

genericFromCookie :: (Generic a, GFromParam (Rep a) 'Cookie) => ParamSettings -> ByteString -> Trie ByteString -> Validation [ParamErr] a
genericFromCookie opts pfx = (fmap to) . gfromParam (Proxy :: Proxy 'Cookie) pfx (ParamAcc 0 False) opts

-- | Serialize a type to a given type of kind 'ParamK'.
class ToParam (parK :: ParamK) a where
  toParam :: Proxy (parK :: ParamK) -> ByteString -> a -> [SerializedData parK]

  default toParam :: (Generic a, GToParam (Rep a) parK) => Proxy (parK :: ParamK) -> ByteString -> a -> [SerializedData parK]
  toParam pt pfx = gtoParam pt pfx (ParamAcc 0 False) defaultParamSettings . from

-- | (Try to) Deserialize a type from a given type of kind 'ParamK'.
class FromParam (parK :: ParamK) a where
  fromParam :: Proxy (parK :: ParamK) -> ByteString -> Trie (DeSerializedData parK) -> Validation [ParamErr] a

  default fromParam :: (Generic a, GFromParam (Rep a) parK) => Proxy (parK :: ParamK) -> ByteString -> Trie (DeSerializedData parK) -> Validation [ParamErr] a
  fromParam pt pfx = (fmap to) . gfromParam pt pfx (ParamAcc 0 False) defaultParamSettings

-- | Serialize a type to 'ByteString'.
class EncodeParam (t :: *) where
  encodeParam :: t -> ByteString

  default encodeParam :: (Generic t, GHttpParam (Rep t)) => t -> ByteString
  encodeParam = gEncodeParam . from

-- | (Try to) Deserialize a type from 'ByteString'.
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
  encodeParam       = ASCII.singleton

instance DecodeParam Char where
  decodeParam str = case decodeUtf8' str of
    Right txt -> fmap fst (T.uncons txt)
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

instance EncodeParam LocalTime where
  encodeParam t = ASCII.pack $ formatTime defaultTimeLocale format t
    where
      format = "%FT%T"

instance DecodeParam LocalTime where
  decodeParam str = case parseTimeM True defaultTimeLocale "%FT%T" (ASCII.unpack str) of
    Just d -> Just d
    _      -> Nothing

instance EncodeParam TimeOfDay where
  encodeParam t = ASCII.pack $ formatTime defaultTimeLocale "%T" t

instance DecodeParam TimeOfDay where
  decodeParam str = case parseTimeM True defaultTimeLocale "%T" (ASCII.unpack str) of
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

instance (EncodeParam t, KnownSymbol sym) => EncodeParam (DelimitedCollection sym t) where
  encodeParam (DelimitedCollection innerVector) = SB.intercalate (ASCII.pack $ symbolVal (Proxy :: Proxy sym)) $ fmap (\singleVal -> encodeParam singleVal ) (V.toList innerVector)

instance (DecodeParam t, KnownSymbol sym) => DecodeParam (DelimitedCollection sym t) where
  decodeParam str =
    case symbolVal (Proxy :: Proxy sym) of
      delim : _ -> case sequenceA $ fmap decodeParam (ASCII.split delim str) of
        Just parsedList -> Just $ DelimitedCollection $ V.fromList $ parsedList
        Nothing -> Nothing
      _ -> Nothing

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

-- | Use this type if for serialization \/ deserialization nesting is not required. The type contained within most likely requires 'EncodeParam' \/ 'DecodeParam'.
newtype NonNested a = NonNested { getNonNestedParam :: a }
                    deriving (Show, Eq, Read)

-- | Serialize a type without nesting.
toNonNestedParam :: (ToParam parK (NonNested a)) => Proxy (parK :: ParamK) -> ByteString -> a -> [SerializedData parK]
toNonNestedParam par pfx a = toParam par pfx (NonNested a)

-- | (Try to) Deserialize a type without nesting.
fromNonNestedParam :: (FromParam parK (NonNested a)) => Proxy (parK :: ParamK) -> ByteString -> Trie (DeSerializedData parK) -> Validation [ParamErr] a
fromNonNestedParam par pfx kvs = getNonNestedParam <$> fromParam par pfx kvs

instance (EncodeParam a) => ToParam 'QueryParam (NonNested a) where
  toParam _ pfx (NonNested val) = [(pfx, Just $ encodeParam val)]

instance (EncodeParam a) => ToParam 'FormParam (NonNested a) where
  toParam _ pfx (NonNested val) = [(pfx, encodeParam val)]

instance (EncodeParam a) => ToParam 'Cookie (NonNested a) where
  toParam _ pfx (NonNested val) = [(pfx, defCookieInfo $ encodeParam val)]

instance (DecodeParam a, Typeable a) => FromParam 'QueryParam (NonNested a) where
  fromParam pt key kvs = case lookupParam pt key kvs of
   Just (Just par) -> case decodeParam par of
         Just v -> Validation $ Right $ NonNested v
         _      -> Validation $ Left [ParseErr key $ T.pack $ "Unable to cast to " ++ (show $ typeOf (Proxy :: Proxy a))]
   _ ->  Validation $ Left [NotFound key]

instance (DecodeParam a, Typeable a) => FromParam 'FormParam (NonNested a) where
  fromParam pt key kvs = case lookupParam pt key kvs of
   Just par -> case decodeParam par of
         Just v -> Validation $ Right $ NonNested v
         _      -> Validation $ Left [ParseErr key $ T.pack $ "Unable to cast to " ++ (show $ typeOf (Proxy :: Proxy a))]
   _ ->  Validation $ Left [NotFound key]

instance (DecodeParam a, Typeable a) => FromParam 'Cookie (NonNested a) where
  fromParam pt key kvs = case lookupParam pt key kvs of
   Just par -> case decodeParam par of
         Just v -> Validation $ Right $ NonNested v
         _      -> Validation $ Left [ParseErr key $ T.pack $ "Unable to cast to " ++ (show $ typeOf (Proxy :: Proxy a))]
   _ ->  Validation $ Left [NotFound key]

instance ToParam parK () where
  toParam _ _ _ = []

instance ToHeader () where
  toHeader _ = []

instance ToParam 'QueryParam Unit where
  toParam _ pfx val = [(pfx, Just $ encodeParam val)]

instance ToParam 'FormParam Unit where
  toParam _ pfx val = [(pfx, encodeParam val)]

instance ToParam 'Cookie Unit where
  toParam _ pfx val = [(pfx, defCookieInfo $ encodeParam val)]

instance ToParam 'QueryParam Int where
  toParam _ pfx val = [(pfx, Just $ encodeParam val)]

instance ToParam 'FormParam Int where
  toParam _ pfx val = [(pfx, encodeParam val)]

instance ToParam 'Cookie Int where
  toParam _ pfx val = [(pfx, defCookieInfo $ encodeParam val)]

instance ToParam 'QueryParam Int8 where
  toParam _ pfx val = [(pfx, Just $ encodeParam val)]

instance ToParam 'FormParam Int8 where
  toParam _ pfx val = [(pfx, encodeParam val)]

instance ToParam 'Cookie Int8 where
  toParam _ pfx val = [(pfx, defCookieInfo $ encodeParam val)]

instance ToParam 'QueryParam Int16 where
  toParam _ pfx val = [(pfx, Just $ encodeParam val)]

instance ToParam 'FormParam Int16 where
  toParam _ pfx val = [(pfx, encodeParam val)]

instance ToParam 'Cookie Int16 where
  toParam _ pfx val = [(pfx, defCookieInfo $ encodeParam val)]

instance ToParam 'QueryParam Int32 where
  toParam _ pfx val = [(pfx, Just $ encodeParam val)]

instance ToParam 'FormParam Int32 where
  toParam _ pfx val = [(pfx, encodeParam val)]

instance ToParam 'Cookie Int32 where
  toParam _ pfx val = [(pfx, defCookieInfo $ encodeParam val)]

instance ToParam 'QueryParam Int64 where
  toParam _ pfx val = [(pfx, Just $ encodeParam val)]

instance ToParam 'FormParam Int64 where
  toParam _ pfx val = [(pfx, encodeParam val)]

instance ToParam 'Cookie Int64 where
  toParam _ pfx val = [(pfx, defCookieInfo $ encodeParam val)]

instance ToParam 'QueryParam Word where
  toParam _ pfx val = [(pfx, Just $ encodeParam val)]

instance ToParam 'FormParam Word where
  toParam _ pfx val = [(pfx, encodeParam val)]

instance ToParam 'Cookie Word where
  toParam _ pfx val = [(pfx, defCookieInfo $ encodeParam val)]

instance ToParam 'QueryParam Word8 where
  toParam _ pfx val = [(pfx, Just $ encodeParam val)]

instance ToParam 'FormParam Word8 where
  toParam _ pfx val = [(pfx, encodeParam val)]

instance ToParam 'Cookie Word8 where
  toParam _ pfx val = [(pfx, defCookieInfo $ encodeParam val)]

instance ToParam 'QueryParam Word16 where
  toParam _ pfx val = [(pfx, Just $ encodeParam val)]

instance ToParam 'FormParam Word16 where
  toParam _ pfx val = [(pfx, encodeParam val)]

instance ToParam 'Cookie Word16 where
  toParam _ pfx val = [(pfx, defCookieInfo $ encodeParam val)]

instance ToParam 'QueryParam Word32 where
  toParam _ pfx val = [(pfx, Just $ encodeParam val)]

instance ToParam 'FormParam Word32 where
  toParam _ pfx val = [(pfx, encodeParam val)]

instance ToParam 'Cookie Word32 where
  toParam _ pfx val = [(pfx, defCookieInfo $ encodeParam val)]

instance ToParam 'QueryParam Word64 where
  toParam _ pfx val = [(pfx, Just $ encodeParam val)]

instance ToParam 'FormParam Word64 where
  toParam _ pfx val = [(pfx, encodeParam val)]

instance ToParam 'Cookie Word64 where
  toParam _ pfx val = [(pfx, defCookieInfo $ encodeParam val)]

instance ToParam 'QueryParam Integer where
  toParam _ pfx val = [(pfx, Just $ encodeParam val)]

instance ToParam 'FormParam Integer where
  toParam _ pfx val = [(pfx, encodeParam val)]

instance ToParam 'Cookie Integer where
  toParam _ pfx val = [(pfx, defCookieInfo $ encodeParam val)]

instance ToParam 'QueryParam Bool where
  toParam _ pfx val = [(pfx, Just $ encodeParam val)]

instance ToParam 'FormParam Bool where
  toParam _ pfx val = [(pfx, encodeParam val)]

instance ToParam 'Cookie Bool where
  toParam _ pfx val = [(pfx, defCookieInfo $ encodeParam val)]

instance ToParam 'QueryParam Double where
  toParam _ pfx val = [(pfx, Just $ encodeParam val)]

instance ToParam 'FormParam Double where
  toParam _ pfx val = [(pfx, encodeParam val)]

instance ToParam 'Cookie Double where
  toParam _ pfx val = [(pfx, defCookieInfo $ encodeParam val)]

instance ToParam 'QueryParam Float where
  toParam _ pfx val = [(pfx, Just $ encodeParam val)]

instance ToParam 'FormParam Float where
  toParam _ pfx val = [(pfx, encodeParam val)]

instance ToParam 'Cookie Float where
  toParam _ pfx val = [(pfx, defCookieInfo $ encodeParam val)]

instance ToParam 'QueryParam Char where
  toParam _ pfx val = [(pfx, Just $ encodeParam val)]

instance ToParam 'FormParam Char where
  toParam _ pfx val = [(pfx, encodeParam val)]

instance ToParam 'Cookie Char where
  toParam _ pfx val = [(pfx, defCookieInfo $ encodeParam val)]

instance ToParam 'QueryParam T.Text where
  toParam _ pfx val = [(pfx, Just $ encodeParam val)]

instance ToParam 'FormParam T.Text where
  toParam _ pfx val = [(pfx, encodeParam val)]

instance ToParam 'Cookie T.Text where
  toParam _ pfx val = [(pfx, defCookieInfo $ encodeParam val)]

instance ToParam 'QueryParam ByteString where
  toParam _ pfx val = [(pfx, Just $ val)]

instance ToParam 'FormParam ByteString where
  toParam _ pfx val = [(pfx, val)]

instance ToParam 'Cookie ByteString where
  toParam _ pfx val = [(pfx, defCookieInfo $ val)]

instance ToParam 'QueryParam Day where
  toParam _ pfx val = [(pfx, Just $ encodeParam val)]

instance ToParam 'FormParam Day where
  toParam _ pfx val = [(pfx, encodeParam val)]

instance ToParam 'Cookie Day where
  toParam _ pfx val = [(pfx, defCookieInfo $ encodeParam val)]

instance ToParam 'QueryParam UTCTime where
  toParam _ pfx val = [(pfx, Just $ encodeParam val)]

instance ToParam 'FormParam UTCTime where
  toParam _ pfx val = [(pfx, encodeParam val)]

instance ToParam 'Cookie UTCTime where
  toParam _ pfx val = [(pfx, defCookieInfo $ encodeParam val)]

instance ToParam 'QueryParam LocalTime where
  toParam _ pfx val = [(pfx, Just $ encodeParam val)]

instance ToParam 'FormParam LocalTime where
  toParam _ pfx val = [(pfx, encodeParam val)]

instance ToParam 'Cookie LocalTime where
  toParam _ pfx val = [(pfx, defCookieInfo $ encodeParam val)]

instance ToParam 'QueryParam TimeOfDay where
  toParam _ pfx val = [(pfx, Just $ encodeParam val)]

instance ToParam 'FormParam TimeOfDay where
  toParam _ pfx val = [(pfx, encodeParam val)]

instance ToParam 'Cookie TimeOfDay where
  toParam _ pfx val = [(pfx, defCookieInfo $ encodeParam val)]

instance (ToParam 'Cookie a) => ToParam 'Cookie (CookieInfo a) where
  toParam p pfx val = Prelude.map (\(k, v) -> (k, val { cookieValue = cookieValue v })) $ toParam p pfx (cookieValue val)

instance (EncodeParam a) => ToParam 'QueryParam (OptValue a) where
  toParam _ pfx (OptValue (Just val)) = [(pfx, Just $ encodeParam val)]
  toParam _ pfx (OptValue Nothing)    = [(pfx, Nothing)]

instance (EncodeParam a) => ToParam 'FormParam (OptValue a) where
  toParam _ pfx (OptValue (Just val)) = [(pfx, encodeParam val)]
  toParam _ _ (OptValue Nothing)     = []

instance (EncodeParam a) => ToParam 'Cookie (OptValue a) where
  toParam _ pfx (OptValue (Just val)) = [(pfx, defCookieInfo $ encodeParam val)]
  toParam _ _ (OptValue Nothing)     = []

instance (ToJSON a) => ToParam 'QueryParam (JsonOf a) where
  toParam _ pfx val = [(pfx, Just $ encodeParam val)]

instance (ToJSON a) => ToParam 'FormParam (JsonOf a) where
  toParam _ pfx val = [(pfx, encodeParam val)]

instance (ToJSON a) => ToParam 'Cookie (JsonOf a) where
  toParam _ pfx val = [(pfx, defCookieInfo $ encodeParam val)]

instance (EncodeParam a, KnownSymbol del) => ToParam 'QueryParam (DelimitedCollection del a) where
  toParam _ pfx val = [(pfx, Just $ encodeParam val)]

instance (EncodeParam a, KnownSymbol del) => ToParam 'FormParam (DelimitedCollection del a) where
  toParam _ pfx val = [(pfx, encodeParam val)]

instance (EncodeParam a, KnownSymbol del) => ToParam 'Cookie (DelimitedCollection del a) where
  toParam _ pfx val = [(pfx, defCookieInfo $ encodeParam val)]  

instance ToParam par a => ToParam par (Maybe a) where
  toParam pt pfx (Just val) = toParam pt pfx val
  toParam _ _ Nothing      = []

instance (ToParam par a, ToParam par b) => ToParam par (Either a b) where
  toParam pt pfx (Left e)  = toParam pt (pfx `nest` "Left") e
  toParam pt pfx (Right v) = toParam pt (pfx `nest` "Right") v

instance ToParam par a => ToParam par [a] where
  toParam pt pfx vals = Prelude.concatMap (\(ix, v) -> toParam pt (pfx `nest` (ASCII.pack $ show ix)) v) $ Prelude.zip [(0 :: Word)..] vals

instance ToParam par a => ToParam par (Vector a) where
  toParam pt pfx vals = toParam pt pfx (V.toList vals)

instance EncodeParam a => ToParam 'QueryParam (Set a) where
  toParam _ pfx vals = fmap (\v -> (pfx, Just $ encodeParam v)) $ Set.toList vals

instance EncodeParam a => ToParam 'FormParam (Set a) where
  toParam _ pfx vals = fmap (\v -> (pfx, encodeParam v)) $ Set.toList vals

instance EncodeParam a => ToParam 'Cookie (Set a) where
  toParam _ pfx vals = fmap (\v -> (pfx, defCookieInfo $ encodeParam v)) $ Set.toList vals

instance EncodeParam a => ToParam 'QueryParam (MultiSet a) where
  toParam _ pfx vals = fmap (\v -> (pfx, Just $ encodeParam v)) $ MultiSet.toList vals

instance EncodeParam a => ToParam 'FormParam (MultiSet a) where
  toParam _ pfx vals = fmap (\v -> (pfx, encodeParam v)) $ MultiSet.toList vals

instance EncodeParam a => ToParam 'Cookie (MultiSet a) where
  toParam _ pfx vals = fmap (\v -> (pfx, defCookieInfo $ encodeParam v)) $ MultiSet.toList vals  

instance (FromJSON a) => FromParam 'QueryParam (JsonOf a) where
  fromParam pt key kvs = case lookupParam pt key kvs of
    Just (Just par) -> case decodeParam par of
      Just v -> Validation $ Right v
      _      -> Validation $ Left [ParseErr key "Unable to decode from json"]
    _ -> Validation $ Left [NotFound key]

instance (FromJSON a) => FromParam 'FormParam (JsonOf a) where
  fromParam pt key kvs = case lookupParam pt key kvs of
    Just par -> case decodeParam par of
      Just v -> Validation $ Right v
      _      -> Validation $ Left [ParseErr key "Unable to decode from json"]
    _ -> Validation $ Left [NotFound key]

instance (DecodeParam a, KnownSymbol del) => FromParam 'QueryParam (DelimitedCollection del a) where
  fromParam pt key kvs = case lookupParam pt key kvs of
    Just (Just par) -> case decodeParam par of
      Just v -> Validation $ Right v
      _      -> Validation $ Left [ParseErr key "Unable to decode from DelimitedCollection"]
    _ -> Validation $ Left [NotFound key]

instance (DecodeParam a, KnownSymbol del) => FromParam 'FormParam (DelimitedCollection del a) where
  fromParam pt key kvs = case lookupParam pt key kvs of
    Just par -> case decodeParam par of
      Just v -> Validation $ Right v
      _      -> Validation $ Left [ParseErr key "Unable to decode from DelimitedCollection"]
    _ -> Validation $ Left [NotFound key]

instance (DecodeParam a, KnownSymbol del) => FromParam 'Cookie (DelimitedCollection del a) where
  fromParam pt key kvs = case lookupParam pt key kvs of
    Just par -> case decodeParam par of
      Just v -> Validation $ Right v
      _      -> Validation $ Left [ParseErr key "Unable to decode from DelimitedCollection"]
    _ -> Validation $ Left [NotFound key]       

instance FromParam parK () where
  fromParam _ _ _ = pure ()

instance FromHeader () where
  fromHeader _ = pure ()

instance FromParam 'QueryParam Unit where
  fromParam pt key kvs = case lookupParam pt key kvs of
   Just (Just par) -> case decodeParam par of
         Just v -> Validation $ Right v
         _      -> Validation $ Left [ParseErr key "Unable to cast to NullaryConstructor"]
   _ ->  Validation $ Left [NotFound key]

instance FromParam 'FormParam Unit where
  fromParam pt key kvs = case lookupParam pt key kvs of
   Just par -> case decodeParam par of
         Just v -> Validation $ Right v
         _      -> Validation $ Left [ParseErr key "Unable to cast to NullaryConstructor"]
   _ ->  Validation $ Left [NotFound key]

instance FromParam 'Cookie Unit where
  fromParam pt key kvs = case lookupParam pt key kvs of
   Just par -> case decodeParam par of
         Just v -> Validation $ Right v
         _      -> Validation $ Left [ParseErr key "Unable to cast to NullaryConstructor"]
   _ ->  Validation $ Left [NotFound key]

instance FromParam 'QueryParam Bool where
  fromParam pt key kvs = case lookupParam pt key kvs of
   Just (Just par) -> case decodeParam par of
         Just v -> Validation $ Right v
         _      -> Validation $ Left [ParseErr key "Unable to cast to Bool"]
   _ ->  Validation $ Left [NotFound key]

instance FromParam 'FormParam Bool where
  fromParam pt key kvs = case lookupParam pt key kvs of
   Just par -> case decodeParam par of
         Just v -> Validation $ Right v
         _      -> Validation $ Left [ParseErr key "Unable to cast to Bool"]
   _ ->  Validation $ Left [NotFound key]

instance FromParam 'Cookie Bool where
  fromParam pt key kvs = case lookupParam pt key kvs of
   Just par -> case decodeParam par of
         Just v -> Validation $ Right v
         _      -> Validation $ Left [ParseErr key "Unable to cast to Bool"]
   _ ->  Validation $ Left [NotFound key]

instance FromParam 'QueryParam Char where
  fromParam pt key kvs = case lookupParam pt key kvs of
   Just (Just par) -> case decodeParam par of
         Just v -> Validation $ Right v
         _      -> Validation $ Left [ParseErr key "Unable to cast to Char"]
   _ ->  Validation $ Left [NotFound key]

instance FromParam 'FormParam Char where
  fromParam pt key kvs = case lookupParam pt key kvs of
   Just par -> case decodeParam par of
         Just v -> Validation $ Right v
         _      -> Validation $ Left [ParseErr key "Unable to cast to Char"]
   _ ->  Validation $ Left [NotFound key]

instance FromParam 'Cookie Char where
  fromParam pt key kvs = case lookupParam pt key kvs of
   Just par -> case decodeParam par of
         Just v -> Validation $ Right v
         _      -> Validation $ Left [ParseErr key "Unable to cast to Char"]
   _ ->  Validation $ Left [NotFound key]

instance FromParam 'QueryParam UTCTime where
  fromParam pt key kvs = case lookupParam pt key kvs of
   Just (Just par) -> case decodeParam par of
         Just v -> Validation $ Right v
         _      -> Validation $ Left [ParseErr key "Unable to cast to UTCTime (ISO-8601)"]
   _ ->  Validation $ Left [NotFound key]

instance FromParam 'FormParam UTCTime where
  fromParam pt key kvs = case lookupParam pt key kvs of
   Just par -> case decodeParam par of
         Just v -> Validation $ Right v
         _      -> Validation $ Left [ParseErr key "Unable to cast to UTCTime (ISO-8601)"]
   _ ->  Validation $ Left [NotFound key]

instance FromParam 'Cookie UTCTime where
  fromParam pt key kvs = case lookupParam pt key kvs of
   Just par -> case decodeParam par of
         Just v -> Validation $ Right v
         _      -> Validation $ Left [ParseErr key "Unable to cast to UTCTime (ISO-8601)"]
   _ ->  Validation $ Left [NotFound key]

instance FromParam 'QueryParam LocalTime where
  fromParam pt key kvs = case lookupParam pt key kvs of
   Just (Just par) -> case decodeParam par of
         Just v -> Validation $ Right v
         _      -> Validation $ Left [ParseErr key "Unable to cast to LocalTime (ISO-8601)"]
   _ ->  Validation $ Left [NotFound key]

instance FromParam 'FormParam LocalTime where
  fromParam pt key kvs = case lookupParam pt key kvs of
   Just par -> case decodeParam par of
         Just v -> Validation $ Right v
         _      -> Validation $ Left [ParseErr key "Unable to cast to LocalTime (ISO-8601)"]
   _ ->  Validation $ Left [NotFound key]

instance FromParam 'Cookie LocalTime where
  fromParam pt key kvs = case lookupParam pt key kvs of
   Just par -> case decodeParam par of
         Just v -> Validation $ Right v
         _      -> Validation $ Left [ParseErr key "Unable to cast to LocalTime (ISO-8601)"]
   _ ->  Validation $ Left [NotFound key]

instance FromParam 'QueryParam TimeOfDay where
  fromParam pt key kvs = case lookupParam pt key kvs of
   Just (Just par) -> case decodeParam par of
         Just v -> Validation $ Right v
         _      -> Validation $ Left [ParseErr key "Unable to cast to TimeOfDay (ISO-8601)"]
   _ ->  Validation $ Left [NotFound key]

instance FromParam 'FormParam TimeOfDay where
  fromParam pt key kvs = case lookupParam pt key kvs of
   Just par -> case decodeParam par of
         Just v -> Validation $ Right v
         _      -> Validation $ Left [ParseErr key "Unable to cast to TimeOfDay (ISO-8601)"]
   _ ->  Validation $ Left [NotFound key]

instance FromParam 'Cookie TimeOfDay where
  fromParam pt key kvs = case lookupParam pt key kvs of
   Just par -> case decodeParam par of
         Just v -> Validation $ Right v
         _      -> Validation $ Left [ParseErr key "Unable to cast to TimeOfDay (ISO-8601)"]
   _ ->  Validation $ Left [NotFound key]

instance FromParam 'QueryParam Int where
  fromParam pt key kvs = case lookupParam pt key kvs of
   Just (Just par) -> case decodeParam par of
     Just v -> Validation $ Right v
     _      -> Validation $ Left [ParseErr key "Unable to cast to Int"]
   _ ->  Validation $ Left [NotFound key]

instance FromParam 'FormParam Int where
  fromParam pt key kvs = case lookupParam pt key kvs of
   Just par -> case decodeParam par of
     Just v -> Validation $ Right v
     _      -> Validation $ Left [ParseErr key "Unable to cast to Int"]
   _ ->  Validation $ Left [NotFound key]

instance FromParam 'Cookie Int where
  fromParam pt key kvs = case lookupParam pt key kvs of
   Just par -> case decodeParam par of
     Just v -> Validation $ Right v
     _      -> Validation $ Left [ParseErr key "Unable to cast to Int"]
   _ ->  Validation $ Left [NotFound key]

instance FromParam 'QueryParam Int8 where
  fromParam pt key kvs = case lookupParam pt key kvs of
   Just (Just par) -> case decodeParam par of
     Just v -> Validation $ Right v
     _      -> Validation $ Left [ParseErr key "Unable to cast to Int8"]
   _ ->  Validation $ Left [NotFound key]

instance FromParam 'FormParam Int8 where
  fromParam pt key kvs = case lookupParam pt key kvs of
   Just par -> case decodeParam par of
     Just v -> Validation $ Right v
     _      -> Validation $ Left [ParseErr key "Unable to cast to Int8"]
   _ ->  Validation $ Left [NotFound key]

instance FromParam 'Cookie Int8 where
  fromParam pt key kvs = case lookupParam pt key kvs of
   Just par -> case decodeParam par of
     Just v -> Validation $ Right v
     _      -> Validation $ Left [ParseErr key "Unable to cast to Int8"]
   _ ->  Validation $ Left [NotFound key]

instance FromParam 'QueryParam Int16 where
  fromParam pt key kvs = case lookupParam pt key kvs of
   Just (Just par) -> case decodeParam par of
     Just v -> Validation $ Right v
     _      -> Validation $ Left [ParseErr key "Unable to cast to Int16"]
   _ ->  Validation $ Left [NotFound key]

instance FromParam 'FormParam Int16 where
  fromParam pt key kvs = case lookupParam pt key kvs of
   Just par -> case decodeParam par of
     Just v -> Validation $ Right v
     _      -> Validation $ Left [ParseErr key "Unable to cast to Int16"]
   _ ->  Validation $ Left [NotFound key]

instance FromParam 'Cookie Int16 where
  fromParam pt key kvs = case lookupParam pt key kvs of
   Just par -> case decodeParam par of
     Just v -> Validation $ Right v
     _      -> Validation $ Left [ParseErr key "Unable to cast to Int16"]
   _ ->  Validation $ Left [NotFound key]

instance FromParam 'QueryParam Int32 where
  fromParam pt key kvs = case lookupParam pt key kvs of
   Just (Just par) -> case decodeParam par of
     Just v -> Validation $ Right v
     _      -> Validation $ Left [ParseErr key "Unable to cast to Int32"]
   _ ->  Validation $ Left [NotFound key]

instance FromParam 'FormParam Int32 where
  fromParam pt key kvs = case lookupParam pt key kvs of
   Just par -> case decodeParam par of
     Just v -> Validation $ Right v
     _      -> Validation $ Left [ParseErr key "Unable to cast to Int32"]
   _ ->  Validation $ Left [NotFound key]

instance FromParam 'Cookie Int32 where
  fromParam pt key kvs = case lookupParam pt key kvs of
   Just par -> case decodeParam par of
     Just v -> Validation $ Right v
     _      -> Validation $ Left [ParseErr key "Unable to cast to Int32"]
   _ ->  Validation $ Left [NotFound key]

instance FromParam 'QueryParam Int64 where
  fromParam pt key kvs = case lookupParam pt key kvs of
   Just (Just par) -> case decodeParam par of
     Just v -> Validation $ Right v
     _      -> Validation $ Left [ParseErr key "Unable to cast to Int64"]
   _ ->  Validation $ Left [NotFound key]

instance FromParam 'FormParam Int64 where
  fromParam pt key kvs = case lookupParam pt key kvs of
   Just par -> case decodeParam par of
     Just v -> Validation $ Right v
     _      -> Validation $ Left [ParseErr key "Unable to cast to Int64"]
   _ ->  Validation $ Left [NotFound key]

instance FromParam 'Cookie Int64 where
  fromParam pt key kvs = case lookupParam pt key kvs of
   Just par -> case decodeParam par of
     Just v -> Validation $ Right v
     _      -> Validation $ Left [ParseErr key "Unable to cast to Int64"]
   _ ->  Validation $ Left [NotFound key]

instance FromParam 'QueryParam Integer where
  fromParam pt key kvs = case lookupParam pt key kvs of
   Just (Just par) -> case decodeParam par of
     Just v -> Validation $ Right v
     _      -> Validation $ Left [ParseErr key "Unable to cast to Int64"]
   _ ->  Validation $ Left [NotFound key]

instance FromParam 'FormParam Integer where
  fromParam pt key kvs = case lookupParam pt key kvs of
   Just par -> case decodeParam par of
     Just v -> Validation $ Right v
     _      -> Validation $ Left [ParseErr key "Unable to cast to Int64"]
   _ ->  Validation $ Left [NotFound key]

instance FromParam 'Cookie Integer where
  fromParam pt key kvs = case lookupParam pt key kvs of
   Just par -> case decodeParam par of
     Just v -> Validation $ Right v
     _      -> Validation $ Left [ParseErr key "Unable to cast to Int64"]
   _ ->  Validation $ Left [NotFound key]

instance FromParam 'QueryParam Word where
  fromParam pt key kvs = case lookupParam pt key kvs of
   Just (Just par) -> case decodeParam par of
         Just v -> Validation $ Right v
         _      -> Validation $ Left [ParseErr key "Unable to cast to Word"]
   _ ->  Validation $ Left [NotFound key]

instance FromParam 'FormParam Word where
  fromParam pt key kvs = case lookupParam pt key kvs of
   Just par -> case decodeParam par of
         Just v -> Validation $ Right v
         _      -> Validation $ Left [ParseErr key "Unable to cast to Word"]
   _ ->  Validation $ Left [NotFound key]

instance FromParam 'Cookie Word where
  fromParam pt key kvs = case lookupParam pt key kvs of
   Just par -> case decodeParam par of
         Just v -> Validation $ Right v
         _      -> Validation $ Left [ParseErr key "Unable to cast to Word"]
   _ ->  Validation $ Left [NotFound key]

instance FromParam 'QueryParam Word8 where
  fromParam pt key kvs = case lookupParam pt key kvs of
   Just (Just par) -> case decodeParam par of
     Just v -> Validation $ Right v
     _      -> Validation $ Left [ParseErr key "Unable to cast to Word8"]
   _ ->  Validation $ Left [NotFound key]

instance FromParam 'FormParam Word8 where
  fromParam pt key kvs = case lookupParam pt key kvs of
   Just par -> case decodeParam par of
     Just v -> Validation $ Right v
     _      -> Validation $ Left [ParseErr key "Unable to cast to Word8"]
   _ ->  Validation $ Left [NotFound key]

instance FromParam 'Cookie Word8 where
  fromParam pt key kvs = case lookupParam pt key kvs of
   Just par -> case decodeParam par of
     Just v -> Validation $ Right v
     _      -> Validation $ Left [ParseErr key "Unable to cast to Word8"]
   _ ->  Validation $ Left [NotFound key]

instance FromParam 'QueryParam Word16 where
  fromParam pt key kvs = case lookupParam pt key kvs of
   Just (Just par) -> case decodeParam par of
     Just v -> Validation $ Right v
     _      -> Validation $ Left [ParseErr key "Unable to cast to Word16"]
   _ ->  Validation $ Left [NotFound key]

instance FromParam 'FormParam Word16 where
  fromParam pt key kvs = case lookupParam pt key kvs of
   Just par -> case decodeParam par of
     Just v -> Validation $ Right v
     _      -> Validation $ Left [ParseErr key "Unable to cast to Word16"]
   _ ->  Validation $ Left [NotFound key]

instance FromParam 'Cookie Word16 where
  fromParam pt key kvs = case lookupParam pt key kvs of
   Just par -> case decodeParam par of
     Just v -> Validation $ Right v
     _      -> Validation $ Left [ParseErr key "Unable to cast to Word16"]
   _ ->  Validation $ Left [NotFound key]

instance FromParam 'QueryParam Word32 where
  fromParam pt key kvs = case lookupParam pt key kvs of
   Just (Just par) -> case decodeParam par of
     Just v -> Validation $ Right v
     _      -> Validation $ Left [ParseErr key "Unable to cast to Word32"]
   _ ->  Validation $ Left [NotFound key]

instance FromParam 'FormParam Word32 where
  fromParam pt key kvs = case lookupParam pt key kvs of
   Just par -> case decodeParam par of
     Just v -> Validation $ Right v
     _      -> Validation $ Left [ParseErr key "Unable to cast to Word32"]
   _ ->  Validation $ Left [NotFound key]

instance FromParam 'Cookie Word32 where
  fromParam pt key kvs = case lookupParam pt key kvs of
   Just par -> case decodeParam par of
     Just v -> Validation $ Right v
     _      -> Validation $ Left [ParseErr key "Unable to cast to Word32"]
   _ ->  Validation $ Left [NotFound key]

instance FromParam 'QueryParam Word64 where
  fromParam pt key kvs = case lookupParam pt key kvs of
   Just (Just par) -> case decodeParam par of
     Just v -> Validation $ Right v
     _      -> Validation $ Left [ParseErr key "Unable to cast to Word64"]
   _ ->  Validation $ Left [NotFound key]

instance FromParam 'FormParam Word64 where
  fromParam pt key kvs = case lookupParam pt key kvs of
   Just par -> case decodeParam par of
     Just v -> Validation $ Right v
     _      -> Validation $ Left [ParseErr key "Unable to cast to Word64"]
   _ ->  Validation $ Left [NotFound key]

instance FromParam 'Cookie Word64 where
  fromParam pt key kvs = case lookupParam pt key kvs of
   Just par -> case decodeParam par of
     Just v -> Validation $ Right v
     _      -> Validation $ Left [ParseErr key "Unable to cast to Word64"]
   _ ->  Validation $ Left [NotFound key]

instance FromParam 'QueryParam Double where
  fromParam pt key kvs = case lookupParam pt key kvs of
   Just (Just par) -> case decodeParam par of
         Just v -> Validation $ Right v
         _      -> Validation $ Left [ParseErr key "Unable to cast to Double"]
   _ ->  Validation $ Left [NotFound key]

instance FromParam 'FormParam Double where
  fromParam pt key kvs = case lookupParam pt key kvs of
   Just par -> case decodeParam par of
         Just v -> Validation $ Right v
         _      -> Validation $ Left [ParseErr key "Unable to cast to Double"]
   _ ->  Validation $ Left [NotFound key]

instance FromParam 'Cookie Double where
  fromParam pt key kvs = case lookupParam pt key kvs of
   Just par -> case decodeParam par of
         Just v -> Validation $ Right v
         _      -> Validation $ Left [ParseErr key "Unable to cast to Double"]
   _ ->  Validation $ Left [NotFound key]

instance FromParam 'QueryParam Float where
  fromParam pt key kvs = case lookupParam pt key kvs of
   Just (Just par) -> case decodeParam par of
         Just v -> Validation $ Right v
         _      -> Validation $ Left [ParseErr key "Unable to cast to Float"]
   _ ->  Validation $ Left [NotFound key]

instance FromParam 'FormParam Float where
  fromParam pt key kvs = case lookupParam pt key kvs of
   Just par -> case decodeParam par of
         Just v -> Validation $ Right v
         _      -> Validation $ Left [ParseErr key "Unable to cast to Float"]
   _ ->  Validation $ Left [NotFound key]

instance FromParam 'Cookie Float where
  fromParam pt key kvs = case lookupParam pt key kvs of
   Just par -> case decodeParam par of
         Just v -> Validation $ Right v
         _      -> Validation $ Left [ParseErr key "Unable to cast to Float"]
   _ ->  Validation $ Left [NotFound key]

instance FromParam 'QueryParam ByteString where
  fromParam pt key kvs = case lookupParam pt key kvs of
   Just (Just par) -> case decodeParam par of
         Just v -> Validation $ Right v
         _      -> Validation $ Left [ParseErr key "Unable to cast to ByteString"]
   _ ->  Validation $ Left [NotFound key]

instance FromParam 'FormParam ByteString where
  fromParam pt key kvs = case lookupParam pt key kvs of
   Just par -> case decodeParam par of
         Just v -> Validation $ Right v
         _      -> Validation $ Left [ParseErr key "Unable to cast to ByteString"]
   _ ->  Validation $ Left [NotFound key]

instance FromParam 'Cookie ByteString where
  fromParam pt key kvs = case lookupParam pt key kvs of
   Just par -> case decodeParam par of
         Just v -> Validation $ Right v
         _      -> Validation $ Left [ParseErr key "Unable to cast to ByteString"]
   _ ->  Validation $ Left [NotFound key]

instance FromParam par a => FromParam par (Maybe a) where
  fromParam pt key kvs = case Trie.null kvs' of
    True  ->  Validation $ Right Nothing
    False -> case (fromParam pt key kvs' :: Validation [ParamErr] a) of
      Validation (Right val) -> Validation $ Right $ Just val
      Validation (Left errs) -> Validation $ Left errs
    where kvs' = submap key kvs

instance (FromParam par a, FromParam par b) => FromParam par (Either a b) where
  fromParam pt key kvs = case Trie.null kvsL of
    True -> case Trie.null kvsR of
      True -> Validation $ Left [ParseErr key "Unable to cast to Either"]
      False -> Right <$> fromParam pt keyR kvsR
    False -> Left <$> fromParam pt keyL kvsL
    where kvsL = submap keyL kvs
          kvsR = submap keyR kvs
          keyL = (key `nest` "Left")
          keyR = (key `nest` "Right")

instance FromParam 'QueryParam T.Text where
  fromParam pt key kvs = case lookupParam pt key kvs of
   Just (Just par) -> case decodeParam par of
     Just v -> Validation $ Right v
     _      -> Validation $ Left [ParseErr key "Unable to cast to Text"]
   _ ->  Validation $ Left [NotFound key]

instance FromParam 'FormParam T.Text where
  fromParam pt key kvs = case lookupParam pt key kvs of
   Just par -> case decodeParam par of
     Just v -> Validation $ Right v
     _      -> Validation $ Left [ParseErr key "Unable to cast to Text"]
   _ ->  Validation $ Left [NotFound key]

instance FromParam 'Cookie T.Text where
  fromParam pt key kvs = case lookupParam pt key kvs of
   Just par -> case decodeParam par of
     Just v -> Validation $ Right v
     _      -> Validation $ Left [ParseErr key "Unable to cast to Text"]
   _ ->  Validation $ Left [NotFound key]

instance FromParam 'QueryParam Day where
  fromParam pt key kvs = case lookupParam pt key kvs of
   Just (Just par) -> case decodeParam par of
     Just v -> Validation $ Right v
     _      -> Validation $ Left [ParseErr key "Unable to cast to Day"]
   _ ->  Validation $ Left [NotFound key]

instance FromParam 'FormParam Day where
  fromParam pt key kvs = case lookupParam pt key kvs of
   Just par -> case decodeParam par of
     Just v -> Validation $ Right v
     _      -> Validation $ Left [ParseErr key "Unable to cast to Day"]
   _ ->  Validation $ Left [NotFound key]

instance FromParam 'Cookie Day where
  fromParam pt key kvs = case lookupParam pt key kvs of
   Just par -> case decodeParam par of
     Just v -> Validation $ Right v
     _      -> Validation $ Left [ParseErr key "Unable to cast to Day"]
   _ ->  Validation $ Left [NotFound key]

instance (FromParam par a) => FromParam par [a] where
  fromParam pt key kvs = case Trie.null kvs' of
    True  ->  Validation $ Right []
    False ->
      let pars = Prelude.map (\(nkey, kv) -> fromParam pt nkey kv :: Validation [ParamErr] a) kvitems
      in catValidations pars
    where kvs' = submap key kvs
          kvitems = Prelude.takeWhile (not . Prelude.null . snd)  (Prelude.map (\ix ->
            let ixkey = key `nest` (ASCII.pack $ show ix)
            in (ixkey, submap ixkey kvs')) [(0 :: Word) .. 2000])

catValidations :: Foldable c => c (Validation [e] a) -> Validation [e] [a]
catValidations res = Prelude.reverse <$> Fold.foldl' accRes (Validation $ Right []) res
  where
    accRes acc elemt = case (acc, elemt) of
      (Validation (Right as), Validation (Right e)) -> Validation $ Right (e:as)
      (Validation (Left as), Validation (Right _)) -> Validation $ Left as
      (Validation (Right _), Validation (Left es)) -> Validation $ Left es
      (Validation (Left as), Validation (Left es)) -> Validation $ Left (es ++ as)

instance (FromParam par a) => FromParam par (Vector a) where
  fromParam pt key kvs = case fromParam pt key kvs of
    Validation (Right v)  -> Validation $ Right (V.fromList v)
    Validation (Left e) -> Validation (Left e)

fromParamToSetLike :: forall a.(DecodeParam a) => ByteString -> [ByteString] -> Validation [ParamErr] [a]
fromParamToSetLike _ [] = Validation $ Right []
fromParamToSetLike key kvs' = 
  let pars = Prelude.map (\(ix, kv) -> case decodeParam kv :: Maybe a of
                             Just v -> Validation $ Right v
                             Nothing -> Validation $ Left [ParseErr key $ "Unable to cast to Set elem at index: " <> (T.pack $ show ix)]
                         ) (Prelude.zip [(1 :: Word) .. ] kvs')
  in catValidations pars

instance (DecodeParam a, Ord a) => FromParam 'QueryParam (Set a) where
  fromParam _ key kvs = Set.fromList <$> fromParamToSetLike key (catMaybes $ Trie.elems $ submap key kvs)
  
instance (DecodeParam a, Ord a) => FromParam 'FormParam (Set a) where
  fromParam _ key kvs = Set.fromList <$> fromParamToSetLike key (Trie.elems $ submap key kvs)

instance (DecodeParam a, Ord a) => FromParam 'Cookie (Set a) where
  fromParam _ key kvs = Set.fromList <$> fromParamToSetLike key (Trie.elems $ submap key kvs)

instance (DecodeParam a, Ord a) => FromParam 'QueryParam (MultiSet a) where
  fromParam _ key kvs = MultiSet.fromList <$> fromParamToSetLike key (catMaybes $ Trie.elems $ submap key kvs)
  
instance (DecodeParam a, Ord a) => FromParam 'FormParam (MultiSet a) where
  fromParam _ key kvs = MultiSet.fromList <$> fromParamToSetLike key (Trie.elems $ submap key kvs)

instance (DecodeParam a, Ord a) => FromParam 'Cookie (MultiSet a) where
  fromParam _ key kvs = MultiSet.fromList <$> fromParamToSetLike key (Trie.elems $ submap key kvs)  
  
instance (DecodeParam a) => FromParam 'QueryParam (OptValue a) where
  fromParam pt key kvs = case lookupParam pt key kvs of
   Just (Just par) -> case decodeParam par of
     Just v     -> Validation $ Right $ OptValue $ Just v
     _          -> Validation $ Left [ParseErr key "Unable to cast to OptValue"]
   Just Nothing -> Validation $ Right $ OptValue Nothing
   _            -> Validation $ Left [NotFound key]

instance (DecodeParam a) => FromParam 'FormParam (OptValue a) where
  fromParam pt key kvs = case lookupParam pt key kvs of
   Just par -> case decodeParam par of
     Just v -> Validation $ Right $ OptValue $ Just v
     _      -> Validation $ Left [ParseErr key "Unable to cast to OptValue"]
   _        -> Validation $ Left [NotFound key]

instance (DecodeParam a) => FromParam 'Cookie (OptValue a) where
  fromParam pt key kvs = case lookupParam pt key kvs of
   Just par -> case decodeParam par of
     Just v -> Validation $ Right $ OptValue $ Just v
     _      -> Validation $ Left [ParseErr key "Unable to cast to OptValue"]
   _        -> Validation $ Left [NotFound key]

instance (FromParam 'Cookie a) => FromParam 'Cookie (CookieInfo a) where
  fromParam pt key kvs = case (fromParam pt key kvs :: Validation [ParamErr] a) of
    Validation (Right val) -> Validation $ Right $ defCookieInfo val
    Validation (Left errs) -> Validation $ Left errs


instance ToParam 'FileParam FileInfo where
  toParam _ key val = [(key, val)]

instance FromParam 'FileParam FileInfo where
  fromParam pt key kvs = case lookupParam pt key kvs of
    Just par -> Validation $ Right par
    Nothing  -> Validation $ Left [NotFound key]

instance ToParam 'PathParam ByteString where
  toParam _ _ v = [encodeParam v]

instance ToParam 'PathParam Int where
  toParam _ _ v = [encodeParam v]

instance ToParam 'PathParam Int8 where
  toParam _ _ v = [encodeParam v]

instance ToParam 'PathParam Int16 where
  toParam _ _ v = [encodeParam v]

instance ToParam 'PathParam Int32 where
  toParam _ _ v = [encodeParam v]

instance ToParam 'PathParam Int64 where
  toParam _ _ v = [encodeParam v]

instance ToParam 'PathParam Word where
  toParam _ _ v = [encodeParam v]

instance ToParam 'PathParam Word8 where
  toParam _ _ v = [encodeParam v]

instance ToParam 'PathParam Word16 where
  toParam _ _ v = [encodeParam v]

instance ToParam 'PathParam Word32 where
  toParam _ _ v = [encodeParam v]

instance ToParam 'PathParam Word64 where
  toParam _ _ v = [encodeParam v]

instance ToParam 'PathParam Float where
  toParam _ _ v = [encodeParam v]

instance ToParam 'PathParam Double where
  toParam _ _ v = [encodeParam v]

instance ToParam 'PathParam Char where
  toParam _ _ v = [encodeParam v]

instance ToParam 'PathParam T.Text where
  toParam _ _ v = [encodeParam v]

instance ToParam 'PathParam Day where
  toParam _ _ v = [encodeParam v]

instance ToParam 'PathParam UTCTime where
  toParam _ _ v = [encodeParam v]

instance ToParam 'PathParam Bool where
  toParam _ _ v = [encodeParam v]

instance ToParam 'PathParam Integer where
  toParam _ _ v = [encodeParam v]

instance (ToJSON a) => ToParam 'PathParam (JsonOf a) where
  toParam _ _ v = [encodeParam v]

instance ( EncodeParam a
         , EncodeParam b
         ) => ToParam 'PathParam (a, b) where
  toParam _ _ (a, b) = [encodeParam a, encodeParam b]

instance ( EncodeParam a
         , EncodeParam b
         , EncodeParam c
         ) => ToParam 'PathParam (a, b, c) where
  toParam _ _ (a, b, c) = [ encodeParam a
                          , encodeParam b
                          , encodeParam c
                          ]

instance ( EncodeParam a
         , EncodeParam b
         , EncodeParam c
         , EncodeParam d
         ) => ToParam 'PathParam (a, b, c, d) where
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
         ) => ToParam 'PathParam (a, b, c, d, e) where
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
         ) => ToParam 'PathParam (a, b, c, d, e, f) where
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
         ) => ToParam 'PathParam (a, b, c, d, e, f, g, h) where
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
         ) => ToParam 'PathParam (a, b, c, d, e, f, g, h, i) where
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
         ) => ToParam 'PathParam (a, b, c, d, e, f, g, h, i, j) where
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

instance FromJSON ParamErr where
  parseJSON = A.withObject "ParamErr" $ \obj ->
    (NotFound . encodeUtf8 <$> obj .: "NotFound")
    <|> (mkParseErr <$> obj .: "ParseErr")
    where mkParseErr [key, msg] = ParseErr (encodeUtf8 key) msg
          mkParseErr vals = error $ "Error parsing ParseErr as JSON. ParseErr accepts exactly two arg but recieved " ++ (show $ Prelude.length vals)


-- | Convert the 'ParamErr' that occured during deserialization into 'ApiErr' type which can then be put in 'Response'.
class ParamErrToApiErr apiErr where
  toApiErr :: [ParamErr] -> apiErr

instance ParamErrToApiErr () where
  toApiErr = const ()

instance ParamErrToApiErr T.Text where
  toApiErr errs = T.pack (show errs)

instance ParamErrToApiErr LT.Text where
  toApiErr errs = LT.pack (show errs)

instance ParamErrToApiErr A.Value where
  toApiErr errs = toJSON errs

-- | Nest the key with a prefix.
--
-- > nest "pfx" "key" == "pfx.key"
-- > nest "" "key" == "key"
nest :: ByteString -> ByteString -> ByteString
nest s1 s2 | SB.null s1 = s2
           | otherwise = SB.concat [s1, ".", s2]

-- | Lookup a value from the 'Trie' using the given key.
lookupParam :: Proxy (parK :: ParamK) -> ByteString -> Trie (DeSerializedData parK) -> Maybe (DeSerializedData parK)
lookupParam _ key kvs = Trie.lookup key kvs

data ParamAcc = ParamAcc { index :: Int, isSum :: Bool }
              deriving (Show, Eq)

data ParamSettings = ParamSettings
  { fieldModifier :: (ByteString -> ByteString)
  }

defaultParamSettings :: ParamSettings
defaultParamSettings = ParamSettings {fieldModifier = id}

-- | Serialize a type to the header params
class ToHeader a where
  toHeader :: a -> [Http.Header]

  default toHeader :: (Generic a, GToHeader (Rep a)) => a -> [Http.Header]
  toHeader = gtoHeader "" (ParamAcc 0 False) defaultParamSettings . from

-- | (Try to) Deserialize a type from the header params
class FromHeader a where
  fromHeader :: [Http.Header] -> Validation [ParamErr] a

  default fromHeader :: (Generic a, GFromHeader (Rep a)) => [Http.Header] -> Validation [ParamErr] a
  fromHeader = (fmap to) . gfromHeader "" (ParamAcc 0 False) defaultParamSettings

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

instance (GFromParam f parK, Selector t, f ~ (K1 i c)) => GFromParam (M1 S t f) parK where
  gfromParam pt pfx pa psett kvs = let fldN = (ASCII.pack $ (selName (undefined :: (M1 S t f) a)))
                                   in case fldN of
                                     "" -> M1 <$> gfromParam pt (pfx `nest` numberedFld pa) pa psett (submap pfx kvs)
                                     _  -> M1 <$> gfromParam pt (pfx `nest` fieldModifier psett fldN) pa psett (submap pfx kvs)

instance (FromParam parK c) => GFromParam (K1 i c) parK where
  gfromParam pt pfx _ _ kvs = K1 <$> fromParam pt pfx kvs

instance (FromParam parK Unit) => GFromParam U1 parK where
  gfromParam pt key _ _ kvs = const U1 <$> (fromParam pt key kvs :: Validation [ParamErr] Unit)

class GToParam f (parK :: ParamK) where
  gtoParam :: Proxy (parK :: ParamK) -> ByteString -> ParamAcc -> ParamSettings -> f a -> [SerializedData parK]

instance (GToParam f parK, GToParam g parK) => GToParam (f :*: g) parK where
  gtoParam pt pfx pa psett (x :*: y) = gtoParam pt pfx pa psett x ++ gtoParam pt pfx (pa { index = index pa + 1 }) psett y

instance (GToParam f parK, GToParam g parK) => GToParam (f :+: g) parK where
  gtoParam pt pfx pa psett(L1 x) = gtoParam pt pfx (pa { isSum = True }) psett x
  gtoParam pt pfx pa psett (R1 y) = gtoParam pt pfx (pa { isSum = True }) psett y

instance (ToParam parK c) => GToParam (K1 i c) parK where
  gtoParam pt pfx _ _ (K1 x) = toParam pt pfx x

instance (GToParam f parK, Constructor t) => GToParam (M1 C t f) parK where
  gtoParam pt pfx pa psett con@(M1 x) = case isSum pa of
    True  -> gtoParam pt (pfx `nest` ASCII.pack (conName con)) (pa { index = 0 }) psett x
    False -> gtoParam pt pfx (pa { index = 0 }) psett x

instance (GToParam f parK) => GToParam (M1 D t f) parK where
  gtoParam pt pfx pa psett (M1 x) = gtoParam pt pfx pa psett x

instance (GToParam f parK, Selector t, f ~ (K1 i c)) => GToParam (M1 S t f) parK where
  gtoParam pt pfx pa psett  m@(M1 x) = let fldN = ASCII.pack (selName m)
                                       in case fldN of
                                         "" -> gtoParam pt (pfx `nest` numberedFld pa) pa psett x
                                         _  -> gtoParam pt (pfx `nest` (fieldModifier psett fldN)) pa psett x

instance (ToParam parK Unit) => GToParam U1 parK where
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
