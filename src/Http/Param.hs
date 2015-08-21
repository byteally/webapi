{-# LANGUAGE DeriveGeneric, TypeFamilies, DataKinds, OverloadedStrings, MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, ScopedTypeVariables, UndecidableInstances, DeriveFunctor, DeriveAnyClass #-}
module Http.Param
       ( ParamK (..)
       , JsonOf (..)
       , OptValue (..)
       , Validation (..)
       , HttpParam (..)
       , ToParam (..)
       , FromParam (..)
       , ParamErr (..)
       , toQueryParam
       , toFormParam
       , toFileParam
       , toPathParam
       , toHeader
       , fromQueryParam
       , fromFormParam
       , fromFileParam
       , fromHeader
       , nest
       , lookupParam
       ) where


import           Data.Aeson (FromJSON (..), ToJSON (..))
import qualified Data.Aeson as A (encode, decodeStrict')
import           Data.Binary (encode)
import           Data.ByteString as SB
import           Data.ByteString.Builder (toLazyByteString, byteString, char7)
import           Data.ByteString.Char8 as ASCII (pack, unpack, readInteger, split)
import           Data.ByteString.Lazy (toStrict)
import           Data.ByteString.Lex.Integral
import           Data.CaseInsensitive as CI
import           Data.Foldable as Fold (foldl')
import           Data.Proxy
import           Data.Text as T (Text)
import           Data.Text.Encoding (encodeUtf8, decodeUtf8')
import           Data.Trie as Trie
import           Network.HTTP.Types
import           Network.HTTP.Types as Http (QueryItem, Header)
import           Data.Int
import           Data.Word
import           Data.Time.Calendar (Day)
import           Data.Monoid ((<>))

data ParamK = QueryParam
            | FormParam
            | FileParam
            | PathParam
            | Header
            | Cookie

newtype OptValue a = OptValue { toMaybe :: Maybe a}
                   deriving (Show, Read, Eq, Ord)

newtype JsonOf a = JsonOf {getValue :: a}
                    deriving (Show, Read, Eq, Ord)

instance ToJSON a => ToJSON (JsonOf a) where
  toJSON (JsonOf a) = toJSON a

instance FromJSON a => FromJSON (JsonOf a) where
  parseJSON jval = JsonOf `fmap` parseJSON jval

type family SerializedData (par :: ParamK) where
  SerializedData 'QueryParam = Http.QueryItem
  SerializedData 'FormParam  = (ByteString, ByteString)
  SerializedData 'FileParam  = (ByteString, FilePath)
  SerializedData 'PathParam  = ByteString
  SerializedData 'Header     = Http.Header

type family DeSerializedData (par :: ParamK) where
  DeSerializedData 'QueryParam = Maybe ByteString
  DeSerializedData 'FormParam  = ByteString
  DeSerializedData 'FileParam  = FilePath
  DeSerializedData 'Header     = ByteString
  
newtype Validation e a = Validation { getValidation :: Either e a }
                       deriving Functor

instance Monoid e => Applicative (Validation e) where
  pure = Validation . Right
  Validation a <*> Validation b = Validation $
    case a of
      Right va -> fmap va b
      Left ea -> either (Left . mappend ea) (const $ Left ea) b           

toQueryParam :: (ToParam a 'QueryParam) => a -> Query
toQueryParam = toParam (Proxy :: Proxy 'QueryParam) ""

toFormParam :: (ToParam a 'FormParam) => a -> [(ByteString, ByteString)]
toFormParam = toParam (Proxy :: Proxy 'FormParam) ""

toFileParam :: (ToParam a 'FileParam) => a -> [(ByteString, FilePath)]
toFileParam = toParam (Proxy :: Proxy 'FileParam) ""

toPathParam :: (ToParam a 'PathParam) => a -> [ByteString]
toPathParam = toParam (Proxy :: Proxy 'PathParam) ""

toHeader :: (ToParam a 'Header) => a -> RequestHeaders
toHeader = toParam (Proxy :: Proxy 'Header) ""

fromQueryParam :: (FromParam a 'QueryParam) => Query -> Validation [ParamErr] a
fromQueryParam par = fromParam (Proxy :: Proxy 'QueryParam) "" $ Trie.fromList par

fromFormParam :: (FromParam a 'FormParam) => [(ByteString, ByteString)] -> Validation [ParamErr] a
fromFormParam par = fromParam (Proxy :: Proxy 'FormParam) "" $ Trie.fromList par

fromFileParam :: (FromParam a 'FileParam) => [(ByteString, FilePath)] -> Validation [ParamErr] a
fromFileParam par = fromParam (Proxy :: Proxy 'FileParam) "" $ Trie.fromList par

fromHeader :: (FromParam a 'Header) => [Http.Header] -> Validation [ParamErr] a
fromHeader _par = fromParam (Proxy :: Proxy 'Header) "" $ error "TODO"


class ToParam a (parK :: ParamK) where
  toParam :: Proxy (parK :: ParamK) -> ByteString -> a -> [SerializedData parK]

class FromParam a (parK :: ParamK) where
  fromParam :: Proxy (parK :: ParamK) -> ByteString -> Trie (DeSerializedData parK) -> Validation [ParamErr] a

class HttpParam (t :: *) where
  toHttpParam :: t -> ByteString
  fromHttpParam :: ByteString -> Maybe t

instance HttpParam Int where
  toHttpParam i = toStrict $ encode i
  fromHttpParam str = case readDecimal str of
    Just (v, "") -> Just v
    _            -> Nothing

instance HttpParam Int8 where
  toHttpParam i = toStrict $ encode i
  fromHttpParam str = case readDecimal str of
    Just (v, "") -> Just v
    _            -> Nothing

instance HttpParam Int16 where
  toHttpParam i = toStrict $ encode i
  fromHttpParam str = case readDecimal str of
    Just (v, "") -> Just v
    _            -> Nothing

instance HttpParam Int32 where
  toHttpParam i = toStrict $ encode i
  fromHttpParam str = case readDecimal str of
    Just (v, "") -> Just v
    _            -> Nothing

instance HttpParam Int64 where
  toHttpParam i = toStrict $ encode i
  fromHttpParam str = case readDecimal str of
    Just (v, "") -> Just v
    _            -> Nothing

instance HttpParam Word where
  toHttpParam i = toStrict $ encode i
  fromHttpParam str = case readDecimal str of
    Just (v, "") -> Just v
    _            -> Nothing

instance HttpParam Word8 where
  toHttpParam i = toStrict $ encode i
  fromHttpParam str = case readDecimal str of
    Just (v, "") -> Just v
    _            -> Nothing

instance HttpParam Word16 where
  toHttpParam i = toStrict $ encode i
  fromHttpParam str = case readDecimal str of
    Just (v, "") -> Just v
    _            -> Nothing

instance HttpParam Word32 where
  toHttpParam i = toStrict $ encode i
  fromHttpParam str = case readDecimal str of
    Just (v, "") -> Just v
    _            -> Nothing

instance HttpParam Word64 where
  toHttpParam i = toStrict $ encode i
  fromHttpParam str = case readDecimal str of
    Just (v, "") -> Just v
    _            -> Nothing

instance HttpParam Text where
  toHttpParam = encodeUtf8
  fromHttpParam str = case decodeUtf8' str of
    Right txt -> Just txt
    Left _    -> Nothing

instance HttpParam Day where
  toHttpParam day = ASCII.pack $ show day
  fromHttpParam str = case reads $ ASCII.unpack str of
    [(a,"")] -> Just a
    _        -> Nothing

instance (HttpParam a, HttpParam b) => HttpParam (a,b) where
  toHttpParam (a,b) = toStrict $ toLazyByteString $ byteString (toHttpParam a) 
                                                  <> char7 ',' 
                                                  <> byteString (toHttpParam b)
  fromHttpParam str = case ASCII.split ',' str of
    [str1, str2] -> (,) <$> fromHttpParam str1 <*> fromHttpParam str2
    _            -> Nothing

instance HttpParam Bool where
  toHttpParam i = ASCII.pack $ show i
  fromHttpParam str | str == "True"  = Just True
  fromHttpParam str | str == "False" = Just False
                    | otherwise     = Nothing

instance HttpParam Integer where
  toHttpParam i = toStrict $ encode i
  fromHttpParam str = case ASCII.readInteger str of
    Just (i, "") -> Just i
    _            -> Nothing

instance (ToJSON a, FromJSON a) => HttpParam (JsonOf a) where
  toHttpParam (JsonOf a) = toStrict $ A.encode a
  fromHttpParam str = A.decodeStrict' str

instance ToParam () par where
  toParam _ _ _ = []
  
instance ToParam Int 'QueryParam where
  toParam _ pfx val = [(pfx, Just $ toHttpParam val)]

instance ToParam Int 'FormParam where
  toParam _ pfx val = [(pfx, toHttpParam val)]

instance ToParam Int 'Header where
  toParam _ pfx val = [(mk pfx, toHttpParam val)]

instance (HttpParam a) => ToParam (OptValue a) 'QueryParam where
  toParam _ pfx (OptValue (Just val)) = [(pfx, Just $ toHttpParam val)]
  toParam _ pfx (OptValue Nothing)    = [(pfx, Nothing)]

instance (HttpParam a) => ToParam (OptValue a) 'FormParam where
  toParam _ pfx (OptValue (Just val)) = [(pfx, toHttpParam val)]
  toParam _ _ (OptValue Nothing)     = []

instance (HttpParam a) => ToParam (OptValue a) 'Header where
  toParam _ pfx (OptValue (Just val)) = [(mk pfx, toHttpParam val)]
  toParam _ _ (OptValue Nothing)     = []

instance (ToJSON a, FromJSON a) => ToParam (JsonOf a) 'QueryParam where
  toParam _ pfx val = [(pfx, Just $ toHttpParam val)]

instance (ToJSON a, FromJSON a) => ToParam (JsonOf a) 'FormParam where
  toParam _ pfx val = [(pfx, toHttpParam val)]

instance (ToJSON a, FromJSON a) => ToParam (JsonOf a) 'Header where
  toParam _ pfx val = [(mk pfx, toHttpParam val)]

instance ToParam a par => ToParam (Maybe a) par where
  toParam pt pfx (Just val) = toParam pt pfx val
  toParam _ _ Nothing      = []

instance ToParam a par => ToParam [a] par where
  toParam pt pfx vals = Prelude.concatMap (\(ix, v) -> toParam pt (pfx `nest` (toStrict $ encode ix)) v) $ Prelude.zip [(0 :: Word)..] vals

instance FromParam () par where
  fromParam _ _ _ = pure ()

instance FromParam Int 'QueryParam where
  fromParam pt key kvs = case lookupParam pt key kvs of
   Just (Just par) -> case readDecimal par of
     Just (v, "") -> Validation $ Right v
     _            -> Validation $ Left [ParseErr key "Unable to cast to Int"]
   _ ->  Validation $ Left [NotFound key]

instance FromParam Int 'FormParam where
  fromParam pt key kvs = case lookupParam pt key kvs of
   Just par -> case readDecimal par of
     Just (v, "") -> Validation $ Right v
     _            -> Validation $ Left [ParseErr key "Unable to cast to Int"]
   _ ->  Validation $ Left [NotFound key]

instance FromParam Int 'Header where
  fromParam pt key kvs = case lookupParam pt key kvs of
   Just par -> case readDecimal par of
     Just (v, "") -> Validation $ Right v
     _            -> Validation $ Left [ParseErr key "Unable to cast to Int"]
   _ ->  Validation $ Left [NotFound key]

instance FromParam Word 'QueryParam where
  fromParam pt key kvs = case lookupParam pt key kvs of
   Just (Just par) -> case readDecimal par of
     Just (v, "") -> Validation $ Right v
     _            -> Validation $ Left [ParseErr key "Unable to cast to Word"]
   _ ->  Validation $ Left [NotFound key]

instance FromParam a par => FromParam (Maybe a) par where
  fromParam pt key kvs = case Trie.null kvs' of
    True  ->  Validation $ Right Nothing
    False -> case (fromParam pt key kvs' :: Validation [ParamErr] a) of
      Validation (Right val) -> Validation $ Right $ Just val
      Validation (Left errs) -> Validation $ Left errs
    where kvs' = submap key kvs

instance FromParam a par => FromParam [a] par where
  fromParam pt key kvs = case Trie.null kvs' of
    True  ->  Validation $ Right []
    False -> 
      let pars = Prelude.map (\kv -> fromParam pt key kv :: Validation [ParamErr] a) kvitems
      in Fold.foldl' accRes (Validation $ Right []) pars
    where kvs' = submap key kvs
          kvitems = Prelude.takeWhile (not . Prelude.null)  (Prelude.map (\ix -> submap (key `nest` (toStrict $ encode ix)) kvs') [(0 :: Word) .. 2000])
          accRes acc elemt = case (acc, elemt) of
            (Validation (Right as), Validation (Right e)) -> Validation $ Right (e:as)
            (Validation (Left as), Validation (Right _)) -> Validation $ Left as
            (Validation (Right _), Validation (Left es)) -> Validation $ Left es
            (Validation (Left as), Validation (Left es)) -> Validation $ Left (es ++ as)


instance (HttpParam a) => FromParam (OptValue a) 'QueryParam where
  fromParam pt key kvs = case lookupParam pt key kvs of
   Just (Just par) -> case fromHttpParam par of
     Just v     -> Validation $ Right $ OptValue $ Just v
     _          -> Validation $ Left [ParseErr key "Unable to cast to OptValue"]
   Just Nothing -> Validation $ Right $ OptValue Nothing
   _            -> Validation $ Left [NotFound key]

instance (HttpParam a) => FromParam (OptValue a) 'FormParam where
  fromParam pt key kvs = case lookupParam pt key kvs of
   Just par -> case fromHttpParam par of
     Just v -> Validation $ Right $ OptValue $ Just v
     _      -> Validation $ Left [ParseErr key "Unable to cast to OptValue"]
   _        -> Validation $ Left [NotFound key]

instance (HttpParam a) => FromParam (OptValue a) 'Header where
  fromParam pt key kvs = case lookupParam pt key kvs of
   Just par -> case fromHttpParam par of
     Just v -> Validation $ Right $ OptValue $ Just v
     _      -> Validation $ Left [ParseErr key "Unable to cast to OptValue"]
   _        -> Validation $ Left [NotFound key]


instance ( HttpParam a
         , HttpParam b
         ) => ToParam (a, b) 'PathParam where
  toParam _ _ (a, b) = [toHttpParam a, toHttpParam b]

instance ( HttpParam a
         , HttpParam b
         , HttpParam c
         ) => ToParam (a, b, c) 'PathParam where
  toParam _ _ (a, b, c) = [ toHttpParam a
                          , toHttpParam b
                          , toHttpParam c
                          ]

instance ( HttpParam a
         , HttpParam b
         , HttpParam c
         , HttpParam d
         ) => ToParam (a, b, c, d) 'PathParam where
  toParam _ _ (a, b, c, d) 
    = [ toHttpParam a
      , toHttpParam b
      , toHttpParam c
      , toHttpParam d
      ]

instance ( HttpParam a
         , HttpParam b
         , HttpParam c
         , HttpParam d
         , HttpParam e
         ) => ToParam (a, b, c, d, e) 'PathParam where
  toParam _ _ (a, b, c, d, e) 
    = [ toHttpParam a
      , toHttpParam b
      , toHttpParam c
      , toHttpParam d
      , toHttpParam e
      ]

instance ( HttpParam a
         , HttpParam b
         , HttpParam c
         , HttpParam d
         , HttpParam e
         , HttpParam f
         ) => ToParam (a, b, c, d, e, f) 'PathParam where
  toParam _ _ (a, b, c, d, e, f) 
    = [ toHttpParam a
      , toHttpParam b
      , toHttpParam c
      , toHttpParam d
      , toHttpParam e
      , toHttpParam f
      ]

instance ( HttpParam a
         , HttpParam b
         , HttpParam c
         , HttpParam d
         , HttpParam e
         , HttpParam f
         , HttpParam g
         , HttpParam h
         ) => ToParam (a, b, c, d, e, f, g, h) 'PathParam where
  toParam _ _ (a, b, c, d, e, f, g, h) 
    = [ toHttpParam a
      , toHttpParam b
      , toHttpParam c
      , toHttpParam d
      , toHttpParam e
      , toHttpParam f
      , toHttpParam g
      , toHttpParam h
      ]

instance ( HttpParam a
         , HttpParam b
         , HttpParam c
         , HttpParam d
         , HttpParam e
         , HttpParam f
         , HttpParam g
         , HttpParam h
         , HttpParam i
         ) => ToParam (a, b, c, d, e, f, g, h, i) 'PathParam where
  toParam _ _ (a, b, c, d, e, f, g, h, i) 
    = [ toHttpParam a
      , toHttpParam b
      , toHttpParam c
      , toHttpParam d
      , toHttpParam e
      , toHttpParam f
      , toHttpParam g
      , toHttpParam h
      , toHttpParam i
      ]

instance ( HttpParam a
         , HttpParam b
         , HttpParam c
         , HttpParam d
         , HttpParam e
         , HttpParam f
         , HttpParam g
         , HttpParam h
         , HttpParam i
         , HttpParam j
         ) => ToParam (a, b, c, d, e, f, g, h, i, j) 'PathParam where
  toParam _ _ (a, b, c, d, e, f, g, h, i, j) 
    = [ toHttpParam a
      , toHttpParam b
      , toHttpParam c
      , toHttpParam d
      , toHttpParam e
      , toHttpParam f
      , toHttpParam g
      , toHttpParam h
      , toHttpParam i
      , toHttpParam j
      ]

data ParamErr = NotFound ByteString 
              | ParseErr ByteString ByteString
                deriving (Show)

nest :: ByteString -> ByteString -> ByteString
nest s1 s2 | SB.null s1 = s2
           | otherwise = SB.concat [s1, ".", s2]


lookupParam :: Proxy (parK :: ParamK) -> ByteString -> Trie (DeSerializedData parK) -> Maybe (DeSerializedData parK)
lookupParam _ key kvs = Trie.lookup key kvs
