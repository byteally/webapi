{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE DefaultSignatures     #-}
{-# LANGUAGE TypeOperators         #-}
module Http.Param
       ( ParamK (..)
       , JsonOf (..)
       , OptValue (..)
       , Validation (..)
       , HttpParam (..)
       , ToParam (..)
       , FromParam (..)
       , ParamErr (..)
       , ParamErrToApiErr (..)
       , SerializedData
       , DeSerializedData  
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
       , toNonNestedParam
       , fromNonNestedParam
       ) where


import           Data.Aeson                     (FromJSON (..), ToJSON (..))
import qualified Data.Aeson                     as A
import           Data.ByteString                as SB hiding (index)
import           Data.ByteString.Builder        (byteString, char7,
                                                 toLazyByteString)
import           Data.ByteString.Char8          as ASCII (pack, readInteger,
                                                          split, unpack)
import           Data.ByteString.Lazy           (toStrict)
import qualified Data.ByteString.Lex.Fractional as LexF
import           Data.ByteString.Lex.Integral
import           Data.CaseInsensitive           as CI
import           Data.Foldable                  as Fold (foldl')
import           Data.Int
import           Data.Monoid                    ((<>))
import           Data.Proxy
import           Data.Text                      as T (Text, pack)
import qualified Data.Vector                    as V
import           Data.Vector                    (Vector)
import           Data.Text.Encoding             (decodeUtf8', encodeUtf8)
import           Data.Time.Calendar             (Day)
import           Data.Time.Clock                (UTCTime)
import           Data.Time.Format               (FormatTime, formatTime, parseTimeM, defaultTimeLocale)
import           Data.Trie                      as Trie
import           Data.Word
import           Debug.Trace
import           Network.HTTP.Types
import           Network.HTTP.Types             as Http (Header, QueryItem)
import           GHC.Generics
import           Data.Typeable

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
                       deriving (Functor, Show)

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

  default toParam :: (Generic a, GToParam (Rep a) parK) => Proxy (parK :: ParamK) -> ByteString -> a -> [SerializedData parK]
  toParam pt pfx = gtoParam pt pfx (ParamAcc 0 False) ParamSettings . from

class FromParam a (parK :: ParamK) where
  fromParam :: Proxy (parK :: ParamK) -> ByteString -> Trie (DeSerializedData parK) -> Validation [ParamErr] a

  default fromParam :: (Generic a, GFromParam (Rep a) parK) => Proxy (parK :: ParamK) -> ByteString -> Trie (DeSerializedData parK) -> Validation [ParamErr] a
  fromParam pt pfx = (fmap to) . gfromParam pt pfx (ParamAcc 0 False) ParamSettings

class HttpParam (t :: *) where
  toHttpParam :: t -> ByteString
  fromHttpParam :: ByteString -> Maybe t

  default toHttpParam :: (Generic t, GHttpParam (Rep t)) => t -> ByteString
  toHttpParam = gToHttpParam . from
  default fromHttpParam :: (Generic t, GHttpParam (Rep t)) => ByteString -> Maybe t
  fromHttpParam = (fmap to) . gFromHttpParam

instance HttpParam Int where
  toHttpParam i = ASCII.pack $ show i
  fromHttpParam str = case readDecimal str of
    Just (v, "") -> Just v
    _            -> Nothing

instance HttpParam Int8 where
  toHttpParam i = ASCII.pack $ show i
  fromHttpParam str = case readDecimal str of
    Just (v, "") -> Just v
    _            -> Nothing

instance HttpParam Int16 where
  toHttpParam i = ASCII.pack $ show i
  fromHttpParam str = case readDecimal str of
    Just (v, "") -> Just v
    _            -> Nothing

instance HttpParam Int32 where
  toHttpParam i = ASCII.pack $ show i
  fromHttpParam str = case readDecimal str of
    Just (v, "") -> Just v
    _            -> Nothing

instance HttpParam Int64 where
  toHttpParam i = ASCII.pack $ show i
  fromHttpParam str = case readDecimal str of
    Just (v, "") -> Just v
    _            -> Nothing

instance HttpParam Word where
  toHttpParam i = ASCII.pack $ show i
  fromHttpParam str = case readDecimal str of
    Just (v, "") -> Just v
    _            -> Nothing

instance HttpParam Word8 where
  toHttpParam i = ASCII.pack $ show i
  fromHttpParam str = case readDecimal str of
    Just (v, "") -> Just v
    _            -> Nothing

instance HttpParam Word16 where
  toHttpParam i = ASCII.pack $ show i
  fromHttpParam str = case readDecimal str of
    Just (v, "") -> Just v
    _            -> Nothing

instance HttpParam Word32 where
  toHttpParam i = ASCII.pack $ show i
  fromHttpParam str = case readDecimal str of
    Just (v, "") -> Just v
    _            -> Nothing

instance HttpParam Word64 where
  toHttpParam i = ASCII.pack $ show i
  fromHttpParam str = case readDecimal str of
    Just (v, "") -> Just v
    _            -> Nothing

instance HttpParam Float where
  toHttpParam d = ASCII.pack $ show d
  fromHttpParam str = case LexF.readExponential str of
    Just (v, "") -> Just v
    _            -> Nothing

instance HttpParam Double where
  toHttpParam d = ASCII.pack $ show d
  fromHttpParam str = case LexF.readExponential str of
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

instance HttpParam UTCTime where
  toHttpParam t = ASCII.pack $ formatTime defaultTimeLocale format t
    where
      format = "%FT%T." ++ formatSubseconds t ++ "Z"
  fromHttpParam str = case parseTimeM True defaultTimeLocale "%FT%T%QZ" (ASCII.unpack str) of
    Just d -> Just d
    _      -> Nothing
    
formatSubseconds :: (FormatTime t) => t -> String
formatSubseconds = formatTime defaultTimeLocale "%q"

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
  toHttpParam i = ASCII.pack $ show i
  fromHttpParam str = case ASCII.readInteger str of
    Just (i, "") -> Just i
    _            -> Nothing

instance (ToJSON a, FromJSON a) => HttpParam (JsonOf a) where
  toHttpParam (JsonOf a) = toStrict $ A.encode a
  fromHttpParam str = A.decodeStrict' str

class GHttpParam f where
  gToHttpParam   :: f a -> ByteString
  gFromHttpParam :: ByteString -> Maybe (f a)

instance (GHttpParam f) => GHttpParam (D1 c f) where
  gToHttpParam (M1 c) = gToHttpParam c
  gFromHttpParam str  = M1 <$> (gFromHttpParam str)
  
instance (GHttpParam f, GHttpParam g) => GHttpParam (f :+: g) where
  gToHttpParam (L1 l) = gToHttpParam l
  gToHttpParam (R1 r) = gToHttpParam r
  gFromHttpParam str  = case L1 <$> gFromHttpParam str of
    l1@(Just _) -> l1
    _           -> R1 <$> gFromHttpParam str
  
instance (GHttpParam f, Constructor c) => GHttpParam (C1 c f) where
  gToHttpParam con@(M1 c) = const (ASCII.pack $ conName con) $ gToHttpParam c
  gFromHttpParam str = if (ASCII.pack $ conName (undefined :: (C1 c f) a)) == str
                       then M1 <$> gFromHttpParam str
                       else Nothing
  
instance GHttpParam U1 where
  gToHttpParam U1    = error "Panic! Unreacheable code @ GHttpParam U1"
  gFromHttpParam _   = Just U1 


newtype NonNested a = NonNested { getNonNestedParam :: a }
                    deriving (Show, Eq, Read)

toNonNestedParam :: (ToParam (NonNested a) parK, HttpParam a) => Proxy (parK :: ParamK) -> ByteString -> a -> [SerializedData parK]
toNonNestedParam par pfx a = toParam par pfx (NonNested a)

fromNonNestedParam :: (FromParam (NonNested a) parK, HttpParam a) => Proxy (parK :: ParamK) -> ByteString -> Trie (DeSerializedData parK) -> Validation [ParamErr] a
fromNonNestedParam par pfx kvs = getNonNestedParam <$> fromParam par pfx kvs

instance (HttpParam a) => ToParam (NonNested a) 'QueryParam where
  toParam _ pfx (NonNested val) = [(pfx, Just $ toHttpParam val)]

instance (HttpParam a) => ToParam (NonNested a) 'FormParam where
  toParam _ pfx (NonNested val) = [(pfx, toHttpParam val)]

instance (HttpParam a) => ToParam (NonNested a) 'Header where
  toParam _ pfx (NonNested val) = [(mk pfx, toHttpParam val)]

instance (HttpParam a, Typeable a) => FromParam (NonNested a) 'QueryParam where
  fromParam pt key kvs = case lookupParam pt key kvs of
   Just (Just par) -> case fromHttpParam par of
         Just v -> Validation $ Right $ NonNested v
         _      -> Validation $ Left [ParseErr key $ T.pack $ "Unable to cast to " ++ (show $ typeOf (Proxy :: Proxy a))]
   _ ->  Validation $ Left [NotFound key]

instance (HttpParam a, Typeable a) => FromParam (NonNested a) 'FormParam where  
  fromParam pt key kvs = case lookupParam pt key kvs of
   Just par -> case fromHttpParam par of
         Just v -> Validation $ Right $ NonNested v
         _      -> Validation $ Left [ParseErr key $ T.pack $ "Unable to cast to " ++ (show $ typeOf (Proxy :: Proxy a))]
   _ ->  Validation $ Left [NotFound key]

instance (HttpParam a, Typeable a) => FromParam (NonNested a) 'Header where
  fromParam pt key kvs = case lookupParam pt key kvs of
   Just par -> case fromHttpParam par of
         Just v -> Validation $ Right $ NonNested v
         _      -> Validation $ Left [ParseErr key $ T.pack $ "Unable to cast to " ++ (show $ typeOf (Proxy :: Proxy a))]
   _ ->  Validation $ Left [NotFound key]
  
instance ToParam () par where
  toParam _ _ _ = []

instance ToParam Int 'QueryParam where
  toParam _ pfx val = [(pfx, Just $ toHttpParam val)]

instance ToParam Int 'FormParam where
  toParam _ pfx val = [(pfx, toHttpParam val)]

instance ToParam Int 'Header where
  toParam _ pfx val = [(mk pfx, toHttpParam val)]

instance ToParam Text 'QueryParam where
  toParam _ pfx val = [(pfx, Just $ toHttpParam val)]

instance ToParam Text 'FormParam where
  toParam _ pfx val = [(pfx, toHttpParam val)]

instance ToParam Text 'Header where
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
  toParam pt pfx vals = Prelude.concatMap (\(ix, v) -> toParam pt (pfx `nest` (ASCII.pack $ show ix)) v) $ Prelude.zip [(0 :: Word)..] vals

instance ToParam a par => ToParam (Vector a) par where
  toParam pt pfx vals = toParam pt pfx (V.toList vals)
  
instance FromParam () par where
  fromParam _ _ _ = pure ()

instance FromParam Bool 'QueryParam where
  fromParam pt key kvs = case lookupParam pt key kvs of
   Just (Just par) -> case fromHttpParam par of
         Just v -> Validation $ Right v
         _      -> Validation $ Left [ParseErr key "Unable to cast to Bool"]
   _ ->  Validation $ Left [NotFound key]

instance FromParam Bool 'FormParam where
  fromParam pt key kvs = case lookupParam pt key kvs of
   Just par -> case fromHttpParam par of
         Just v -> Validation $ Right v
         _      -> Validation $ Left [ParseErr key "Unable to cast to Bool"]
   _ ->  Validation $ Left [NotFound key]

instance FromParam Bool 'Header where
  fromParam pt key kvs = case lookupParam pt key kvs of
   Just par -> case fromHttpParam par of
         Just v -> Validation $ Right v
         _      -> Validation $ Left [ParseErr key "Unable to cast to Bool"]
   _ ->  Validation $ Left [NotFound key]

instance FromParam Word 'QueryParam where
  fromParam pt key kvs = case lookupParam pt key kvs of
   Just (Just par) -> case fromHttpParam par of
         Just v -> Validation $ Right v
         _      -> Validation $ Left [ParseErr key "Unable to cast to Word"]
   _ ->  Validation $ Left [NotFound key]

instance FromParam Word 'FormParam where
  fromParam pt key kvs = case lookupParam pt key kvs of
   Just par -> case fromHttpParam par of
         Just v -> Validation $ Right v
         _      -> Validation $ Left [ParseErr key "Unable to cast to Word"]
   _ ->  Validation $ Left [NotFound key]

instance FromParam Word 'Header where
  fromParam pt key kvs = case lookupParam pt key kvs of
   Just par -> case fromHttpParam par of
         Just v -> Validation $ Right v
         _      -> Validation $ Left [ParseErr key "Unable to cast to Word"]
   _ ->  Validation $ Left [NotFound key]

instance FromParam UTCTime 'QueryParam where
  fromParam pt key kvs = case lookupParam pt key kvs of
   Just (Just par) -> case fromHttpParam par of
         Just v -> Validation $ Right v
         _      -> Validation $ Left [ParseErr key "Unable to cast to UTCTime (ISO-8601)"]
   _ ->  Validation $ Left [NotFound key]

instance FromParam UTCTime 'FormParam where
  fromParam pt key kvs = case lookupParam pt key kvs of
   Just par -> case fromHttpParam par of
         Just v -> Validation $ Right v
         _      -> Validation $ Left [ParseErr key "Unable to cast to UTCTime (ISO-8601)"]
   _ ->  Validation $ Left [NotFound key]

instance FromParam UTCTime 'Header where
  fromParam pt key kvs = case lookupParam pt key kvs of
   Just par -> case fromHttpParam par of
         Just v -> Validation $ Right v
         _      -> Validation $ Left [ParseErr key "Unable to cast to UTCTime (ISO-8601)"]
   _ ->  Validation $ Left [NotFound key]

instance FromParam Int 'QueryParam where
  fromParam pt key kvs = case lookupParam pt key kvs of
   Just (Just par) -> case readDecimal par of
     Just (v, "") -> Validation $ Right v
     _            -> Validation $ Left [ParseErr key "Unable to cast to Int"]
   _ ->  Validation $ Left [NotFound key]

instance FromParam Int 'FormParam where
  fromParam pt key kvs | traceShow (key, kvs, lookupParam pt key kvs) False = error "Unable trace code"
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

instance FromParam Double 'QueryParam where
  fromParam pt key kvs = case lookupParam pt key kvs of
   Just (Just par) -> case fromHttpParam par of
         Just v -> Validation $ Right v
         _      -> Validation $ Left [ParseErr key "Unable to cast to Double"]
   _ ->  Validation $ Left [NotFound key]

instance FromParam Double 'FormParam where
  fromParam pt key kvs = case lookupParam pt key kvs of
   Just par -> case fromHttpParam par of
         Just v -> Validation $ Right v
         _      -> Validation $ Left [ParseErr key "Unable to cast to Double"]
   _ ->  Validation $ Left [NotFound key]

instance FromParam Double 'Header where
  fromParam pt key kvs = case lookupParam pt key kvs of
   Just par -> case fromHttpParam par of
         Just v -> Validation $ Right v
         _      -> Validation $ Left [ParseErr key "Unable to cast to Double"]
   _ ->  Validation $ Left [NotFound key]

instance FromParam Float 'QueryParam where
  fromParam pt key kvs = case lookupParam pt key kvs of
   Just (Just par) -> case fromHttpParam par of
         Just v -> Validation $ Right v
         _      -> Validation $ Left [ParseErr key "Unable to cast to Float"]
   _ ->  Validation $ Left [NotFound key]

instance FromParam Float 'FormParam where
  fromParam pt key kvs = case lookupParam pt key kvs of
   Just par -> case fromHttpParam par of
         Just v -> Validation $ Right v
         _      -> Validation $ Left [ParseErr key "Unable to cast to Float"]
   _ ->  Validation $ Left [NotFound key]

instance FromParam Float 'Header where
  fromParam pt key kvs = case lookupParam pt key kvs of
   Just par -> case fromHttpParam par of
         Just v -> Validation $ Right v
         _      -> Validation $ Left [ParseErr key "Unable to cast to Float"]
   _ ->  Validation $ Left [NotFound key]

instance FromParam SB.ByteString 'QueryParam where
  fromParam pt key kvs = case lookupParam pt key kvs of
   Just (Just par) -> Validation $ Right par
   _ ->  Validation $ Left [NotFound key]

instance FromParam SB.ByteString 'FormParam where
  fromParam pt key kvs = case lookupParam pt key kvs of
   Just par -> Validation $ Right par
   _ ->  Validation $ Left [NotFound key]

instance FromParam SB.ByteString 'Header where
  fromParam pt key kvs = case lookupParam pt key kvs of
   Just par -> Validation $ Right par
   _ ->  Validation $ Left [NotFound key]

instance FromParam a par => FromParam (Maybe a) par where
  fromParam pt key kvs = case Trie.null kvs' of
    True  ->  Validation $ Right Nothing
    False -> case (fromParam pt key kvs' :: Validation [ParamErr] a) of
      Validation (Right val) -> Validation $ Right $ Just val
      Validation (Left errs) -> Validation $ Left errs
    where kvs' = submap key kvs

instance FromParam Text 'QueryParam where
  fromParam pt key kvs = case lookupParam pt key kvs of
   Just (Just par) -> case fromHttpParam par of
     Just v -> Validation $ Right v
     _      -> Validation $ Left [ParseErr key "Unable to cast to Text"]
   _ ->  Validation $ Left [NotFound key]

instance FromParam Text 'FormParam where
  fromParam pt key kvs = case lookupParam pt key kvs of
   Just par -> case fromHttpParam par of
     Just v -> Validation $ Right v
     _      -> Validation $ Left [ParseErr key "Unable to cast to Text"]
   _ ->  Validation $ Left [NotFound key]

instance FromParam Text 'Header where
  fromParam pt key kvs = case lookupParam pt key kvs of
   Just par -> case fromHttpParam par of
     Just v -> Validation $ Right v
     _      -> Validation $ Left [ParseErr key "Unable to cast to Text"]
   _ ->  Validation $ Left [NotFound key]

instance (Show (DeSerializedData par), FromParam a par) => FromParam [a] par where
  fromParam pt key kvs = case Trie.null kvs' of
    True  ->  Validation $ Right []
    False ->
      let pars = Prelude.map (\(nkey, kv) -> fromParam pt nkey kv :: Validation [ParamErr] a) kvitems
      in Fold.foldl' accRes (Validation $ Right []) pars
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
              | ParseErr ByteString Text
                deriving (Show)

utf8DecodeError :: String -> String -> a
utf8DecodeError src msg = error $ "Error decoding Bytes into UTF8 string at: " ++ src ++ " Message: " ++ msg

instance ToJSON ParamErr where
  toJSON (NotFound bs) = case decodeUtf8' bs of
    Left ex   -> utf8DecodeError "ToJSON ParamErr" (show ex)
    Right bs' -> A.object ["NotFound" A..= bs']
  toJSON (ParseErr bs msg) = case decodeUtf8' bs of
    Left ex -> utf8DecodeError "ToJSON ParamErr" (show ex)
    Right bs' -> A.object ["ParseErr" A..= [bs', msg]]
    
class ParamErrToApiErr apiErr where
  toApiErr :: [ParamErr] -> apiErr

instance ParamErrToApiErr () where
  toApiErr = const ()

instance ParamErrToApiErr A.Value where
  toApiErr errs = toJSON errs
  
nest :: ByteString -> ByteString -> ByteString
nest s1 s2 | SB.null s1 = s2
           | otherwise = SB.concat [s1, ".", s2]

lookupParam :: Proxy (parK :: ParamK) -> ByteString -> Trie (DeSerializedData parK) -> Maybe (DeSerializedData parK)
lookupParam _ key kvs = Trie.lookup key kvs

data ParamAcc = ParamAcc { index :: Int, isSum :: Bool }
              deriving (Show, Eq)

data ParamSettings = ParamSettings
                   deriving (Show, Eq)


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

numberedFld :: ParamAcc -> ByteString
numberedFld pa = ASCII.pack $ show (index pa)
