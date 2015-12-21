{-# LANGUAGE TypeFamilies, KindSignatures, MultiParamTypeClasses, DataKinds, FlexibleContexts, GADTs, TypeOperators, PolyKinds, UndecidableInstances, FlexibleInstances, DefaultSignatures, ScopedTypeVariables, ConstraintKinds, TemplateHaskell, OverloadedStrings, DeriveGeneric #-}
module Main where

import qualified Network.Wai as Wai

import WebApi
import WebApi.Internal
import GHC.Generics
import Data.Int
import Data.Word
import Data.Text (Text)
import Data.ByteString (ByteString)
import Data.Time.Calendar (Day)
import Data.Vector (Vector)
import Data.Time.Clock (UTCTime)
import Network.Wai.Handler.Warp (run)
import Network.HTTP.Types
import Network.Wai.Test (defaultRequest, request,
                         runSession, simpleBody)
import Test.Hspec
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.Hspec.Wai ( get, liftIO, matchHeaders,
                        matchStatus, post, request,
                        shouldRespondWith, with, (<:>))
import Test.QuickCheck
import WebApi.RouteSpec
import WebApi.RequestSpec

data FourSquare = FourSquare
data FourSquareImpl = FourSquareImpl
type UserPhotos   = "users":/Int:/"photos":/Int
type UserCheckins = "users":/Int:/"checkins"
type StaticT = Static ""

instance WebApiImplementation FourSquareImpl where
  type HandlerM FourSquareImpl = IO
  type ApiInterface FourSquareImpl = FourSquare
    
instance ApiContract FourSquare GET UserPhotos where
  type ApiOut GET UserPhotos = ()

instance ApiContract FourSquare POST UserPhotos where
  type ApiOut POST UserPhotos = ()

instance ApiContract FourSquare GET UserCheckins where
  type ApiOut GET UserCheckins = ()

data QP1 = QP1
  { query :: Int
  , key   :: Int
  } deriving (Show)

{-
instance ToParam Int par => ToParam QP1 par where
  toParam pt pfx (QP1 q k) = concat [ toParam pt (pfx `nest` "query") q
                                    , toParam pt (pfx `nest` "key") k
                                    ]
-}
instance (FromParam Int par) => FromParam QP1 par where
  fromParam pt pfx kvs = QP1 <$> (fromParam  pt (pfx `nest` "query") kvs)
                             <*> (fromParam  pt (pfx `nest` "key") kvs)
                         
instance ApiContract FourSquare GET StaticT where
  type QueryParam GET StaticT = QP1
  type ApiOut GET StaticT = ()

instance WebApi FourSquare where
  type Version FourSquare = MajorMinor '(0, 1)
  type Apis FourSquare    = '[ Route GET UserPhotos
                             , Route POST UserPhotos
                             , Route GET UserCheckins
                             , Route GET StaticT
                             ]

instance ApiHandler FourSquareImpl GET UserPhotos where
  handler _ _ req = print (pathParam req) >> respond ()

instance ApiHandler FourSquareImpl POST UserPhotos where
  handler _ _ = error "@ POST UserPhotos"

instance ApiHandler FourSquareImpl GET UserCheckins where
  handler _ _ _req = putStrLn "@UserCheckings" >> respond ()

instance ApiHandler FourSquareImpl GET StaticT where
  handler _ _ req = print (queryParam req) >> respond ()

test :: Wai.Application  
test = serverApp serverSettings FourSquareImpl

spec1 :: Spec
spec1 = do
  with (return test) $ do
    it "should be 200 ok" $ do
      get "?key=1&query=2" `shouldRespondWith` 200
    it "should be 200 ok" $ do
      get "users/10/checkins" `shouldRespondWith` 200
    it "should be 200 ok" $ do
      get "users/10/photos/1" `shouldRespondWith` 200

data U   = U | V
         deriving (Show, Eq, Generic)

data Foo = Foo { bar :: Bool, baz :: Char }
         deriving (Show, Generic, Eq)

data Bar = Bar1 { barr :: Foo, other :: Either Int Bool }
         | Bar2 { barr :: Foo, otherOne :: Int }
         deriving (Show, Generic, Eq)

data Zap = Zap { zap :: Foo, paz :: Bar , nullary :: U}
         deriving (Show, Generic, Eq)

data PrimTys = PrimTys { pInt        :: Int
                       , pInt8       :: Int8  
                       , pInt16      :: Int16
                       , pInt32      :: Int32
                       , pInt64      :: Int64
                       , pInteger    :: Integer  
                       , pWord       :: Word  
                       , pWord8      :: Word8
                       , pWord16     :: Word16
                       , pWord32     :: Word32
                       , pWord64     :: Word64
                       --, pFloat      :: Float
                       --, pDouble     :: Double
                       , pChar       :: Char
                       , pText       :: Text
                       --, pDay        :: Day
                       --, pUTCTime    :: UTCTime
                       , pBool       :: Bool
                       , pEither     :: Either Int Bool
                       , pMaybe      :: Maybe Bool
                       , pList       :: [Char]
                       --, pVector     :: Vector Text
                       --, pByteString :: ByteString
                       } deriving (Generic, Show, Eq)

instance Arbitrary PrimTys where
  arbitrary = PrimTys <$> arbitrary
                      <*> arbitrary
                      <*> arbitrary
                      <*> arbitrary
                      <*> arbitrary
                      <*> arbitrary
                      <*> arbitrary
                      <*> arbitrary
                      <*> arbitrary
                      <*> arbitrary
                      <*> arbitrary
                      -- <*> arbitrary
                      -- <*> arbitrary
                      <*> arbitrary
                      <*> elements ["foo", "Ελλάδα", "français"]
                      -- <*> elements [
                      -- <*> arbitrary
                      <*> arbitrary
                      <*> arbitrary
                      <*> arbitrary
                      <*> arbitrary
                      -- <*> arbitrary
                      -- <*> arbitrary

instance ToParam PrimTys 'QueryParam where
instance ToParam PrimTys 'FormParam where  
instance FromParam PrimTys 'QueryParam where
instance FromParam PrimTys 'FormParam where


instance ToParam U 'QueryParam where
instance ToParam U 'FormParam where  
instance FromParam U 'QueryParam where
instance FromParam U 'FormParam where

instance ToParam Foo 'QueryParam where
instance ToParam Foo 'FormParam where
instance FromParam Foo 'QueryParam where  
instance FromParam Foo 'FormParam where  

instance ToParam Bar 'QueryParam where
instance ToParam Bar 'FormParam where  
instance FromParam Bar 'QueryParam where
instance FromParam Bar 'FormParam where

instance ToParam Zap 'QueryParam where
instance ToParam Zap 'FormParam where  
instance FromParam Zap 'QueryParam where
instance FromParam Zap 'FormParam where

propParamsPrimSpec :: Spec
propParamsPrimSpec = do
  describe "Params for prims : QueryParams" $ do
    it "works for all prims" $ do
      property $ \v -> fromQueryParam (toQueryParam v) == Validation (Right (v :: PrimTys))

paramsGenericSpec :: Spec
paramsGenericSpec = do
  describe "Generic derivation for params QueryParams" $ do
    it "works for unit type" $ do
      let v = U
      fromQueryParam (toQueryParam v) `shouldBe` (Validation (Right v))
    it "works for product types" $ do
      let v = Foo True 'c'
      fromQueryParam (toQueryParam v) `shouldBe` (Validation (Right v))
    it "works for sum types 1" $ do
      let v = Bar1 (Foo False 'd') (Left 8)
      fromQueryParam (toQueryParam v) `shouldBe` (Validation (Right v))
    it "works for sum types 2" $ do
      let v = Bar2 (Foo False 'd') 7
      fromQueryParam (toQueryParam v) `shouldBe` (Validation (Right v))
    it "works for a complex type" $ do
      let v = Zap (Foo False 'f') (Bar2 (Foo False 'f') 7) U
      fromQueryParam (toQueryParam v) `shouldBe` (Validation (Right v))
  describe "Generic derivation for params FormParams" $ do
    it "works for unit type" $ do
      let v = U
      fromFormParam (toFormParam v) `shouldBe` (Validation (Right v))
    it "works for product types" $ do
      let v = Foo False 'f'
      fromFormParam (toFormParam v) `shouldBe` (Validation (Right v))
    it "works for sum types 1" $ do
      let v = Bar1 (Foo False 'f') (Right True)
      fromFormParam (toFormParam v) `shouldBe` (Validation (Right v))
    it "works for sum types 2" $ do
      let v = Bar2 (Foo False 'f') 7
      fromFormParam (toFormParam v) `shouldBe` (Validation (Right v))
    it "works for a complex type" $ do
      let v = Zap (Foo False 'f') (Bar2 (Foo False 'f') 7) U
      fromFormParam (toFormParam v) `shouldBe` (Validation (Right v))


main :: IO ()
main = do
--  run 8000 app
  hspec spec1
--  hspec paramsGenericSpec
--  hspec propParamsPrimSpec
  -- hspec routeSpec
  hspec reqSpec
  -- return ()
