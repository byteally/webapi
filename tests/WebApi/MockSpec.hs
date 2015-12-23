{-# LANGUAGE MultiParamTypeClasses, TypeFamilies, OverloadedStrings, DataKinds, TypeOperators, TypeSynonymInstances, FlexibleInstances, DeriveGeneric #-}
module WebApi.MockSpec (spec) where

import Data.Aeson
import GHC.Generics
import WebApi
import Test.Hspec
import Test.Hspec.Wai
import Test.QuickCheck
import qualified Network.Wai as Wai

withApp :: SpecWith Wai.Application -> Spec
withApp = with (return mockApp)

mockApp :: Wai.Application
mockApp = mockServer serverSettings (MockServer mockServerSettings :: MockServer MockSpec)

data MockSpec = MockSpec

type MockApi = Static "mock"

data QP = QP { qp1 :: Int, qp2 :: Bool }
        deriving (Show, Eq, Generic)

data MockOut = MockOut { out1 :: Int
                       , out2 :: Bool
                       , out3 :: Char
                       } deriving (Show, Eq, Generic)

instance ToJSON MockOut where
instance FromParam QP 'QueryParam where
instance Arbitrary MockOut where
  arbitrary = MockOut <$> arbitrary
                      <*> arbitrary
                      <*> arbitrary

instance WebApi MockSpec where
  type Apis MockSpec = '[ Route GET MockApi ]

instance ApiContract MockSpec GET MockApi where
  type QueryParam GET MockApi = QP
  type ApiOut     GET MockApi = MockOut


spec :: Spec
spec = withApp $ describe "WebApi mockserver" $ do
    it "should be 200 ok" $ do
      get "mock?qp1=5&qp2=True" `shouldRespondWith` 200
