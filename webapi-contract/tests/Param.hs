{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module Param where

import GHC.Generics (Generic)
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.Hedgehog
import Test.Tasty.HUnit
import WebApi.Param

test_ParamRoundTrip :: TestTree
test_ParamRoundTrip =
  testGroup
    "http params round trip"
    [ testGroup
        "encode/decode param rountrip"
        [ testProperty "minBound @Int" $ withTests 1 $ tripper tripParam $ minBoundOf Gen.int
        , testProperty "maxBound @Int" $ withTests 1 $ tripper tripParam $ maxBoundOf Gen.int
        , testProperty "minBound @Int8" $ withTests 1 $ tripper tripParam $ minBoundOf Gen.int8
        , testProperty "maxBound @Int8" $ withTests 1 $ tripper tripParam $ maxBoundOf Gen.int8
        , testProperty "minBound @Int16" $ withTests 1 $ tripper tripParam $ minBoundOf Gen.int16
        , testProperty "maxBound @Int16" $ withTests 1 $ tripper tripParam $ maxBoundOf Gen.int16
        , testProperty "minBound @Int32" $ withTests 1 $ tripper tripParam $ minBoundOf Gen.int32
        , testProperty "maxBound @Int32" $ withTests 1 $ tripper tripParam $ maxBoundOf Gen.int32
        , testProperty "minBound @Int64" $ withTests 1 $ tripper tripParam $ minBoundOf Gen.int64
        , testProperty "maxBound @Int64" $ withTests 1 $ tripper tripParam $ maxBoundOf Gen.int64
        , testProperty "minBound @Word" $ withTests 1 $ tripper tripParam $ minBoundOf Gen.word
        , testProperty "maxBound @Word" $ withTests 1 $ tripper tripParam $ maxBoundOf Gen.word
        , testProperty "minBound @Word8" $ withTests 1 $ tripper tripParam $ minBoundOf Gen.word8
        , testProperty "maxBound @Word8" $ withTests 1 $ tripper tripParam $ maxBoundOf Gen.word8
        , testProperty "minBound @Word16" $ withTests 1 $ tripper tripParam $ minBoundOf Gen.word16
        , testProperty "maxBound @Word16" $ withTests 1 $ tripper tripParam $ maxBoundOf Gen.word16
        , testProperty "minBound @Word32" $ withTests 1 $ tripper tripParam $ minBoundOf Gen.word32
        , testProperty "maxBound @Word32" $ withTests 1 $ tripper tripParam $ maxBoundOf Gen.word32
        , testProperty "minBound @Word64" $ withTests 1 $ tripper tripParam $ minBoundOf Gen.word64
        , testProperty "maxBound @Word64" $ withTests 1 $ tripper tripParam $ maxBoundOf Gen.word64
        ],
      testGroup
        "query param rountrip"
        [ testProperty "minBound @Int" $ withTests 1 $ tripper tripQuery $ minBoundOf Gen.int,
          testProperty "maxBound @Int" $ withTests 1 $ tripper tripQuery $ maxBoundOf Gen.int
        ]
    ]

-- | Two optional fields where one name is a byte-prefix of the other: the
-- presence test of @page@ must not see @pageSize@.
data Paging = Paging
  { page :: Maybe Int
  , pageSize :: Maybe Int
  , ids :: Maybe [Int]
  , idsFilter :: Maybe Int
  , inner :: Maybe Inner
  , innerMost :: Maybe Int
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromParam 'QueryParam, ToParam 'QueryParam)

newtype Inner = Inner { x :: Int }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromParam 'QueryParam, ToParam 'QueryParam)

noPaging :: Paging
noPaging = Paging Nothing Nothing Nothing Nothing Nothing Nothing

test_ParamPresence :: TestTree
test_ParamPresence =
  testGroup
    "an optional param is present by its own key, not by a sibling it prefixes"
    [ testCase "pageSize alone leaves page absent" $
        getValidation (fromQueryParam [("pageSize", Just "20")]) @?= Right noPaging {pageSize = Just 20}
    , testCase "page alone" $
        getValidation (fromQueryParam [("page", Just "2")]) @?= Right noPaging {page = Just 2}
    , testCase "page and pageSize" $
        getValidation (fromQueryParam [("page", Just "2"), ("pageSize", Just "20")]) @?= Right noPaging {page = Just 2, pageSize = Just 20}
    , testCase "a sibling does not make a list present" $
        getValidation (fromQueryParam [("idsFilter", Just "5")]) @?= Right noPaging {idsFilter = Just 5}
    , testCase "a list by its nested keys" $
        getValidation (fromQueryParam [("ids.0", Just "1"), ("ids.1", Just "2")]) @?= Right noPaging {ids = Just [1, 2]}
    , testCase "a nested record by its nested keys, beside a sibling it prefixes" $
        getValidation (fromQueryParam [("inner.x", Just "7"), ("innerMost", Just "8")]) @?= Right noPaging {inner = Just (Inner 7), innerMost = Just 8}
    , testCase "a sibling does not make a nested record present" $
        getValidation (fromQueryParam [("innerMost", Just "8")]) @?= Right noPaging {innerMost = Just 8}
    , testCase "round trip" $
        let v = Paging (Just 1) (Just 2) (Just [3, 4]) (Just 5) (Just (Inner 6)) (Just 7)
        in getValidation (fromQueryParam (toQueryParam v)) @?= Right v
    ]

maxBoundOf :: forall a gen. (Bounded a, MonadGen gen) => (Range a -> gen a) -> gen a
maxBoundOf gen = gen (Range.singleton $ maxBound @a)

minBoundOf :: forall a gen. (Bounded a, MonadGen gen) => (Range a -> gen a) -> gen a
minBoundOf gen = gen (Range.singleton $ minBound @a)

tripParam ::
  ( MonadTest m,
    Show a,
    Eq a,
    EncodeParam a,
    DecodeParam a
  ) =>
  a ->
  m ()
tripParam a = tripping a encodeParam decodeParam

tripQuery ::
  ( MonadTest m,
    Show a,
    Eq a,
    ToParam 'QueryParam a,
    FromParam 'QueryParam a
  ) =>
  a ->
  m ()
tripQuery a = tripping a toQueryParam fromQueryParam

tripForm ::
  ( MonadTest m,
    Show a,
    Eq a,
    ToParam 'FormParam a,
    FromParam 'FormParam a
  ) =>
  a ->
  m ()
tripForm a = tripping a toFormParam fromFormParam

tripFile ::
  ( MonadTest m,
    Show a,
    Eq a,
    ToParam 'FileParam a,
    FromParam 'FileParam a
  ) =>
  a ->
  m ()
tripFile a = tripping a toFileParam fromFileParam

{-
tripPath ::
  ( MonadTest m,
    Show a,
    Eq a,
    ToParam 'PathParam a,
    FromParam 'PathParam a
  ) =>
  a ->
  m ()
tripPath a = tripping a toPathParam fromPathParam


tripCookie ::
  ( MonadTest m,
    Show a,
    Eq a,
    ToParam 'Cookie a,
    FromParam 'Cookie a
  ) =>
  a ->
  m ()
tripCookie a = tripping a toCookie fromCookie
-}

tripper :: (Show a) => (forall m. MonadTest m => a -> m ()) -> Gen a -> Property
tripper trip gen = property $ do
  a <- forAll gen
  trip a
