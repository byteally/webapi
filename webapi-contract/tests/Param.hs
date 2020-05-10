{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Param where

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.Hedgehog
import WebApi.Param

test_ParamRoundTrip :: TestTree
test_ParamRoundTrip =
  testGroup
    "http params round trip"
    [ testGroup
        "encode/decode param rountrip"
        [ testProperty "minBound @Int" $ withTests 1 $ tripper tripParam $ minBoundOf Gen.int,
          testProperty "maxBound @Int" $ withTests 1 $ tripper tripParam $ maxBoundOf Gen.int
        ],
      testGroup
        "query param rountrip"
        [ testProperty "minBound @Int" $ withTests 1 $ tripper tripQuery $ minBoundOf Gen.int,
          testProperty "maxBound @Int" $ withTests 1 $ tripper tripQuery $ maxBoundOf Gen.int
        ]
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
