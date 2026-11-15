{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
-- | A regression case: a narrowed failing case of a property, written out
-- so a later run can replay it against the program as it is then
-- (zenbridge #2713).
--
-- A case is quickcheck-dynamic's own test case, the way the DL program
-- drew it: its quantifier picks and its steps, in order. A step the
-- program writes itself (a rule of the script, a fresh value, a foreign
-- transform) is a function of the program, its picks and the earlier
-- steps, so the case keeps only what tells it apart (its kind, where it
-- was written, its operation) and the replay rebuilds it from the program.
-- A step drawn by @anyActions@ is not in the program: the case keeps the
-- operation, the positions its generation drew, and the earlier steps its
-- request refers to.
--
-- Fresh values are never written: a fresh step runs again on replay and
-- makes a new token, and every step that shared it shares the new one.
module Test.WebApi.Case
  ( CaseRecord (..)
  , caseFormat
  , CaseItem (..)
  , PickRecord (..)
  , MoveRecord (..)
  , GeneratedRecord (..)
  , LinkRecord (..)
    -- * Picks
  , encodePick
  , decodePick
    -- * Variables
  , varIndex
  ) where

import           Control.Applicative ((<|>))
import           Data.Aeson (FromJSON (..), ToJSON (..), Value (..), object, withObject, (.:), (.:?), (.!=), (.=))
import qualified Data.Aeson.Types as A
import           Data.Maybe (fromMaybe)
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Typeable (Typeable, cast, eqT, (:~:) (..))
import           Numeric.Natural (Natural)
import           Test.QuickCheck.StateModel.Variables (HasNoVariables (..), Var, mkVar)
import           Text.Read (readMaybe)
import qualified Type.Reflection as R

-- | The format's version: a reader refuses a case of another.
caseFormat :: Int
caseFormat = 1

data CaseRecord = CaseRecord
  { format :: Int
  , scenario :: Text
    -- ^ the version of the program the case was drawn from (the runner's
    -- hash of the plan)
  , property :: Text
    -- ^ the property, by its full name
  , size :: Int
    -- ^ the QuickCheck size it was drawn at (a program may read it)
  , items :: [CaseItem]
  }
  deriving (Eq, Show)

-- | One item of the test case: a quantifier's pick, or a step.
data CaseItem
  = ItemPick PickRecord
  | ItemMove MoveRecord
  deriving (Eq, Show)

-- | A pick, by the type it was drawn at and its value. A variable picked
-- ('Test.QuickCheck.DynamicLogic.forAllVar') is the move that bound it.
data PickRecord = PickRecord
  { pickType :: Text
  , value :: Value
  }
  deriving (Eq, Show)

data MoveRecord = MoveRecord
  { n :: Int
    -- ^ the step's number in the case, from 1 (the trace's numbering)
  , kind :: Text
  , trail :: [Text]
    -- ^ where the step was written (a rule's trail); empty for a drawn one
  , fqn :: Text
    -- ^ the operation (or the glue, the fresh value's label, …)
  , negative :: Bool
    -- ^ a step expected to fail (quickcheck-dynamic's 'NegPolarity')
  , generated :: Maybe GeneratedRecord
    -- ^ a step drawn by @anyActions@: how to draw it again
  }
  deriving (Eq, Show)

data GeneratedRecord = GeneratedRecord
  { operation :: Text
  , draws :: [Int]
    -- ^ the positions its generation drew, in order
  , links :: [LinkRecord]
    -- ^ every earlier step its request refers to, where
  }
  deriving (Eq, Show)

-- | A leaf of a step's request that is an earlier step's result: where it
-- sits (the request part, then the position of each node on the way down)
-- and which move bound it.
data LinkRecord = LinkRecord
  { at :: [Int]
  , move :: Int
  }
  deriving (Eq, Show)

-- ---------------------------------------------------------------------------
-- JSON (snake_case, as the trace)

instance ToJSON CaseRecord where
  toJSON CaseRecord {..} = object
    [ "format" .= format, "scenario" .= scenario, "property" .= property, "size" .= size, "items" .= items ]

instance FromJSON CaseRecord where
  parseJSON = withObject "a regression case" $ \o -> do
    format <- o .: "format"
    if format /= caseFormat
      then fail ("a regression case of format " <> show format <> "; this reader reads format " <> show caseFormat)
      else CaseRecord format <$> o .: "scenario" <*> o .: "property" <*> o .: "size" <*> o .: "items"

instance ToJSON CaseItem where
  toJSON = \case
    ItemPick p -> object ["pick" .= p]
    ItemMove m -> object ["move" .= m]

instance FromJSON CaseItem where
  parseJSON = withObject "a case item" $ \o ->
    (ItemPick <$> o .: "pick") <|> (ItemMove <$> o .: "move")

instance ToJSON PickRecord where
  toJSON PickRecord {..} = object ["type" .= pickType, "value" .= value]

instance FromJSON PickRecord where
  parseJSON = withObject "a pick" $ \o -> PickRecord <$> o .: "type" <*> o .: "value"

instance ToJSON MoveRecord where
  toJSON MoveRecord {..} = object
    [ "n" .= n, "kind" .= kind, "trail" .= trail, "fqn" .= fqn, "negative" .= negative, "generated" .= generated ]

instance FromJSON MoveRecord where
  parseJSON = withObject "a move" $ \o -> MoveRecord
    <$> o .: "n" <*> o .: "kind" <*> o .:? "trail" .!= [] <*> o .: "fqn"
    <*> o .:? "negative" .!= False <*> o .:? "generated"

instance ToJSON GeneratedRecord where
  toJSON GeneratedRecord {..} = object ["operation" .= operation, "draws" .= draws, "links" .= links]

instance FromJSON GeneratedRecord where
  parseJSON = withObject "a drawn move" $ \o -> GeneratedRecord
    <$> o .: "operation" <*> o .:? "draws" .!= [] <*> o .:? "links" .!= []

instance ToJSON LinkRecord where
  toJSON LinkRecord {..} = object ["at" .= at, "move" .= move]

instance FromJSON LinkRecord where
  parseJSON = withObject "a link" $ \o -> LinkRecord <$> o .: "at" <*> o .: "move"

-- ---------------------------------------------------------------------------
-- Picks

-- | A pick as the case keeps it. The values a program draws are few and
-- plain (positions, booleans, numbers, texts; 'HasNoVariables' around
-- them when drawn with 'Test.QuickCheck.DynamicLogic.forAllNonVariableQ'),
-- or a variable. Anything else is kept by its 'show' alone, and a replay
-- says it cannot read it back.
encodePick :: forall a. (Typeable a, Show a) => (Int -> Int) -> a -> PickRecord
encodePick moveOfVar a = fromMaybe opaque (plain <|> wrapped <|> variable)
  where
    ty = T.pack (show (R.typeRep @a))
    pick v = PickRecord {pickType = ty, value = v}
    opaque = pick (object ["shown" .= show a])
    plain = pick <$> plainValue a
    wrapped = pick <$> wrappedValue a
    variable = case R.typeRep @a of
      R.App con _ | Just R.HRefl <- R.eqTypeRep con (R.typeRep @Var) -> pick . toJSON . moveOfVar <$> varIndexOf a
      _ -> Nothing
    varIndexOf :: a -> Maybe Int
    varIndexOf x = readMaybe (drop 3 (show x))

plainValue :: forall a. Typeable a => a -> Maybe Value
plainValue a =
      (toJSON <$> cast @a @Int a)
  <|> (toJSON <$> cast @a @Integer a)
  <|> (toJSON <$> cast @a @Natural a)
  <|> (toJSON <$> cast @a @Bool a)
  <|> (toJSON <$> cast @a @Text a)
  <|> (toJSON <$> cast @a @Double a)
  <|> (toJSON <$> cast @a @() a)
  <|> (toJSON <$> cast @a @(Maybe Int) a)
  <|> (toJSON <$> cast @a @(Maybe Text) a)
  <|> (toJSON <$> cast @a @(Maybe Bool) a)

wrappedValue :: forall a. Typeable a => a -> Maybe Value
wrappedValue a =
      (unwrap <$> cast @a @(HasNoVariables Int) a)
  <|> (unwrap <$> cast @a @(HasNoVariables Integer) a)
  <|> (unwrap <$> cast @a @(HasNoVariables Natural) a)
  <|> (unwrap <$> cast @a @(HasNoVariables Bool) a)
  <|> (unwrap <$> cast @a @(HasNoVariables Text) a)
  <|> (unwrap <$> cast @a @(HasNoVariables Double) a)
  <|> (unwrap <$> cast @a @(HasNoVariables ()) a)
  where
    unwrap :: ToJSON x => HasNoVariables x -> Value
    unwrap (HasNoVariables x) = toJSON x

-- | A pick read back at the type the program now draws at. 'Left' says
-- why it cannot be: the program draws another type there, or the value
-- was kept by its 'show' alone.
decodePick :: forall a. Typeable a => (Int -> Int) -> PickRecord -> Either Text a
decodePick varOfMove PickRecord {pickType, value}
  | pickType /= ty = Left ("a pick drawn as " <> pickType <> " is now drawn as " <> ty)
  | otherwise = maybe (Left ("a pick of type " <> ty <> " cannot be read back")) Right (plain <|> wrapped <|> variable)
  where
    ty = T.pack (show (R.typeRep @a))
    parsed :: forall x. FromJSON x => Maybe x
    parsed = A.parseMaybe parseJSON value
    as :: forall x. (Typeable x, FromJSON x) => Maybe a
    as = case eqT @a @x of
      Just Refl -> parsed @x
      Nothing -> Nothing
    asWrapped :: forall x. (Typeable x, FromJSON x) => Maybe a
    asWrapped = case eqT @a @(HasNoVariables x) of
      Just Refl -> HasNoVariables <$> parsed @x
      Nothing -> Nothing
    plain = as @Int <|> as @Integer <|> as @Natural <|> as @Bool <|> as @Text <|> as @Double <|> as @()
        <|> as @(Maybe Int) <|> as @(Maybe Text) <|> as @(Maybe Bool)
    wrapped = asWrapped @Int <|> asWrapped @Integer <|> asWrapped @Natural <|> asWrapped @Bool
          <|> asWrapped @Text <|> asWrapped @Double <|> asWrapped @()
    variable = case R.typeRep @a of
      R.App con _ | Just R.HRefl <- R.eqTypeRep con (R.typeRep @Var) ->
        (\m -> mkVarAt (varOfMove m)) <$> parsed @Int
      _ -> Nothing
    -- the variable at the index, at the type @a@ is (a @Var x@)
    mkVarAt :: Int -> a
    mkVarAt i = case R.typeRep @a of
      R.App con _ | Just R.HRefl <- R.eqTypeRep con (R.typeRep @Var) -> mkVar i
      _ -> error "decodePick: not a variable"

-- | A variable's index (quickcheck-dynamic shows a variable as @var<n>@ and
-- keeps its constructor to itself).
varIndex :: Var a -> Int
varIndex v = fromMaybe (error ("varIndex: " <> show v)) (readMaybe (drop 3 (show v)))
