{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
-- | What a scenario predicts (zenbridge #2714): the things it tracks, the
-- stage each is predicted to be at, and how the platform's own answers
-- move a thing on. A case is drawn against the prediction alone; after
-- each move settles, a 'Judge' holds it against what the host observes.
--
-- * A /thing/ is one tracked item of a kind (an order, a shipment), made by
--   a move. Its id is its place in the order things were made.
-- * A /track/ is one kind of thing: its stages, the stages marked as
--   refusals, and the /answers/, the paths the platform takes by itself.
--   The answers leaving a stage are an ordered list; the first whose
--   @when@ holds is taken.
-- * A thing at a stage is also recorded under the class @track\@stage@
--   ('stageClassName'), the model's ordinary classes.
--
-- The engine names no host: what a thing's value means, what an
-- observation is and what the checks read are the layer above's.
module Test.WebApi.Prediction
  ( -- * Things
    ThingId (..)
  , Thing (..)
  , thingSubject
  , Prediction (..)
  , emptyPrediction
  , predictedThings
  , thingsAt
  , stageClassName
    -- * Tracks
  , Answer (..)
  , Track (..)
  , follow
    -- * Judging a move
  , MoveEnd (..)
  , Judge (..)
  , noJudge
  , StageOf (..)
  , ThingCheck (..)
  , PredictionCheck (..)
  , SomeObservation (..)
    -- * The host
  , MoveHost (..)
  , defMoveHost
  , MoveInconclusive (..)
  ) where

import           Control.Exception (Exception)
import qualified Data.Map.Strict as M
import qualified Data.Set as Set
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Typeable (Typeable)
import           Test.QuickCheck.StateModel (LookUp)
import           Test.WebApi (WebApiSessions)
import           Test.WebApi.Trace (StepInfo)
import           Test.WebApi.Val (AnyVal)

-- ---------------------------------------------------------------------------
-- Things

-- | A thing's place in the order things were made, from 1.
newtype ThingId = ThingId Int
  deriving newtype (Show, Eq, Ord)

data Thing = Thing
  { thingId :: ThingId
  , track :: Text
  , stage :: Text
    -- ^ the stage it is predicted to be at
  , movesOn :: [Text]
    -- ^ the moves made on it, oldest first (its making move included)
  , labels :: Set.Set Text
    -- ^ what the draws of its moves said of it
  , value :: AnyVal
    -- ^ what its making move gave it; a leaf still symbolic when the case
    -- is drawn (a fresh value, an earlier step's result) stays opaque
  }

-- | How a check names the thing: its track and its id.
thingSubject :: Thing -> Text
thingSubject Thing {track, thingId = ThingId k} = track <> " #" <> T.pack (show k)

data Prediction = Prediction
  { things :: M.Map ThingId Thing
  , nextThing :: Int
    -- ^ the id the next thing made gets
  }

emptyPrediction :: Prediction
emptyPrediction = Prediction {things = M.empty, nextThing = 1}

-- | Every thing, oldest first.
predictedThings :: Prediction -> [Thing]
predictedThings Prediction {things} = M.elems things

-- | The things of the track at the stage, oldest first.
thingsAt :: Text -> Text -> Prediction -> [Thing]
thingsAt tr sg p = [ t | t@Thing {track, stage} <- predictedThings p, track == tr, stage == sg ]

-- | The class a thing at a stage is recorded under.
stageClassName :: Text -> Text -> Text
stageClassName tr sg = tr <> "@" <> sg

-- ---------------------------------------------------------------------------
-- Tracks

-- | A path the platform takes by itself, from one stage to another.
data Answer = Answer
  { from :: Text
  , to :: Text
  , when :: Maybe (Prediction -> Thing -> Bool)
    -- ^ none: always taken (the last answer leaving its stage)
  , label :: Maybe Text
    -- ^ for the map
  }

data Track = Track
  { name :: Text
  , stages :: [Text]
  , refusals :: Set.Set Text
    -- ^ the stages a thing that should be refused ends at
  , answers :: [Answer]
  }

-- | The thing carried along the track's answers from where it stands:
-- at each stage the first answer whose @when@ holds, until no answer
-- leaves the stage. 'Left' when the answers go round in a circle.
follow :: Track -> Prediction -> Thing -> Either Text Thing
follow Track {name = trackName, answers} Prediction {things = known, nextThing} = go Set.empty
  where
    go seen t@Thing {stage, thingId}
      | Set.member stage seen = Left ("the answers of track " <> trackName <> " go round in a circle at " <> stage)
      | otherwise =
          let p = Prediction {things = M.insert thingId t known, nextThing}
              taken = [ a | a@Answer {from, when} <- answers, from == stage, maybe True (\w -> w p t) when ]
          in case taken of
               [] -> Right t
               Answer {to} : _ -> go (Set.insert stage seen) t {stage = to}

-- ---------------------------------------------------------------------------
-- Judging a move

-- | A move's closing step, as the engine plays it ("Test.WebApi.Moves"):
-- what it does to the prediction, and how the property judges it.
data MoveEnd = MoveEnd
  { name :: Text
  , info :: StepInfo
  , picked :: [(ThingId, Text, Maybe Text)]
    -- ^ each thing it picked, the stage it was picked at, and where it
    -- goes (none: it stays, and no answer moves it)
  , makes :: [(Text, Text, AnyVal)]
    -- ^ each thing it makes: its track, its stage, its value
  , labels :: Set.Set Text
    -- ^ what the move's draws said, onto every thing it touches
  , tracks :: M.Map Text Track
  , judge :: Judge
  , budget :: Int
    -- ^ seconds the host may take to settle
  }

-- | The stage a thing is observed at.
newtype StageOf obs = StageOf (LookUp -> obs -> Thing -> Either Text Text)

-- | A check of one thing against the observation, seeing the prediction.
newtype ThingCheck obs = ThingCheck (LookUp -> Prediction -> Thing -> obs -> Either Text ())

-- | A check across things.
newtype PredictionCheck obs = PredictionCheck (LookUp -> Prediction -> obs -> Either Text ())

-- | A property's refinements and checks, over the observation the host
-- makes (its type is the layer above's: the host's observation is cast to
-- it). After every move, for every thing: its observed stage equals the
-- predicted one, then the checks at that stage; the checks after the move
-- for the things it touched; the checks across things.
data Judge where
  Judge :: Typeable obs =>
    { stageOf :: M.Map Text (StageOf obs)
      -- ^ by track; a track with none is not observed
    , atStage :: [(Text, (Text, Text), ThingCheck obs)]
      -- ^ name, (track, stage)
    , afterMove :: [(Text, Text, ThingCheck obs)]
      -- ^ name, move
    , always :: [(Text, PredictionCheck obs)]
    } -> Judge

-- | Nothing to judge.
noJudge :: Judge
noJudge = Judge {stageOf = M.empty @Text @(StageOf ()), atStage = [], afterMove = [], always = []}

-- | What the host observed, whatever its type.
data SomeObservation where
  SomeObservation :: (Typeable o, Show o) => o -> SomeObservation

instance Show SomeObservation where
  show (SomeObservation o) = show o

-- ---------------------------------------------------------------------------
-- The host

-- | What the host does after a move: wait until what the move caused has
-- settled, within the move's budget; observe; and the checks it makes
-- itself on every flow.
data MoveHost apps = MoveHost
  { settle :: Int -> WebApiSessions apps (Either Text ())
    -- ^ 'Left' says why it did not settle within the budget (seconds):
    -- the move is inconclusive
  , observe :: WebApiSessions apps (Either Text SomeObservation)
    -- ^ 'Left': nothing could be observed, and the move is inconclusive
  , hostChecks :: SomeObservation -> [(Text, Either Text ())]
  }

-- | A host whose moves settle at once, that observes nothing.
defMoveHost :: MoveHost apps
defMoveHost = MoveHost
  { settle = \_ -> pure (Right ())
  , observe = pure (Right (SomeObservation ()))
  , hostChecks = const []
  }

-- | A move that did not settle within its budget, or could not be
-- observed: neither passed nor failed. The case runner discards the case
-- and the trace says so.
data MoveInconclusive = MoveInconclusive
  { move :: Text
  , why :: Text
  }
  deriving Show

instance Exception MoveInconclusive
