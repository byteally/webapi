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
    -- * A move as it was played
  , MovePlayed (..)
  , MoveStep (..)
  , Sent (..)
  , Answered (..)
  , sentAs
  , answeredAs
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
import           Data.Typeable (Typeable, cast)
import           Test.QuickCheck.StateModel (Any, LookUp, Var)
import           Test.WebApi (ClientRequest, WebApiSessions)
import           Test.WebApi.Trace (StepInfo, StepKind)
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
  , uses :: Set.Set (Any Var)
    -- ^ the variables the things it makes refer to: the DL keeps the
    -- steps that bind them
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
-- A move as it was played

-- | A move as it was played (zenbridge #2715): its name, where it was
-- written, and its own steps in the order they ran. The host is handed it
-- with every call it gets about the move ('MoveHost'), so it can tell what
-- the move did (what it wrote, and what came back) without keeping
-- anything between those calls.
data MovePlayed = MovePlayed
  { name :: Text
  , info :: StepInfo
  , steps :: [MoveStep]
    -- ^ every call, every effect step (a foreign transform, a host's own
    -- step) and every fresh value the move's body ran, oldest first. The
    -- move's begin and settle steps are not among them, nor are the
    -- engine's own steps (one that only moves the model, a context switch,
    -- a model's extended action), nor any step outside the move
  }

-- | One of a move's own steps, as the engine held it when it ran. Nothing
-- is rendered, clipped or masked: these are the values themselves, read by
-- a cast to the types the host knows ('sentAs', 'answeredAs'). A step that
-- fails ends its case before the move's settle step, so every step here
-- has its value.
data MoveStep = MoveStep
  { kind :: StepKind
    -- ^ a call, an effect step (glue) or a fresh value
  , info :: Maybe StepInfo
    -- ^ where it was written, when a layer above the engine knows: its
    -- rule, and the name that layer knows its operation by
  , operation :: Text
    -- ^ the operation id (a call), the effect's name, or the fresh value's
    -- label, as the trace's record of the step names it
  , sent :: Maybe Sent
    -- ^ a call's request as it went out; none for any other step (the
    -- engine never holds an effect step's input as a value)
  , value :: Answered
    -- ^ what the step gave: a call's result as its caller made it of the
    -- answer (of the error, for a call that was expected to be refused),
    -- an effect step's result, a fresh value's token
  }

-- | A call's request as it went out: its variables looked up, its fresh
-- values and its files as the step made them.
data Sent where
  Sent :: Typeable (ClientRequest meth r) => ClientRequest meth r -> Sent

-- | What a step gave, whatever its type.
data Answered where
  Answered :: (Typeable a, Show a) => a -> Answered

instance Show Answered where
  show (Answered a) = show a

-- | The step's request, when it is a call of this method and route:
-- @sentAs \@POST \@ItemsR step@.
sentAs :: forall meth r. Typeable (ClientRequest meth r) => MoveStep -> Maybe (ClientRequest meth r)
sentAs MoveStep {sent} = sent >>= \(Sent r) -> cast r

-- | The step's value, when it is of this type.
answeredAs :: forall a. Typeable a => MoveStep -> Maybe a
answeredAs MoveStep {value = Answered a} = cast a

-- ---------------------------------------------------------------------------
-- The host

-- | What the host does after a move: wait until what the move caused has
-- settled, within the move's budget; observe; and the checks it makes
-- itself on every flow. Each call about a move is handed the move as it
-- was played, the same value each time.
data MoveHost apps = MoveHost
  { settle :: MovePlayed -> Int -> WebApiSessions apps (Either Text ())
    -- ^ 'Left' says why it did not settle within the budget (seconds):
    -- the move is inconclusive
  , observe :: MovePlayed -> WebApiSessions apps (Either Text SomeObservation)
    -- ^ 'Left': nothing could be observed, and the move is inconclusive
  , hostChecks :: SomeObservation -> [(Text, Either Text ())]
  }

-- | A host whose moves settle at once, that observes nothing. Build a
-- host from it (@defMoveHost {settle = …, observe = …}@), so that a field
-- added later breaks nothing.
defMoveHost :: MoveHost apps
defMoveHost = MoveHost
  { settle = \_ _ -> pure (Right ())
  , observe = \_ -> pure (Right (SomeObservation ()))
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
