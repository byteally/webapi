{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
-- | A scenario's moves, as DL (zenbridge #2714).
--
-- A move is one action by an actor. Its things are picked by the stage
-- they are predicted at ('pickAt'); then it begins (a step that performs
-- nothing), plays its own steps (calls, glue, draws: ordinary DL), and
-- ends ('MoveSettle'): the prediction moves on, the host settles what the
-- move caused and observes it, and the property's 'Judge' holds every
-- thing against the observation. A move that does not settle within its
-- budget is inconclusive.
--
-- > anyMoves 10
-- >   [ (3, send850)
-- >   , (1, do o <- pickAt "orders" "855 sent" (\_ _ -> True)
-- >            move Move { name = "replace850", picked = [(o, Just "850 arrives")], .. }) ]
--
-- Narrowing keeps or drops a move whole, and makes every shorter case
-- again from its picks through the program
-- ('Test.WebApi.Replay.narrowThroughProgram').
module Test.WebApi.Moves
  ( module Test.WebApi.Prediction
  , anyMoves
  , pickAt
  , Move (..)
  , MoveBody (..)
  , move
  ) where

import           Control.Applicative (Alternative (..))
import           Control.Monad (void, when)
import qualified Data.Map.Strict as M
import qualified Data.Set as Set
import           Data.Foldable (asum)
import           Data.Text (Text)
import qualified Data.Text as T
import           Test.QuickCheck.DynamicLogic (DL, action, exactlyQ, forAllQ, getModelStateDL, stopping, weight)
import           Test.QuickCheck.StateModel.Variables (HasNoVariables (..))
import           Test.QuickCheck.StateModel (Action, Any, StateModel, Var)
import           Test.WebApi.Prediction
import           Test.WebApi.StateModel
import           Test.WebApi.Trace (StepInfo)

-- | Moves drawn one after another, @n@ on average (as 'Test.QuickCheck.DynamicLogic.anyActions'
-- draws actions), each by its weight among the moves allowed at that
-- point. A move that is not allowed now (its pick finds nothing) is
-- 'empty' and takes no share.
--
-- Each draw of a move opens with a pick of nothing (a unit): once a case
-- is past its size, quickcheck-dynamic prefers to stop, and drops only
-- the branches that open with a pick or a drawn action; a move that makes
-- a thing opens with a plain step, and without the pick the case would
-- run on to the step limit.
anyMoves :: Int -> [(Double, DL s ())] -> DL s ()
anyMoves n moves = loop
  where
    weighted = [ (w, m) | (w, m) <- moves, w > 0 ]
    total = sum (map fst weighted)
    loop = stopping <|> pure () <|> (aMove >> loop)
    aMove = do
      HasNoVariables () <- forAllQ (exactlyQ (HasNoVariables ()))
      asum [ weight (fromIntegral n * w / total) >> m | (w, m) <- weighted ]

-- | A thing of the track at the stage, among those the guard allows, by
-- position (a pick narrowing moves toward the oldest); 'empty' when there
-- is none. The guard sees the whole prediction.
pickAt :: (StateModel xstate, Eq (Action xstate Text)) => Text -> Text -> (Prediction -> Thing -> Bool) -> DL (ApiState s c xstate apps) Thing
pickAt tr sg allowed = do
  ApiState {prediction} <- getModelStateDL
  case [ t | t <- thingsAt tr sg prediction, allowed prediction t ] of
    [] -> empty
    ts -> elementsM ts

-- | A move, as the layer above builds it once its things are picked. A
-- guard over several picks goes before it ('Control.Monad.guard'): once a
-- move has begun, it is played.
data Move s c xstate apps = Move
  { name :: Text
  , info :: StepInfo
    -- ^ where it was written, for the trace and for a regression case
  , picked :: [(Thing, Maybe Text)]
    -- ^ each thing it picked and where it sends it (none: it stays, and
    -- no answer moves it)
  , makes :: [(Text, Text)]
    -- ^ the track and stage of each thing it makes
  , body :: DL (ApiState s c xstate apps) MoveBody
    -- ^ its steps, and what they gave
  , tracks :: M.Map Text Track
  , judge :: Judge
  , budget :: Int
    -- ^ seconds the host may take to settle it
  }

-- | What a move's steps gave.
data MoveBody = MoveBody
  { values :: [AnyVal]
    -- ^ the values of the things it makes, one per 'makes'
  , labels :: Set.Set Text
    -- ^ what its draws said, onto every thing it touches
  , uses :: Set.Set (Any Var)
    -- ^ the variables those values refer to beyond their own ('Val's
    -- carry theirs), for a value the engine cannot look into
  }

-- | The move played: it begins, its steps, it ends.
move :: (StateModel xstate, Eq (Action xstate ())) => Move s c xstate apps -> DL (ApiState s c xstate apps) ()
move Move {..} = do
  void (action (mkWebApiAction (MoveBegin name info)))
  MoveBody {values, labels, uses} <- body
  when (length values /= length makes) $
    fail ("move " <> T.unpack name <> " makes " <> show (length makes) <> " things, and its steps gave " <> show (length values))
  void $ action $ mkWebApiAction $ MoveSettle MoveEnd
    { name
    , info
    , picked = [ (thingId, stage, to) | (Thing {thingId, stage}, to) <- picked ]
    , makes = zipWith (\(tr, sg) v -> (tr, sg, v)) makes values
    , labels
    , tracks
    , judge
    , budget
    , uses
    }
