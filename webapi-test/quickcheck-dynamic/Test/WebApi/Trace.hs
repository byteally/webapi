{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
-- | A run's trace (zenbridge #2712): what each case played, step by step,
-- as data rather than as QuickCheck's counterexample text.
--
-- * One 'StepRecord' per step a case played: the request as sent, the
--   answer as received, every check by name with whether it held, the
--   labels that held, the model after the step, and the stage before and
--   after it (a model's own hook; none by default).
-- * For the run ('RunTrace'): one summary per generated case and per
--   narrowing try, the first failing case and the narrowed one in full,
--   and the label and move counts over the generated cases.
--
-- The records are plain text and numbers: the engine renders what it can
-- show and names no model. A 'Recorder' is one property's sink. The state
-- model's @perform@ and @postcondition@ write the case being played into
-- it ("Test.WebApi.StateModel"), and the property's QuickCheck callbacks
-- close each case ('recordedProperty'): 'P.PostTest' fires after every
-- test, a generated case and each narrowing try alike, and
-- 'P.PostFinalFailure' once, on the narrowed case. So the phases are told
-- apart without reading any text.
module Test.WebApi.Trace
  ( -- * The records
    StepInfo (..)
  , stepLabel
  , StepKind (..)
  , Expectation (..)
  , RequestRecord (..)
  , FileRecord (..)
  , AnswerRecord (..)
  , CheckRecord (..)
  , StageRecord (..)
  , StepRecord (..)
  , CaseOutcome (..)
  , CaseSummary (..)
  , TrySummary (..)
  , CaseTrace (..)
  , LabelCount (..)
  , MoveCount (..)
  , RunTrace (..)
    -- * Recording
  , Recorder
  , newRecorder
  , recordedProperty
  , readRunTrace
  , beginCase
  , pushStep
  , amendStep
  , afterStep
    -- * Rendering helpers
  , clipText
  , bodyText
  ) where

import           Control.Applicative ((<|>))
import           Data.Aeson (ToJSON (..), Value (..), object, (.=))
import qualified Data.Aeson.Key as K
import qualified Data.ByteString.Lazy as LBS
import           Data.Foldable (toList)
import           Data.IORef
import           Data.List (find)
import qualified Data.Map.Strict as M
import           Data.Sequence (Seq, (|>))
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import           Test.QuickCheck (Property)
import qualified Test.QuickCheck.Property as P

-- ---------------------------------------------------------------------------
-- The records

-- | Where a step was written, when a layer above the engine knows (a
-- script's rule): the rule's trail, its summary, and the name the layer
-- knows the step's operation by (the engine knows only the operation id).
data StepInfo = StepInfo
  { trail :: [Text]
  , summary :: Text
  , name :: Maybe Text
  }
  deriving (Eq, Show)

-- | The step's label as the counterexample text shows it: @rule: summary@.
stepLabel :: StepInfo -> Text
stepLabel StepInfo {trail, summary} = T.intercalate "." trail <> ": " <> summary

-- | What a step is. A /move/ is a call or a glue step; the rest are the
-- engine's own (a fresh value, a context switch, a step that only moves
-- the model).
data StepKind = KindCall | KindGlue | KindFresh | KindContext | KindModel | KindOther
  deriving (Eq, Show)

kindText :: StepKind -> Text
kindText = \case
  KindCall -> "call"
  KindGlue -> "glue"
  KindFresh -> "fresh"
  KindContext -> "context"
  KindModel -> "model"
  KindOther -> "other"

isMove :: StepKind -> Bool
isMove k = k == KindCall || k == KindGlue

-- | What a call expects of its answer.
data Expectation
  = GoesThrough
  | Refused (Maybe Int)
    -- ^ refused with this status (any error status: 'Nothing')
  deriving (Eq, Show)

data FileRecord = FileRecord
  { field :: Text
  , fileName :: Text
  , contentType :: Text
  }
  deriving (Eq, Show)

-- | The request as it went out: its fresh values filled in, credentials
-- masked, the body clipped. Or the field nobody supplied, when it never
-- went out.
data RequestRecord
  = RequestSent
      { line :: Text
      , headers :: [(Text, Text)]
      , form :: [(Text, Text)]
      , files :: [FileRecord]
      , body :: Maybe Text
      , bodyType :: Maybe Text
      }
  | RequestUnset Text
  deriving (Eq, Show)

-- | What came back: the status and the body as received (a call), the
-- step's value as the engine shows it, or why there was none.
data AnswerRecord = AnswerRecord
  { httpStatus :: Maybe Int
  , body :: Maybe Text
  , value :: Maybe Text
  , failure :: Maybe Text
  }
  deriving (Eq, Show)

data CheckRecord = CheckRecord
  { check :: Text
  , held :: Bool
  , why :: Maybe Text
  }
  deriving (Eq, Show)

data StageRecord = StageRecord
  { before :: Maybe Text
  , after :: Maybe Text
  }
  deriving (Eq, Show)

data StepRecord = StepRecord
  { n :: Int
    -- ^ the step's position in its case, from 1
  , kind :: StepKind
  , info :: Maybe StepInfo
  , operation :: Text
    -- ^ the operation id (a call), the transform (a glue step), or what the
    -- engine step is
  , method :: Maybe Text
  , expect :: Maybe Expectation
  , request :: Maybe RequestRecord
  , answer :: Maybe AnswerRecord
  , checks :: [CheckRecord]
  , labels :: [Text]
    -- ^ the labels that held at this step
  , model :: Maybe (M.Map Text [Text])
    -- ^ the model after the step: each class with its values; 'Nothing'
    -- when the step failed (no next state is computed then)
  , stage :: StageRecord
  , ok :: Bool
  }
  deriving (Eq, Show)

-- | The name a summary lists a move by: its rule, else its operation.
moveName :: StepRecord -> Text
moveName StepRecord {info, operation} = case info of
  Just StepInfo {trail = t@(_ : _)} -> T.intercalate "." t
  _ -> operation

-- | The name the move counts go by: the operation's name where the layer
-- above knows one, else the operation id.
moveCountKey :: StepRecord -> Text
moveCountKey StepRecord {info, operation} = case info of
  Just StepInfo {name = Just nm} -> nm
  _ -> operation

data CaseOutcome = Passed | Failed | Discarded
  deriving (Eq, Show)

outcomeText :: CaseOutcome -> Text
outcomeText = \case
  Passed -> "passed"
  Failed -> "failed"
  Discarded -> "discarded"

-- | One generated case, in a line.
data CaseSummary = CaseSummary
  { number :: Int
  , played :: Bool
    -- ^ whether any step ran: a case the draw refused (a failing
    -- @assertModel@) plays nothing
  , moves :: [Text]
  , outcome :: CaseOutcome
  , at :: Maybe Int
    -- ^ the failing step's 'n'
  , why :: Maybe Text
  , labels :: [Text]
  }
  deriving (Eq, Show)

-- | One narrowing try, in a line: 'taken' when it still failed, so
-- narrowing went on from it.
data TrySummary = TrySummary
  { number :: Int
  , played :: Bool
  , moves :: [Text]
  , outcome :: CaseOutcome
  , at :: Maybe Int
  , why :: Maybe Text
  , taken :: Bool
  }
  deriving (Eq, Show)

-- | A case in full: @case 3@ or @try 7@.
data CaseTrace = CaseTrace
  { from :: Text
  , steps :: [StepRecord]
  }
  deriving (Eq, Show)

-- | In how many of the generated cases a label held. A declared label no
-- case reached counts 0.
data LabelCount = LabelCount
  { label :: Text
  , cases :: Int
  , outOf :: Int
  }
  deriving (Eq, Show)

-- | How many times a move was played over the generated cases.
data MoveCount = MoveCount
  { move :: Text
  , timesPlayed :: Int
  }
  deriving (Eq, Show)

data RunTrace = RunTrace
  { cases :: [CaseSummary]
  , discarded :: Int
    -- ^ the generated cases discarded before they counted (QuickCheck
    -- draws again); they are not in 'cases'
  , narrowing :: [TrySummary]
  , firstFailure :: Maybe CaseTrace
  , narrowed :: Maybe CaseTrace
  , labels :: [LabelCount]
  , moves :: [MoveCount]
  }
  deriving (Eq, Show)

-- ---------------------------------------------------------------------------
-- JSON (snake_case, the names the plan runs' per-rule record uses where a
-- field means the same)

instance ToJSON StepKind where
  toJSON = String . kindText

instance ToJSON Expectation where
  toJSON = \case
    GoesThrough -> String "goes through"
    Refused st -> object ["refused" .= st]

instance ToJSON FileRecord where
  toJSON FileRecord {..} = object ["field" .= field, "name" .= fileName, "content_type" .= contentType]

instance ToJSON RequestRecord where
  toJSON = \case
    RequestSent {..} -> object
      [ "line" .= line
      , "headers" .= pairs headers
      , "form" .= pairs form
      , "files" .= files
      , "body" .= body
      , "body_type" .= bodyType
      ]
    RequestUnset fld -> object ["unset" .= fld]
    where
      pairs kvs = object [ K.fromText k .= v | (k, v) <- kvs ]

instance ToJSON AnswerRecord where
  toJSON AnswerRecord {..} = object
    [ "http_status" .= httpStatus, "body" .= body, "value" .= value, "failure" .= failure ]

instance ToJSON CheckRecord where
  toJSON CheckRecord {..} = object ["check" .= check, "held" .= held, "why" .= why]

instance ToJSON StageRecord where
  toJSON StageRecord {..} = case (before, after) of
    (Nothing, Nothing) -> Null
    _ -> object ["before" .= before, "after" .= after]

instance ToJSON StepRecord where
  toJSON StepRecord {..} = object
    [ "n" .= n
    , "kind" .= kind
    , "trail" .= maybe [] (\StepInfo {trail} -> trail) info
    , "summary" .= fmap (\StepInfo {summary} -> summary) info
    , "fqn" .= maybe operation id (info >>= \StepInfo {name} -> name)
    , "operation" .= operation
    , "method" .= method
    , "expect" .= expect
    , "request" .= request
    , "answer" .= answer
    , "checks" .= checks
    , "labels" .= labels
    , "model" .= model
    , "stage" .= stage
    , "outcome" .= (if ok then "ok" else "failed" :: Text)
    ]

instance ToJSON CaseSummary where
  toJSON CaseSummary {..} = object
    [ "case" .= number, "played" .= played, "moves" .= moves, "outcome" .= outcomeText outcome
    , "at" .= at, "why" .= why, "labels" .= labels ]

instance ToJSON TrySummary where
  toJSON TrySummary {..} = object
    [ "try" .= number, "played" .= played, "moves" .= moves, "outcome" .= outcomeText outcome
    , "at" .= at, "why" .= why, "taken" .= taken ]

instance ToJSON CaseTrace where
  toJSON CaseTrace {..} = object ["from" .= from, "moves" .= steps]

instance ToJSON LabelCount where
  toJSON LabelCount {..} = object ["label" .= label, "cases" .= cases, "of" .= outOf]

instance ToJSON MoveCount where
  toJSON MoveCount {..} = object ["move" .= move, "played" .= timesPlayed]

instance ToJSON RunTrace where
  toJSON RunTrace {..} = object
    [ "cases" .= cases
    , "discarded" .= discarded
    , "narrowing" .= narrowing
    , "first_failure" .= firstFailure
    , "narrowed" .= narrowed
    , "labels" .= labels
    , "moves" .= moves
    ]

-- ---------------------------------------------------------------------------
-- Rendering helpers

-- | At most @k@ characters, an ellipsis marking a cut.
clipText :: Int -> Text -> Text
clipText k t
  | T.length t <= k = t
  | otherwise = T.take k t <> "…"

-- | A body as text: UTF-8 as it is (clipped), anything else by its size.
bodyText :: Int -> LBS.ByteString -> Maybe Text
bodyText k bs
  | LBS.null bs = Nothing
  | otherwise = Just $ case TE.decodeUtf8' (LBS.toStrict bs) of
      Right t -> clipText k t
      Left _ -> "<" <> T.pack (show (LBS.length bs)) <> " bytes, not text>"

-- ---------------------------------------------------------------------------
-- Recording

-- | One property's sink: the case being played, and what the run has
-- gathered so far.
data Recorder = Recorder
  { current :: IORef (Seq StepRecord)
  , gathered :: IORef Gathered
  , declared :: [Text]
  }

data Gathered = Gathered
  { narrowingStarted :: Bool
  , caseCount :: Int
  , tryCount :: Int
  , discardedCount :: Int
  , caseLines :: Seq CaseSummary
  , tryLines :: Seq TrySummary
  , firstFailing :: Maybe CaseTrace
  , lastTaken :: Maybe CaseTrace
  , final :: Maybe CaseTrace
  , countedCases :: Int
  , labelCounts :: M.Map Text Int
  , moveCounts :: M.Map Text Int
  }

-- | A recorder for one property, given the labels its steps declare (a
-- declared label no case reaches reads 0, not absent).
newRecorder :: [Text] -> IO Recorder
newRecorder declaredLabels = do
  cur <- newIORef Seq.empty
  g <- newIORef Gathered
    { narrowingStarted = False
    , caseCount = 0
    , tryCount = 0
    , discardedCount = 0
    , caseLines = Seq.empty
    , tryLines = Seq.empty
    , firstFailing = Nothing
    , lastTaken = Nothing
    , final = Nothing
    , countedCases = 0
    , labelCounts = M.empty
    , moveCounts = M.empty
    }
  pure Recorder {current = cur, gathered = g, declared = declaredLabels}

-- | A case starts: whatever an interrupted one left is dropped.
beginCase :: Recorder -> IO ()
beginCase Recorder {current} = writeIORef current Seq.empty

-- | A step ran: append it, numbered.
pushStep :: Recorder -> StepRecord -> IO ()
pushStep Recorder {current} StepRecord {..} =
  modifyIORef' current $ \s -> s |> StepRecord {n = Seq.length s + 1, ..}

-- | Change the step last pushed (its postcondition ran).
amendStep :: Recorder -> (StepRecord -> StepRecord) -> IO ()
amendStep Recorder {current} f = modifyIORef' current $ \s -> case Seq.viewr s of
  Seq.EmptyR -> s
  rest Seq.:> lastStep -> rest |> f lastStep

-- | What a step's postcondition adds: its checks, the labels that held,
-- the model after it, the stage after it, and whether it all held.
afterStep :: [CheckRecord] -> [Text] -> M.Map Text [Text] -> Maybe Text -> Bool -> StepRecord -> StepRecord
afterStep cs ls m stageAfter held StepRecord {checks, stage = StageRecord {before}, ok, ..} = StepRecord
  { checks = checks ++ cs
  , labels = ls
  , model = Just m
  , stage = StageRecord {before, after = stageAfter}
  , ok = ok && held
  , ..
  }

-- | The property, its cases closed into the recorder as QuickCheck
-- finishes each one.
recordedProperty :: Recorder -> Property -> Property
recordedProperty rec =
  P.callback (P.PostFinalFailure P.NotCounterexample (\_ _ -> finalFailure rec))
    . P.callback (P.PostTest P.NotCounterexample (\_ res -> closeCase rec res))

closeCase :: Recorder -> P.Result -> IO ()
closeCase Recorder {current, gathered} res = do
  stepList <- toList <$> atomicModifyIORef' current (\s -> (Seq.empty, s))
  let
    outcome = case P.ok res of
      Just True -> Passed
      Just False -> Failed
      Nothing -> Discarded
    failing = find (\StepRecord {ok} -> not ok) stepList
    at = (\StepRecord {n} -> n) <$> failing
    why
      | outcome == Failed = (failing >>= stepWhy) <|> nonEmpty (T.pack (P.reason res))
      | otherwise = Nothing
    played = not (null stepList)
    moveList = [ moveName s | s@StepRecord {kind} <- stepList, isMove kind ]
    countKeys = [ moveCountKey s | s@StepRecord {kind} <- stepList, isMove kind ]
    held = Set.toList (Set.fromList (map T.pack (P.classes res)))
  modifyIORef' gathered $ \g@Gathered {..} ->
    if not narrowingStarted
      then case outcome of
        Discarded -> g {discardedCount = discardedCount + 1}
        _ ->
          let k = caseCount + 1
              line' = CaseSummary {number = k, played, moves = moveList, outcome, at, why, labels = held}
          in g { caseCount = k
               , caseLines = caseLines |> line'
               , countedCases = countedCases + 1
               , labelCounts = M.unionWith (+) labelCounts (M.fromList [ (l, 1) | l <- held ])
               , moveCounts = M.unionWith (+) moveCounts (M.fromListWith (+) [ (m, 1) | m <- countKeys ])
               , firstFailing = if outcome == Failed then Just CaseTrace {from = "case " <> tshow k, steps = stepList} else firstFailing
               , narrowingStarted = outcome == Failed
               }
      else
        let k = tryCount + 1
            taken = outcome == Failed
            line' = TrySummary {number = k, played, moves = moveList, outcome, at, why, taken}
        in g { tryCount = k
             , tryLines = tryLines |> line'
             , lastTaken = if taken then Just CaseTrace {from = "try " <> tshow k, steps = stepList} else lastTaken
             }
  where
    stepWhy StepRecord {checks, answer} =
      case [ w | CheckRecord {held = False, why = Just w} <- checks ] of
        (w : _) -> Just w
        [] -> answer >>= \AnswerRecord {failure} -> failure
    nonEmpty t = if T.null (T.strip t) then Nothing else Just t

finalFailure :: Recorder -> IO ()
finalFailure Recorder {gathered} = modifyIORef' gathered $ \g@Gathered {lastTaken, firstFailing} ->
  g {final = lastTaken <|> firstFailing}

-- | The run as recorded so far (read it once the property has run).
readRunTrace :: Recorder -> IO RunTrace
readRunTrace Recorder {gathered, declared} = do
  Gathered {..} <- readIORef gathered
  let
    seen = M.keys labelCounts
    extra = filter (`notElem` declared) seen
    labelOrder = dedupe declared ++ extra
  pure RunTrace
    { cases = toList caseLines
    , discarded = discardedCount
    , narrowing = toList tryLines
    , firstFailure = firstFailing
    , narrowed = final <|> (if narrowingStarted then lastTaken <|> firstFailing else Nothing)
    , labels = [ LabelCount {label = l, cases = M.findWithDefault 0 l labelCounts, outOf = countedCases} | l <- labelOrder ]
    , moves = [ MoveCount {move = m, timesPlayed = c} | (m, c) <- M.toList moveCounts ]
    }
  where
    dedupe = go Set.empty
      where
        go _ [] = []
        go s (x : xs)
          | Set.member x s = go s xs
          | otherwise = x : go (Set.insert x s) xs

tshow :: Show a => a -> Text
tshow = T.pack . show
