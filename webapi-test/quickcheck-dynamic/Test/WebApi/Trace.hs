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
--
-- A move's settle step also carries the /evidence/ its host attached to
-- the move (zenbridge #2715, 'Evidence'): what the host noted about it,
-- which changes no outcome. The run's trace counts it by kind.
--
-- A recorder may hand what it records to a sink as it happens (zenbridge
-- #2741, 'newRecorderTo'): each step once it is final ('StepEnded') and
-- each case as it closes ('CaseEnded'), so a run can be followed while it
-- plays instead of read at its end.
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
  , Evidence (..)
  , boundEvidence
  , evidenceLimit
  , StageRecord (..)
  , StepRecord (..)
  , CaseOutcome (..)
  , CaseSummary (..)
  , TrySummary (..)
  , CaseTrace (..)
  , LabelCount (..)
  , MoveCount (..)
  , EvidenceSeen (..)
  , EvidenceCount (..)
  , RunTrace (..)
  , ReplayOutcome (..)
  , ReplayRecord (..)
  , kindText
    -- * As it happens
  , Phase (..)
  , CaseClose (..)
  , TraceEvent (..)
    -- * Recording
  , Recorder
  , newRecorder
  , newRecorderFor
  , newRecorderTo
  , recordedProperty
  , readRunTrace
  , closeCut
  , beginCase
  , beginCaseWith
  , beginReplay
  , takeSteps
  , noteReplay
  , stepsFailure
  , pushStep
  , amendStep
  , sealStep
  , afterStep
  , withEvidence
  , markInconclusive
  , caseMoves
  , reached
    -- * Rendering helpers
  , clipText
  , bodyText
  ) where

import           Control.Applicative ((<|>))
import           Control.Exception (mask_)
import           Control.Monad (forM_, unless)
import           Data.Aeson (ToJSON (..), Value (..), encode, object, (.=))
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
import           Test.WebApi.Case (CaseItem, CaseRecord (..), caseFormat)
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

-- | What a step is. A /move/ is a call or a glue step, or a scenario's
-- move ("Test.WebApi.Moves"): its 'KindBegin' step, its own steps, and its
-- 'KindSettle' step, which settles, observes and judges it. The rest are
-- the engine's own (a fresh value, a context switch, a step that only
-- moves the model).
data StepKind = KindCall | KindGlue | KindFresh | KindContext | KindModel | KindOther | KindBegin | KindSettle
  deriving (Eq, Show)

kindText :: StepKind -> Text
kindText = \case
  KindCall -> "call"
  KindGlue -> "glue"
  KindFresh -> "fresh"
  KindContext -> "context"
  KindModel -> "model"
  KindOther -> "other"
  KindBegin -> "begin"
  KindSettle -> "settle"

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
  , subject :: Maybe Text
    -- ^ the thing a scenario's check was about (@orders #2@)
  }
  deriving (Eq, Show)

-- | Something a host noted about a move (zenbridge #2715): a fact that
-- explains what happened and changes no outcome. The engine names no
-- host, so what the kinds are is the host's own.
data Evidence = Evidence
  { kind :: Text
    -- ^ the host's own word for what sort of evidence this is. The engine
    -- counts by it ('EvidenceCount') and never reads it
  , what :: Text
    -- ^ one line a person reads
  , detail :: Maybe Value
    -- ^ the host's own data, carried as it is: whatever a reader of the
    -- trace needs to follow the evidence up (an id, a link)
  }
  deriving (Eq, Show)

-- | How much evidence a record keeps: the items of one move, the
-- characters of an item's line, and the bytes of an item's detail as JSON.
evidenceLimit :: (Int, Int, Int)
evidenceLimit = (100, 4000, 32768)

-- | A move's evidence as its record keeps it: each line clipped, a detail
-- over the limit replaced by a note of its size (@{"dropped": bytes}@),
-- and the items past the limit counted in one last item of the engine's
-- own kind, @evidence not kept@.
boundEvidence :: [Evidence] -> [Evidence]
boundEvidence items = map bound kept ++ [ Evidence {kind = "evidence not kept", what = tshow (length rest) <> " more items (a move keeps " <> tshow maxItems <> ")", detail = Nothing} | not (null rest) ]
  where
    (maxItems, maxLine, maxDetail) = evidenceLimit
    (kept, rest) = splitAt maxItems items
    bound Evidence {kind, what, detail} = Evidence
      { kind
      , what = clipText maxLine what
      , detail = detail >>= \d ->
          let size = LBS.length (encode d)
          in Just (if size > fromIntegral maxDetail then object ["dropped" .= size] else d)
      }

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
  , inMove :: Maybe Int
    -- ^ the scenario's move the step belongs to (its begin and settle
    -- steps included), numbered from 1 in its case
  , inconclusive :: Maybe Text
    -- ^ a move's settle step that did not settle, or could not observe, or
    -- that its host held inconclusive once every check had held: why.
    -- Neither passed nor failed. One marked before its checks ran has no
    -- check of its own; one marked after them has them all, held
  , evidence :: [Evidence]
    -- ^ what the host attached to the move (a settle step's; none on any
    -- other step), as 'boundEvidence' keeps it
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

-- | 'Cut': the run was stopped while the case was open (its time or
-- allocation limit, or its thread cancelled), so the case ends at the last
-- step it pushed — the move it reached — neither passed nor failed.
data CaseOutcome = Passed | Failed | Discarded | Inconclusive | Cut
  deriving (Eq, Show)

outcomeText :: CaseOutcome -> Text
outcomeText = \case
  Passed -> "passed"
  Failed -> "failed"
  Discarded -> "discarded"
  Inconclusive -> "inconclusive"
  Cut -> "cut"

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
  , replayable :: Maybe CaseRecord
    -- ^ the case written out to be replayed ("Test.WebApi.Case"), when the
    -- property was played through a recording runner
  }
  deriving (Eq, Show)

-- | How a regression case fared when a run played it first.
data ReplayOutcome
  = ReplayPassed
  | ReplayFailed
  | CantReplay
    -- ^ the program no longer plays it (a move removed, a pick of another
    -- type, …); 'why' says what
  | ReplayInconclusive
    -- ^ a move did not settle within its budget; 'why' says why
  | ReplayCut
    -- ^ the run was cut while it played ('closeCut'): its moves are the
    -- ones it had made, 'replayAt' its last step
  deriving (Eq, Show)

data ReplayRecord = ReplayRecord
  { caseNumber :: Int
    -- ^ its place in the run's cases, from 1
  , replayOutcome :: ReplayOutcome
  , replayAt :: Maybe Int
    -- ^ the move it failed at
  , replayWhy :: Maybe Text
  , replaySteps :: [StepRecord]
    -- ^ every move it played, in full
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

-- | One piece of evidence where it was seen: the generated case, the move
-- (as the case's summary lists it), and the item.
data EvidenceSeen = EvidenceSeen
  { caseNumber :: Int
  , move :: Text
  , what :: Text
  , detail :: Maybe Value
  }
  deriving (Eq, Show)

-- | The evidence of one kind over the generated cases that counted: in how
-- many cases and on how many moves it was attached, and the first three
-- in full. A passing case keeps no step, so this is where its evidence is
-- read.
data EvidenceCount = EvidenceCount
  { kind :: Text
  , cases :: Int
  , moves :: Int
  , first :: [EvidenceSeen]
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
  , evidence :: [EvidenceCount]
    -- ^ what the hosts attached to the moves of the generated cases that
    -- counted, by kind (an inconclusive or discarded case counts nothing,
    -- as for the labels and the moves)
  , replays :: [ReplayRecord]
    -- ^ the regression cases the run played first, before it drew any
  , cut :: Maybe CaseTrace
    -- ^ the case open when the run was cut ('closeCut'), its steps up to
    -- the move it reached
  }
  deriving (Eq, Show)

-- | Which play of its property a step belongs to: a regression case
-- replayed (its number among the property's cases), a generated case (the
-- number its line of the trace has; a case QuickCheck discards has the
-- number of the case drawn in its place), or a narrowing try.
data Phase = Replayed Int | Generated Int | Narrowing Int
  deriving (Eq, Show)

-- | How a play ended, as its line of the trace says it: the step it failed
-- at (a cut one: the step it reached), why, the moves it made and the
-- labels that held. A regression case the program no longer plays reads
-- 'Discarded' here, with why (@cant_replay@ among the trace's replays).
data CaseClose = CaseClose
  { outcome :: CaseOutcome
  , at :: Maybe Int
  , why :: Maybe Text
  , moves :: [Text]
  , labels :: [Text]
  }
  deriving (Eq, Show)

-- | What a recorder hands its sink as it happens (zenbridge #2741).
--
-- * 'StepEnded': a step, once, when it is final: as soon as its checks
--   have run ('sealStep'), so with its checks, its labels, the model and
--   the stage after it, or its inconclusive mark. A step whose checks
--   never ran (it failed, or its case was cut) is handed over as its case
--   closes. The steps of a case come in order.
-- * 'CaseEnded': a play closed, after its last step.
data TraceEvent
  = StepEnded Phase StepRecord
  | CaseEnded Phase CaseClose
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
  toJSON CheckRecord {..} = object ["check" .= check, "held" .= held, "why" .= why, "subject" .= subject]

instance ToJSON Evidence where
  toJSON Evidence {..} = object ["kind" .= kind, "what" .= what, "detail" .= detail]

instance ToJSON EvidenceSeen where
  toJSON EvidenceSeen {..} = object ["case" .= caseNumber, "move" .= move, "what" .= what, "detail" .= detail]

instance ToJSON EvidenceCount where
  toJSON EvidenceCount {..} = object ["kind" .= kind, "cases" .= cases, "moves" .= moves, "first" .= first]

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
    , "move" .= inMove
    , "outcome" .= (case inconclusive of
                      Just _ -> "inconclusive"
                      Nothing -> if ok then "ok" else "failed" :: Text)
    , "inconclusive" .= inconclusive
    , "evidence" .= evidence
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
  toJSON CaseTrace {..} = object ["from" .= from, "moves" .= steps, "case" .= replayable]

instance ToJSON ReplayOutcome where
  toJSON = String . \case
    ReplayPassed -> "passed"
    ReplayFailed -> "failed"
    CantReplay -> "cant_replay"
    ReplayInconclusive -> "inconclusive"
    ReplayCut -> "cut"

instance ToJSON ReplayRecord where
  toJSON ReplayRecord {..} = object
    [ "case" .= caseNumber, "outcome" .= replayOutcome, "at" .= replayAt, "why" .= replayWhy, "moves" .= replaySteps ]

instance ToJSON LabelCount where
  toJSON LabelCount {..} = object ["label" .= label, "cases" .= cases, "of" .= outOf]

instance ToJSON MoveCount where
  toJSON MoveCount {..} = object ["move" .= move, "played" .= timesPlayed]

instance ToJSON CaseOutcome where
  toJSON = String . outcomeText

-- | @{"replay": 1}@, @{"case": 2}@ or @{"try": 3}@.
instance ToJSON Phase where
  toJSON = \case
    Replayed k -> object ["replay" .= k]
    Generated k -> object ["case" .= k]
    Narrowing k -> object ["try" .= k]

instance ToJSON CaseClose where
  toJSON CaseClose {..} = object
    [ "outcome" .= outcome, "at" .= at, "why" .= why, "moves" .= moves, "labels" .= labels ]

-- | A step: @{"event": "step", "phase": …, "step": …}@; a case's close:
-- @{"event": "case", "phase": …, "outcome": …, "at": …, "why": …, "moves": …, "labels": …}@.
instance ToJSON TraceEvent where
  toJSON = \case
    StepEnded ph st -> object ["event" .= ("step" :: Text), "phase" .= ph, "step" .= st]
    CaseEnded ph CaseClose {..} -> object
      [ "event" .= ("case" :: Text), "phase" .= ph
      , "outcome" .= outcome, "at" .= at, "why" .= why, "moves" .= moves, "labels" .= labels ]

instance ToJSON RunTrace where
  toJSON RunTrace {..} = object
    [ "cases" .= cases
    , "discarded" .= discarded
    , "narrowing" .= narrowing
    , "first_failure" .= firstFailure
    , "narrowed" .= narrowed
    , "labels" .= labels
    , "moves" .= moves
    , "evidence" .= evidence
    , "replays" .= replays
    , "cut" .= cut
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
  , currentCase :: IORef (Maybe (Int, [CaseItem]))
    -- ^ the case being played, as a regression case would keep it (its
    -- size and items), when the runner records it
  , gathered :: IORef Gathered
  , declared :: [Text]
  , origin :: (Text, Text)
    -- ^ the program's version and the property's name, for the cases it
    -- writes out
  , sink :: Maybe (TraceEvent -> IO ())
    -- ^ where what is recorded goes as it happens ('newRecorderTo')
  , handed :: IORef Int
    -- ^ how many of the open case's steps the sink has been handed
  , replaying :: IORef (Maybe Int)
    -- ^ the regression case being replayed ('beginReplay'), its number
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
  , evidenceCounts :: M.Map Text EvidenceCount
  , replayLines :: Seq ReplayRecord
  , cutCase :: Maybe CaseTrace
  }

-- | A recorder for one property, given the labels its steps declare (a
-- declared label no case reaches reads 0, not absent).
newRecorder :: [Text] -> IO Recorder
newRecorder = newRecorderFor "" ""

-- | A recorder whose cases can be written out to be replayed: the
-- program's version (the runner's hash of it) and the property's name.
newRecorderFor :: Text -> Text -> [Text] -> IO Recorder
newRecorderFor = newRecorderWith Nothing

-- | 'newRecorderFor', what it records handed to the sink as it happens
-- (zenbridge #2741): see 'TraceEvent'.
--
-- The sink is called on the thread that plays the case (a cut case's last
-- events on the thread that closes it, 'closeCut'), with asynchronous
-- exceptions masked, so an event is handed over whole or not at all: it
-- must neither block nor throw, and do no work to speak of (put the event
-- on a queue; a thread of the host's own takes it from there). A case's
-- allocation budget ("Test.WebApi.Budget") counts what it allocates.
newRecorderTo :: (TraceEvent -> IO ()) -> Text -> Text -> [Text] -> IO Recorder
newRecorderTo out = newRecorderWith (Just out)

newRecorderWith :: Maybe (TraceEvent -> IO ()) -> Text -> Text -> [Text] -> IO Recorder
newRecorderWith out scenarioHash propertyName declaredLabels = do
  cur <- newIORef Seq.empty
  curCase <- newIORef Nothing
  handedRef <- newIORef 0
  replayingRef <- newIORef Nothing
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
    , evidenceCounts = M.empty
    , replayLines = Seq.empty
    , cutCase = Nothing
    }
  pure Recorder
    { current = cur, currentCase = curCase, gathered = g, declared = declaredLabels, origin = (scenarioHash, propertyName)
    , sink = out, handed = handedRef, replaying = replayingRef }

-- | The play the recorder is in: the regression case being replayed, else
-- the narrowing try or the generated case its next close will number.
phaseNow :: Recorder -> IO Phase
phaseNow Recorder {gathered, replaying} = readIORef replaying >>= \case
  Just k -> pure (Replayed k)
  Nothing -> do
    Gathered {narrowingStarted, caseCount, tryCount} <- readIORef gathered
    pure (if narrowingStarted then Narrowing (tryCount + 1) else Generated (caseCount + 1))

-- | The open case's steps the sink has not had yet, handed over in order,
-- each once (an asynchronous exception cannot fall between a step's
-- hand-over and its being marked handed over).
handOver :: Recorder -> IO ()
handOver rec@Recorder {sink, current, handed} = forM_ sink $ \out -> mask_ $ do
  s <- readIORef current
  k <- readIORef handed
  unless (Seq.length s <= k) $ do
    ph <- phaseNow rec
    writeIORef handed (Seq.length s)
    mapM_ (out . StepEnded ph) (drop k (toList s))

-- | A case closed: its line handed to the sink.
handClose :: Recorder -> Phase -> CaseClose -> IO ()
handClose Recorder {sink} ph c = forM_ sink $ \out -> mask_ (out (CaseEnded ph c))

-- | The step last pushed is final (its checks have run, or it was marked
-- inconclusive): the sink is handed it now, not when the next step is
-- pushed. Nothing may amend it afterwards.
sealStep :: Recorder -> IO ()
sealStep = handOver

-- | A case starts: whatever an interrupted one left is dropped.
beginCase :: Recorder -> IO ()
beginCase Recorder {current, handed} = writeIORef current Seq.empty >> writeIORef handed 0

-- | A regression case's replay starts: the steps played from here on are
-- its ('Replayed'), until 'noteReplay' says how it fared.
beginReplay :: Recorder -> Int -> IO ()
beginReplay Recorder {replaying} k = writeIORef replaying (Just k)

-- | A case starts, and this is how a regression case would keep it.
beginCaseWith :: Recorder -> Maybe (Int, [CaseItem]) -> IO ()
beginCaseWith rec@Recorder {currentCase} c = beginCase rec >> writeIORef currentCase c

-- | The steps played so far, taken (a replay's, which the runner closes
-- itself).
takeSteps :: Recorder -> IO [StepRecord]
takeSteps rec@Recorder {current, currentCase, handed} = do
  handOver rec
  writeIORef currentCase Nothing
  writeIORef handed 0
  toList <$> atomicModifyIORef' current (\s -> (Seq.empty, s))

-- | A regression case was played: how it fared.
noteReplay :: Recorder -> ReplayRecord -> IO ()
noteReplay rec@Recorder {gathered, replaying} r@ReplayRecord {caseNumber, replayOutcome, replayAt, replayWhy, replaySteps} = do
  modifyIORef' gathered $ \g@Gathered {replayLines} -> g {replayLines = replayLines |> r}
  writeIORef replaying Nothing
  handClose rec (Replayed caseNumber) CaseClose
    { outcome = case replayOutcome of
        ReplayPassed -> Passed
        ReplayFailed -> Failed
        CantReplay -> Discarded
        ReplayInconclusive -> Inconclusive
        ReplayCut -> Cut
    , at = replayAt, why = replayWhy
    , moves = map moveName (caseMoves replaySteps), labels = [] }

-- | A step ran: append it, numbered, and with the scenario's move it
-- belongs to: a begin step opens the case's next move, and every step
-- after it up to its settle step is in it.
pushStep :: Recorder -> StepRecord -> IO ()
pushStep rec@Recorder {current} StepRecord {..} = do
  -- a step before it that was never sealed is final now
  handOver rec
  modifyIORef' current $ \s ->
    let
      marks = [ (k, m) | StepRecord {kind = k, inMove = m} <- toList s, k == KindBegin || k == KindSettle ]
      opened = length [ () | (KindBegin, _) <- marks ]
      open = case reverse marks of
        (KindBegin, m) : _ -> m
        _ -> Nothing
      inMove' = case kind of
        KindBegin -> Just (opened + 1)
        _ -> open
    in s |> StepRecord {n = Seq.length s + 1, inMove = inMove', ..}

-- | Change the step last pushed (its postcondition ran). The last change
-- to a step is followed by 'sealStep'.
amendStep :: Recorder -> (StepRecord -> StepRecord) -> IO ()
amendStep Recorder {current} f = modifyIORef' current $ \s -> case Seq.viewr s of
  Seq.EmptyR -> s
  rest Seq.:> lastStep -> rest |> f lastStep

-- | A move's settle step that did not settle (or could not observe): why.
markInconclusive :: Text -> StepRecord -> StepRecord
markInconclusive why StepRecord {..} = StepRecord {inconclusive = Just why, ok = False, ..}

-- | The moves a case played, as its summary lists them: a scenario's move
-- once (by its settle step), and every call or glue step outside one.
caseMoves :: [StepRecord] -> [StepRecord]
caseMoves stepList = [ st | st@StepRecord {kind, inMove} <- stepList, kind == KindSettle || (isMove kind && inMove == Nothing) ]

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

-- | What a move's host attached to it, onto its settle step.
withEvidence :: [Evidence] -> StepRecord -> StepRecord
withEvidence ev StepRecord {..} = StepRecord {evidence = ev, ..}

-- | A counted case's evidence added to the run's counts: each kind once
-- for the case, once for every move that carries it, and its instances
-- while fewer than three are kept.
tallyEvidence :: Int -> [StepRecord] -> M.Map Text EvidenceCount -> M.Map Text EvidenceCount
tallyEvidence k stepList counts
  | all (\StepRecord {evidence} -> null evidence) stepList = counts
  | otherwise = M.unionWith merge counts (M.fromListWith (flip merge) (concatMap ofStep stepList))
  where
    -- a move's kinds, each with its instances on that move
    ofStep st@StepRecord {evidence} =
      [ (kd, EvidenceCount {kind = kd, cases = 1, moves = 1, first = [ EvidenceSeen {caseNumber = k, move = moveName st, what, detail} | Evidence {kind = kd', what, detail} <- evidence, kd' == kd ]})
      | kd <- Set.toList (Set.fromList [ kd | Evidence {kind = kd} <- evidence ]) ]
    -- within the case the kind counts its case once; across cases they add
    merge EvidenceCount {kind, cases = c1, moves = m1, first = f1} EvidenceCount {cases = c2, moves = m2, first = f2} =
      EvidenceCount {kind, cases = if sameCase f1 f2 then c1 else c1 + c2, moves = m1 + m2, first = take 3 (f1 ++ f2)}
    sameCase f1 f2 = case (f1, f2) of
      (EvidenceSeen {caseNumber = a} : _, EvidenceSeen {caseNumber = b} : _) -> a == b
      _ -> False

-- | The property, its cases closed into the recorder as QuickCheck
-- finishes each one.
recordedProperty :: Recorder -> Property -> Property
recordedProperty rec =
  P.callback (P.PostFinalFailure P.NotCounterexample (\_ _ -> finalFailure rec))
    . P.callback (P.PostTest P.NotCounterexample (\_ res -> closeCase rec res))

closeCase :: Recorder -> P.Result -> IO ()
closeCase rec@Recorder {current, currentCase, gathered, handed, origin = (scenarioHash, propertyName)} res = do
  -- a step whose checks never ran (it failed) is final now
  handOver rec
  phase <- phaseNow rec
  stepList <- toList <$> atomicModifyIORef' current (\s -> (Seq.empty, s))
  writeIORef handed 0
  caseKept <- atomicModifyIORef' currentCase (\c -> (Nothing, c))
  let
    unsettled = find (\StepRecord {inconclusive} -> inconclusive /= Nothing) stepList
    outcome = case (unsettled, P.ok res) of
      (Just _, _) -> Inconclusive
      (_, Just True) -> Passed
      (_, Just False) -> Failed
      (_, Nothing) -> Discarded
    failing = find (\StepRecord {ok} -> not ok) stepList
    at = (\StepRecord {n} -> n) <$> failing
    why
      | outcome == Failed = (failing >>= stepWhy) <|> nonEmpty (T.pack (P.reason res))
      | outcome == Inconclusive = unsettled >>= \StepRecord {inconclusive} -> inconclusive
      | otherwise = Nothing
    played = not (null stepList)
    moveList = map moveName (caseMoves stepList)
    countKeys = map moveCountKey (caseMoves stepList)
    held = Set.toList (Set.fromList (map T.pack (P.classes res)))
    replayable = (\(sz, its) -> CaseRecord {format = caseFormat, scenario = scenarioHash, property = propertyName, size = sz, items = its}) <$> caseKept
  modifyIORef' gathered $ \g@Gathered {..} ->
    if not narrowingStarted
      then case outcome of
        Discarded -> g {discardedCount = discardedCount + 1}
        -- QuickCheck discards it and draws another; the trace lists it, and
        -- counts no label or move of it
        Inconclusive ->
          let k = caseCount + 1
          in g { caseCount = k
               , caseLines = caseLines |> CaseSummary {number = k, played, moves = moveList, outcome, at, why, labels = []} }
        _ ->
          let k = caseCount + 1
              line' = CaseSummary {number = k, played, moves = moveList, outcome, at, why, labels = held}
          in g { caseCount = k
               , caseLines = caseLines |> line'
               , countedCases = countedCases + 1
               , labelCounts = M.unionWith (+) labelCounts (M.fromList [ (l, 1) | l <- held ])
               , moveCounts = M.unionWith (+) moveCounts (M.fromListWith (+) [ (m, 1) | m <- countKeys ])
               , evidenceCounts = tallyEvidence k stepList evidenceCounts
               , firstFailing = if outcome == Failed then Just CaseTrace {from = "case " <> tshow k, steps = stepList, replayable} else firstFailing
               , narrowingStarted = outcome == Failed
               }
      else
        let k = tryCount + 1
            taken = outcome == Failed
            line' = TrySummary {number = k, played, moves = moveList, outcome, at, why, taken}
        in g { tryCount = k
             , tryLines = tryLines |> line'
             , lastTaken = if taken then Just CaseTrace {from = "try " <> tshow k, steps = stepList, replayable} else lastTaken
             }
  -- the labels its line of the trace counts: none for a case that did not count
  handClose rec phase CaseClose {outcome, at, why, moves = moveList, labels = if outcome == Passed || outcome == Failed then held else []}
  where
    stepWhy StepRecord {checks, answer} =
      case [ w | CheckRecord {held = False, why = Just w} <- checks ] of
        (w : _) -> Just w
        [] -> answer >>= \AnswerRecord {failure} -> failure
    nonEmpty t = if T.null (T.strip t) then Nothing else Just t

-- | Where a case's steps failed, and why: the first step that did not
-- hold, its first failed check's reason or its answer's failure.
stepsFailure :: [StepRecord] -> (Maybe Int, Maybe Text)
stepsFailure stepList = case find (\StepRecord {ok} -> not ok) stepList of
  Nothing -> (Nothing, Nothing)
  Just s@StepRecord {n} -> (Just n, whyOf s)
  where
    whyOf StepRecord {checks, answer} =
      case [ w | CheckRecord {held = False, why = Just w} <- checks ] of
        (w : _) -> Just w
        [] -> answer >>= \AnswerRecord {failure} -> failure

-- | The run was cut while a case was open — a time or allocation limit, or
-- its thread cancelled — so QuickCheck never closed it ('recordedProperty'
-- closes a case only when QuickCheck finishes it): the case is closed from
-- the steps already pushed, as 'Cut', at its last step, with why. Its
-- moves are the ones it played and, last, the one it was in (begun, never
-- settled). A regression case cut while it replayed is noted among the
-- replays, as 'ReplayCut'. A recorder with no open case is left as it is.
-- Call it once the run has stopped, before 'readRunTrace': the sink is
-- handed the steps the case had made, then its close.
closeCut :: Recorder -> Text -> IO ()
closeCut rec@Recorder {current, currentCase, gathered, handed, replaying, origin = (scenarioHash, propertyName)} why = do
  -- the steps it had made are final now
  handOver rec
  phase <- phaseNow rec
  stepList <- toList <$> atomicModifyIORef' current (\s -> (Seq.empty, s))
  writeIORef handed 0
  caseKept <- atomicModifyIORef' currentCase (\c -> (Nothing, c))
  replayed <- atomicModifyIORef' replaying (\r -> (Nothing, r))
  let
    (at, _) = reached stepList
    moveList = movesReached stepList
    replayable = (\(sz, its) -> CaseRecord {format = caseFormat, scenario = scenarioHash, property = propertyName, size = sz, items = its}) <$> caseKept
    traced from = Just CaseTrace {from, steps = stepList, replayable}
    closed = handClose rec phase CaseClose {outcome = Cut, at, why = Just why, moves = moveList, labels = []}
  case replayed of
    -- a regression case cut while it replayed: among the replays, as cut,
    -- whether or not it had made a step
    Just k -> do
      modifyIORef' gathered $ \g@Gathered {replayLines} -> g
        { replayLines = replayLines |> ReplayRecord {caseNumber = k, replayOutcome = ReplayCut, replayAt = at, replayWhy = Just why, replaySteps = stepList}
        , cutCase = Just CaseTrace {from = "replay " <> tshow k, steps = stepList, replayable = Nothing} }
      closed
    Nothing -> unless (null stepList) $ do
      modifyIORef' gathered $ \g@Gathered {..} ->
        if narrowingStarted
          then
            let k = tryCount + 1
            in g { tryCount = k
                 , tryLines = tryLines |> TrySummary {number = k, played = True, moves = moveList, outcome = Cut, at, why = Just why, taken = False}
                 , cutCase = traced ("try " <> tshow k) }
          else
            let k = caseCount + 1
            in g { caseCount = k
                 , caseLines = caseLines |> CaseSummary {number = k, played = True, moves = moveList, outcome = Cut, at, why = Just why, labels = []}
                 , cutCase = traced ("case " <> tshow k) }
      closed

-- | The moves a case stopped while it was open had made: the ones it
-- played and, last, the one it was in (begun, never settled).
movesReached :: [StepRecord] -> [Text]
movesReached stepList = map moveName (caseMoves stepList) <> openMove
  where
    settled = [ m | StepRecord {kind = KindSettle, inMove = m} <- stepList ]
    openMove = case reverse [ st | st@StepRecord {kind = KindBegin} <- stepList ] of
      st@StepRecord {inMove = m} : _ | m `notElem` settled -> [moveName st]
      _ -> []

-- | Where a case stopped while it was open got to: its last step, and the
-- move it was in (else the last it made).
reached :: [StepRecord] -> (Maybe Int, Maybe Text)
reached stepList =
  ( case reverse stepList of
      StepRecord {n} : _ -> Just n
      [] -> Nothing
  , case reverse (movesReached stepList) of
      m : _ -> Just m
      [] -> Nothing )

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
    , evidence = M.elems evidenceCounts
    , replays = toList replayLines
    , cut = cutCase
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
