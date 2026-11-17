-- | A case's allocation budget, and a move's (zenbridge #2730): GHC's
-- allocation limit on the thread a case plays on, armed when the case
-- starts ('withCaseBudget', around the runner that plays it) and set again
-- when each of its moves begins ('moveBegins', from the move's first
-- step): a move gets its own budget or what the case has left, whichever
-- is less. A case or a move over its budget gets 'AllocationLimitExceeded'
-- on its thread, an asynchronous exception nothing on the way catches
-- (QuickCheck re-throws asynchronous exceptions), and the runner closes
-- the case as cut ("Test.WebApi.Trace".'closeCut').
--
-- It counts what the case's own thread allocates: a host that runs work of
-- its own on that thread (an application called in process) and does not
-- want it counted runs it on a thread of its own.
module Test.WebApi.Budget
  ( AllocBudget (..)
  , noBudget
  , withCaseBudget
  , moveBegins
  ) where

import           Control.Concurrent (ThreadId, myThreadId)
import           Control.Exception (finally)
import           Control.Monad (forM_)
import           Data.Int (Int64)
import           Data.IORef
import qualified Data.Map.Strict as M
import           Data.Maybe (isNothing)
import           System.IO.Unsafe (unsafePerformIO)
import           System.Mem (disableAllocationLimit, enableAllocationLimit, getAllocationCounter, setAllocationCounter)

-- | Bytes a case may allocate, and a move; 'Nothing' for no limit.
data AllocBudget = AllocBudget
  { perCase :: Maybe Int64
  , perMove :: Maybe Int64
  }
  deriving (Eq, Show)

noBudget :: AllocBudget
noBudget = AllocBudget {perCase = Nothing, perMove = Nothing}

-- | What a thread playing a case has left: the case's bytes when its
-- current move began, and what the counter was set to then.
data Armed = Armed
  { budget :: AllocBudget
  , caseLeft :: Int64
  , setTo :: Int64
  }

armedThreads :: IORef (M.Map ThreadId Armed)
armedThreads = unsafePerformIO (newIORef M.empty)
{-# NOINLINE armedThreads #-}

-- | Play a case under the budget, on this thread: the limit armed before,
-- lifted after (however the case ends). No budget: the case as it is.
withCaseBudget :: AllocBudget -> IO a -> IO a
withCaseBudget b@AllocBudget {perCase, perMove} act
  | isNothing perCase && isNothing perMove = act
  | otherwise = do
      tid <- myThreadId
      let left = maybe maxBound id perCase
      setAllocationCounter left
      atomicModifyIORef' armedThreads (\m -> (M.insert tid Armed {budget = b, caseLeft = left, setTo = left} m, ()))
      enableAllocationLimit
      act `finally` do
        disableAllocationLimit
        atomicModifyIORef' armedThreads (\m -> (M.delete tid m, ()))

-- | A move begins on this thread: what the case spent since the last move
-- began is taken off what it has left, and the counter is set to the
-- move's budget or the case's remainder, whichever is less. A thread
-- playing no budgeted case: nothing.
moveBegins :: IO ()
moveBegins = do
  tid <- myThreadId
  armed <- M.lookup tid <$> readIORef armedThreads
  forM_ armed $ \Armed {budget = b@AllocBudget {perMove}, caseLeft, setTo} -> do
    now <- getAllocationCounter
    let left = caseLeft - (setTo - now)
        next = maybe left (min left) perMove
    setAllocationCounter next
    atomicModifyIORef' armedThreads (\m -> (M.insert tid Armed {budget = b, caseLeft = left, setTo = next} m, ()))
