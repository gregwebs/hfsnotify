--
-- Copyright (c) 2012 Mark Dittmer - http://www.markdittmer.org
-- Developed for a Google Summer of Code project - http://gsoc2012.markdittmer.org
--

module System.IO.FSNotify.Types
       ( act
       , ActionPredicate
       , Action
       , DebounceConfig(..)
       , DebounceData(..)
       , DebouncePayload
       , Event(..)
       , EventChannel
       , eventPath
       , eventTime
       , IOEvent
       ) where

import Prelude hiding (FilePath)

import Control.Concurrent.Chan.Strict
import Data.IORef (IORef)
import Data.Time (NominalDiffTime)
import Data.Time.Clock (UTCTime)
import Filesystem.Path.CurrentOS
import Control.DeepSeq

-- | A file event reported by a file watcher. Each event contains the
-- canonical path for the file and a timestamp guaranteed to be after the
-- event occurred (timestamps represent current time when FSEvents receives
-- it from the OS and/or platform-specific Haskell modules).
data Event =
    Added    FilePath UTCTime
  | Modified FilePath UTCTime
  | Removed  FilePath UTCTime
  deriving (Eq, Show)

instance NFData Event where
   rnf a = a `seq` ()

-- | Helper for extracting the path associated with an event.
eventPath :: Event -> FilePath
eventPath (Added    path _) = path
eventPath (Modified path _) = path
eventPath (Removed  path _) = path

-- | Helper for extracting the time associated with an event.
eventTime :: Event -> UTCTime
eventTime (Added    _ timestamp) = timestamp
eventTime (Modified _ timestamp) = timestamp
eventTime (Removed  _ timestamp) = timestamp

type EventChannel = Chan Event

-- | Config object for debouncing events.
data DebounceConfig = DebounceDefault | Debounce NominalDiffTime | NoDebounce

type IOEvent = IORef Event

-- | DebouncePayload contents. Contains epsilon value for debouncing
-- near-simultaneous events and an IORef of the latest Event. Difference in
-- arrival time is measured according to Event value timestamps.
data DebounceData = DebounceData NominalDiffTime IOEvent

-- | Data "payload" passed to event handlers to enable debouncing. This value
-- is automatically derived from a DebounceConfig value. A value of Just
-- DebounceData results in debouncing according to the given epsilon and
-- IOEvent. A value of Nothing results in no debouncing.
type DebouncePayload = Maybe DebounceData

-- | A predicate used to determine whether to act on an event.
type ActionPredicate = Event -> Bool

-- | An action to be performed in response to an event.
type Action = Event -> IO ()

-- | Predicate to always act.
act :: ActionPredicate
act _ = True
