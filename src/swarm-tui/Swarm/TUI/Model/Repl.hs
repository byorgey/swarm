{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
module Swarm.TUI.Model.Repl (
  -- ** REPL
  REPLEntryType (..),
  REPLHistItemType (..),
  REPLHistItem (..),
  mkREPLSubmission,
  mkREPLSaved,
  mkREPLOutput,
  mkREPLError,
  isREPLEntry,
  getREPLSubmitted,
  isREPLSaved,
  getREPLEntry,
  REPLHistory,
  replIndex,
  replLength,
  replHasExecutedManualInput,
  replSeq,
  newREPLHistory,
  addREPLItem,
  restartREPLHistory,
  getLatestREPLHistoryItems,
  getSessionREPLHistoryItems,
  moveReplHistIndex,
  getCurrentItemText,
  replIndexIsAtInput,
  TimeDir (..),

  -- ** Prompt utils
  REPLPrompt (..),
  removeEntry,

  -- *** REPL Panel Model
  REPLState,
  ReplControlMode (..),
  replPromptType,
  replPromptEditor,
  replPromptText,
  replValid,
  replLast,
  replType,
  replControlMode,
  replHistory,
  newREPLEditor,

  -- ** Initialization
  initREPLState,
  defaultPrompt,
  lastEntry,
) where

import Brick.Widgets.Edit (Editor, applyEdit, editorText, getEditContents)
import Control.Applicative (Applicative (liftA2))
import Control.Lens hiding (from, (.=), (<.>))
import Data.Aeson (ToJSON, object, toJSON, (.=))
import Data.Foldable (toList)
import Data.Maybe (fromMaybe, isJust)
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Zipper qualified as TZ
import Servant.Docs (ToSample)
import Servant.Docs qualified as SD
import Swarm.Language.Syntax (SrcLoc (..))
import Swarm.Language.Types
import Swarm.TUI.Model.Name
import Swarm.Util (applyWhen)
import Swarm.Util.Lens (makeLensesNoSigs)
import Prelude hiding (Applicative (..))

------------------------------------------------------------
-- REPL History
------------------------------------------------------------

-- | Whether a user REPL entry was submitted or merely saved.
data REPLEntryType
  = -- | The entry was submitted (with Enter) and should thus be shown
    --   in the REPL scrollback.
    REPLEntrySubmitted
  | -- | The entry was merely saved (e.g. by hitting down
    --   arrow) and should thus be available in the history but not
    --   shown in the scrollback.
    REPLEntrySaved
  deriving (Eq, Ord, Show, Read)

-- | Various types of REPL history items (user input, output, error).
data REPLHistItemType
  = -- | Something entered by the user.
    REPLEntry REPLEntryType
  | -- | A response printed by the system.
    REPLOutput
  | -- | An error printed by the system.
    REPLError
  deriving (Eq, Ord, Show, Read)

-- | An item in the REPL history.
data REPLHistItem = REPLHistItem {replItemType :: REPLHistItemType, replItemText :: Text}
  deriving (Eq, Ord, Show, Read)

mkREPLSubmission :: Text -> REPLHistItem
mkREPLSubmission = REPLHistItem (REPLEntry REPLEntrySubmitted)

mkREPLSaved :: Text -> REPLHistItem
mkREPLSaved = REPLHistItem (REPLEntry REPLEntrySaved)

mkREPLOutput :: Text -> REPLHistItem
mkREPLOutput = REPLHistItem REPLOutput

mkREPLError :: Text -> REPLHistItem
mkREPLError = REPLHistItem REPLError

instance ToSample REPLHistItem where
  toSamples _ =
    SD.samples
      [ REPLHistItem (REPLEntry REPLEntrySubmitted) "grab"
      , REPLHistItem REPLOutput "it0 : text = \"tree\""
      , REPLHistItem (REPLEntry REPLEntrySaved) "place"
      , REPLHistItem (REPLEntry REPLEntrySubmitted) "place tree"
      , REPLHistItem REPLError "1:7: Unbound variable tree"
      ]

instance ToJSON REPLHistItem where
  toJSON (REPLHistItem itemType x) = object [label .= x]
   where
    label = case itemType of
      REPLEntry REPLEntrySubmitted -> "in"
      REPLEntry REPLEntrySaved -> "save"
      REPLOutput -> "out"
      REPLError -> "err"

-- | Useful helper function to only get user input text.  Gets all
--   user input, including both submitted and saved history items.
getREPLEntry :: REPLHistItem -> Maybe Text
getREPLEntry = \case
  REPLHistItem (REPLEntry {}) t -> Just t
  _ -> Nothing

-- | Useful helper function to filter out REPL output.  Returns True
--   for all user input, including both submitted and saved history
--   items.
isREPLEntry :: REPLHistItem -> Bool
isREPLEntry = isJust . getREPLEntry

-- | Helper function to get only submitted user input text.
getREPLSubmitted :: REPLHistItem -> Maybe Text
getREPLSubmitted = \case
  REPLHistItem (REPLEntry REPLEntrySubmitted) t -> Just t
  _ -> Nothing

-- | Useful helper function to filter out saved REPL entries (which
--   should not be shown in the scrollback).
isREPLSaved :: REPLHistItem -> Bool
isREPLSaved (REPLHistItem (REPLEntry REPLEntrySaved) _) = True
isREPLSaved _ = False

-- | History of the REPL with indices (0 is first entry) to the current
--   line and to the first entry since loading saved history.
--   We also (ab)use the length of the REPL as the index of current
--   input line, since that number is one past the index of last entry.
data REPLHistory = REPLHistory
  { _replSeq :: Seq REPLHistItem
  , _replIndex :: Int
  , _replStart :: Int
  , _replHasExecutedManualInput :: Bool
  }
  deriving (Show)

makeLensesNoSigs ''REPLHistory

-- | Sequence of REPL inputs and outputs, oldest entry is leftmost.
replSeq :: Lens' REPLHistory (Seq REPLHistItem)

-- | The current index in the REPL history (if the user is going back
--   through the history using up/down keys).
replIndex :: Lens' REPLHistory Int

-- | The index of the first entry since loading saved history.
--
-- It will be set on load and reset on save (happens during exit).
replStart :: Lens' REPLHistory Int

-- | Keep track of whether the user has explicitly executed commands
--   at the REPL prompt, thus making them ineligible for code size scoring.
--
--   Note: Instead of adding a dedicated field to the 'REPLHistory' record,
--   an early attempt entailed checking for:
--
--     @_replIndex > _replStart@
--
--   However, executing an initial script causes a "REPLOutput" to be
--   appended to the REPL history, which increments the replIndex, and
--   thus makes the Index greater than the Start even though the
--   player has not input commands directly into the REPL.
--
--   Therefore, a dedicated boolean is introduced into 'REPLHistory'
--   which simply latches True when the user has input a command.
--
--   An alternative is described in
--   <https://github.com/swarm-game/swarm/pull/974#discussion_r1112380380 issue #974>.
replHasExecutedManualInput :: Lens' REPLHistory Bool

-- | Create new REPL history (i.e. from loaded history file lines).
newREPLHistory :: [REPLHistItem] -> REPLHistory
newREPLHistory xs =
  let s = Seq.fromList xs
   in REPLHistory
        { _replSeq = s
        , _replStart = length s
        , _replIndex = length s
        , _replHasExecutedManualInput = False
        }

-- | Point the start of REPL history after current last line. See 'replStart'.
restartREPLHistory :: REPLHistory -> REPLHistory
restartREPLHistory h = h & replStart .~ replLength h

-- | Current number lines of the REPL history - (ab)used as index of input buffer.
replLength :: REPLHistory -> Int
replLength = length . _replSeq

-- | Add new REPL input - the index must have been pointing one past
--   the last element already, so we increment it to keep it that way.
addREPLItem :: REPLHistItem -> REPLHistory -> REPLHistory
addREPLItem t h =
  h
    & replSeq %~ (|> t)
    & replIndex .~ 1 + replLength h

-- | Get the latest N items in history, starting with the oldest one.
--
-- This is used to show previous REPL lines in UI, so we need the items
-- sorted in the order they were entered and will be drawn top to bottom.
getLatestREPLHistoryItems :: Int -> REPLHistory -> [REPLHistItem]
getLatestREPLHistoryItems n h = toList latestN
 where
  latestN = Seq.drop oldestIndex $ h ^. replSeq
  oldestIndex = max (h ^. replStart) $ length (h ^. replSeq) - n

-- | Get only the items from the REPL history that were entered during
--   the current session.
getSessionREPLHistoryItems :: REPLHistory -> Seq REPLHistItem
getSessionREPLHistoryItems h = Seq.drop (h ^. replStart) (h ^. replSeq)

data TimeDir = Newer | Older deriving (Eq, Ord, Show)

moveReplHistIndex :: TimeDir -> Text -> REPLHistory -> REPLHistory
moveReplHistIndex d lastEntered history = history & replIndex .~ newIndex
 where
  historyLen = replLength history
  curText = fromMaybe lastEntered $ getCurrentItemText history
  curIndex = history ^. replIndex
  entries = history ^. replSeq
  -- split repl at index
  (olderP, newer) = Seq.splitAt curIndex entries
  -- find first different entry in direction
  notSameEntry = \case
    REPLHistItem (REPLEntry {}) t -> t /= curText
    _ -> False
  newIndex = case d of
    Newer -> maybe historyLen (curIndex +) $ Seq.findIndexL notSameEntry newer
    Older -> fromMaybe curIndex $ Seq.findIndexR notSameEntry olderP

getCurrentItemText :: REPLHistory -> Maybe Text
getCurrentItemText history = replItemText <$> Seq.lookup (history ^. replIndex) (history ^. replSeq)

replIndexIsAtInput :: REPLHistory -> Bool
replIndexIsAtInput repl = repl ^. replIndex == replLength repl

-- | Given some text,  removes the 'REPLEntry' within 'REPLHistory' which is equal to that.
--   This is used when the user enters search mode and wants to traverse the history.
--   If a command has been used many times, the history will be populated with it causing
--   the effect that search command always finds the same command.
removeEntry :: Text -> REPLHistory -> REPLHistory
removeEntry foundtext hist = hist & replSeq %~ Seq.filter ((/= Just foundtext) . getREPLEntry)

-- | Get the last 'REPLEntry' in 'REPLHistory' matching the given text
lastEntry :: Text -> REPLHistory -> Maybe Text
lastEntry t h =
  case Seq.viewr $ Seq.filter matchEntry $ h ^. replSeq of
    Seq.EmptyR -> Nothing
    _ Seq.:> a -> Just (replItemText a)
 where
  matchesText histItem = t `T.isInfixOf` replItemText histItem
  matchEntry = liftA2 (&&) matchesText isREPLEntry

------------------------------------------------------------
-- REPL
------------------------------------------------------------

-- | This data type tells us how to interpret the text typed
--   by the player at the prompt (which is stored in Editor).
data REPLPrompt
  = -- | Interpret the prompt text as a regular command.
    --   The list is for potential completions, which we can
    --   cycle through by hitting Tab repeatedly
    CmdPrompt [Text]
  | -- | Interpret the prompt text as "search this text in history"
    SearchPrompt REPLHistory

defaultPrompt :: REPLPrompt
defaultPrompt = CmdPrompt []

-- | What is being done with user input to the REPL panel?
data ReplControlMode
  = -- | The user is typing at the REPL.
    Typing
  | -- | The user is driving the base using piloting mode.
    Piloting
  | -- | A custom user key handler is processing user input.
    Handling
  deriving (Eq, Bounded, Enum)

data REPLState = REPLState
  { _replPromptType :: REPLPrompt
  , _replPromptEditor :: Editor Text Name
  , _replValid :: Either SrcLoc ()
  , _replLast :: Text
  , _replType :: Maybe Polytype
  , _replControlMode :: ReplControlMode
  , _replHistory :: REPLHistory
  }

newREPLEditor :: Text -> Editor Text Name
newREPLEditor t = applyEdit gotoEnd $ editorText REPLInput (Just 5) t
 where
  ls = T.lines t
  pos = (length ls - 1, T.length (last ls))
  gotoEnd = applyWhen (not $ null ls) $ TZ.moveCursor pos

initREPLState :: REPLHistory -> REPLState
initREPLState hist =
  REPLState
    { _replPromptType = defaultPrompt
    , _replPromptEditor = newREPLEditor ""
    , _replValid = Right ()
    , _replLast = ""
    , _replType = Nothing
    , _replControlMode = Typing
    , _replHistory = hist
    }

makeLensesNoSigs ''REPLState

-- | The way we interpret text typed by the player in the REPL prompt.
replPromptType :: Lens' REPLState REPLPrompt

-- | The prompt where the user can type input at the REPL.
replPromptEditor :: Lens' REPLState (Editor Text Name)

-- | Convenience lens to get text from editor and replace it with new
--   one that has the provided text.
replPromptText :: Lens' REPLState Text
replPromptText = lens g s
 where
  g r = r ^. replPromptEditor . to getEditContents . to T.unlines
  s r t = r & replPromptEditor .~ newREPLEditor t

-- | Whether the prompt text is a valid 'Swarm.Language.Syntax.Term'.
--   If it is invalid, the location of error. ('NoLoc' means the whole
--   text causes the error.)
replValid :: Lens' REPLState (Either SrcLoc ())

-- | The type of the current REPL input which should be displayed to
--   the user (if any).
replType :: Lens' REPLState (Maybe Polytype)

-- | The last thing the user has typed which isn't part of the history.
--   This is used to restore the repl form after the user visited the history.
replLast :: Lens' REPLState Text

-- | The current REPL control mode, i.e. how user input to the REPL
--   panel is being handled.
replControlMode :: Lens' REPLState ReplControlMode

-- | History of things the user has typed at the REPL, interleaved
--   with outputs the system has generated.
replHistory :: Lens' REPLState REPLHistory
