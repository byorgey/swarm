{-# LANGUAGE TemplateHaskell #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
module Swarm.TUI.View.Robot.Type where

import Brick (Widget)
import Brick.Focus (FocusRing)
import Brick.Widgets.List qualified as BL
import Brick.Widgets.TabularList.Mixed
import Control.Lens hiding (Const, from, (<.>))
import Data.Sequence (Seq)
import GHC.Generics (Generic)
import Swarm.Game.Robot
import Swarm.Language.Syntax (Const)
import Swarm.Log
import Swarm.TUI.Model.Name

-- | It is desirable to store the a priori known width of widgets
-- based on their text length or other fixed properties, and
-- use this as the type of the Brick list.
--
-- We can't defer this width computation to the "draw" function of a cell,
-- because we need access to all cell widths independently
-- outside of the cell draw function.
data WithWidth a = WithWidth
  { wWidth :: Int
  , wWidget :: a
  }
  deriving (Functor)

type WidthWidget = WithWidth (Widget Name)

padWidth :: Int -> WithWidth a -> WithWidth a
padWidth extra (WithWidth w x) = WithWidth (w + extra) x

newtype Widths = Widths
  { robotRowWidths :: [ColWidth]
  }
  deriving (Generic)

type RobotWidgetRow = RobotRowPayload WidthWidget

-- | This type is parameterized such that the same
-- collection of fields can specify both
-- cell widgets and column headings
data RobotRow a = RobotRow
  { _fID :: a
  , _fName :: a
  , _fAge :: a
  , _fPos :: a
  , _fItems :: a
  , _fStatus :: a
  , _fActns :: a
  , _fCmds :: a
  , _fCycles :: a
  , _fActivity :: a
  , _fLog :: a
  }
  deriving (Functor)

data RobotRowPayload a = RobotRowPayload
  { _robot :: Robot
  , _rPayload :: RobotRow a
  }
  deriving (Functor)

makeLenses ''RobotRowPayload

data RobotDetailsPaneState = RobotDetailsPaneState
  { _logsList :: BL.GenericList Name Seq LogEntry
  , _cmdHistogramList :: BL.List Name (Const, Int)
  }

makeLenses ''RobotDetailsPaneState

data RobotListContent = RobotListContent
  { _robotsListWidget :: MixedTabularList Name RobotWidgetRow Widths
  , _robotsListRenderers :: MixedRenderers Name RobotWidgetRow Widths
  , _robotDetailsPaneState :: RobotDetailsPaneState
  }

makeLenses ''RobotListContent

data RobotDisplay = RobotDisplay
  { _robotDetailsFocus :: FocusRing Name
  , _robotListContent :: RobotListContent
  }

makeLenses ''RobotDisplay
