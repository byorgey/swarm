{-# LANGUAGE OverloadedStrings #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Validity checking for 'Objective' prerequisites
module Swarm.Game.Scenario.Objective.Validation where

import Control.Lens (view, (^.))
import Control.Monad (unless)
import Data.Foldable (for_, toList)
import Data.Graph (stronglyConnComp)
import Data.Maybe (mapMaybe)
import Data.Set qualified as Set
import Data.Text qualified as T
import Swarm.Game.Scenario.Objective
import Swarm.Game.Scenario.Objective.Graph
import Swarm.Util (failT, quote)

-- | Performs monadic validation before returning
-- the "pure" construction of a wrapper record.
-- This validation entails:
--
-- 1. Ensuring that all goal references utilized in prerequisites
--    actually exist
-- 2. Ensuring that the graph of dependencies is acyclic.
validateObjectives ::
  MonadFail m =>
  [Objective] ->
  m [Objective]
validateObjectives objectives = do
  for_ objectives $ \x -> case x ^. objectivePrerequisite of
    Just p ->
      unless (null remaining) $
        failT
          [ "Reference to undefined objective(s)"
          , T.intercalate ", " (map quote $ Set.toList remaining) <> "."
          , "Defined are:"
          , T.intercalate ", " (map quote $ Set.toList allIds)
          ]
     where
      refs = Set.fromList $ toList $ logic p
      remaining = Set.difference refs allIds
    Nothing -> return ()

  unless (isAcyclicGraph connectedComponents) $
    failT ["There are dependency cycles in the prerequisites."]

  return objectives
 where
  connectedComponents = stronglyConnComp $ makeGraphEdges objectives
  allIds = Set.fromList $ mapMaybe (view objectiveId) objectives
