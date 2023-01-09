{-# LANGUAGE OverloadedStrings #-}

module Swarm.TUI.Model.Achievement.Description where

import Swarm.TUI.Model.Achievement.Definitions

describe :: CategorizedAchievement -> AchievementInfo
describe (GlobalAchievement CompletedSingleTutorial) =
  AchievementInfo
    "Welcome Freshmen"
    (Just $ Freeform "School is in session!")
    "Complete one of the tutorials."
    Easy
    False
describe (GlobalAchievement CompletedAllTutorials) =
  AchievementInfo
    "Autodidact"
    ( Just $
        FTQuotation $
          Quotation
            "Terry Pratchet"
            "I didn't go to university... But I have sympathy for those who did."
    )
    "Complete all of the tutorials."
    Moderate
    False
describe (GlobalAchievement LookedAtAboutScreen) =
  AchievementInfo
    "About time!"
    Nothing
    "View the About screen."
    Trivial
    True
describe (GameplayAchievement CraftedBitcoin) =
  -- Bitcoin is the deepest level of the recipes
  -- hierarchy.
  AchievementInfo
    "Master of Your Craft"
    Nothing
    "Make a Bitcoin"
    Moderate
    True
describe (GameplayAchievement RobotIntoWater) =
  AchievementInfo
    "Watery Grave"
    (Just $ Freeform "This little robot thinks he's a submarine.")
    "Destroy a robot by sending it into the water."
    Easy
    True
describe (GameplayAchievement AttemptSelfDestructBase) =
  AchievementInfo
    "Call of the Void"
    (Just $ Freeform "What does that big red button do?")
    "Attempt to self-destruct your base."
    Easy
    True
describe (GameplayAchievement DestroyedBase) =
  AchievementInfo
    "That Could Have Gone Better"
    (Just $ Freeform "Boom.")
    "Actually destroy your base."
    Moderate
    True
