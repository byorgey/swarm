{-# LANGUAGE OverloadedStrings #-}

module Swarm.Game.Scenario.Launch.View where

import Brick
import Brick.Focus
import Brick.Forms qualified as BF
import Brick.Widgets.Border
import Brick.Widgets.Center (centerLayer, hCenter)
import Brick.Widgets.Edit
import Brick.Widgets.Edit qualified as E
import Brick.Widgets.FileBrowser qualified as FB
import Control.Exception qualified as E
import Data.Maybe (listToMaybe)
import Data.Text qualified as T
import Swarm.Game.Scenario.Launch.Model
import Swarm.TUI.Attr
import Swarm.TUI.Model.Name

drawFileBrowser :: FB.FileBrowser Name -> Widget Name
drawFileBrowser b =
  centerLayer $ hLimit 50 $ ui <=> help
 where
  ui =
    vLimit 15 $
      borderWithLabel (txt "Choose a file") $
        FB.renderFileBrowser True b
  help =
    padTop (Pad 1) $
      vBox $
        [ case FB.fileBrowserException b of
            Nothing -> emptyWidget
            Just e ->
              hCenter $
                withDefAttr BF.invalidFormInputAttr $
                  txt $
                    T.pack $
                      E.displayException e
        ] <> map (hCenter . txt) [
          "Up/Down: select"
        , "/: search, Ctrl-C or Esc: cancel search"
        , "Enter: change directory or select file"
        , "Esc: quit"
        ]

drawLaunchConfigPanel :: LaunchOptions -> [Widget Name]
drawLaunchConfigPanel (LaunchOptions (FileBrowserControl fb isFbDisplayed) seedEditor ring _isDisplayedFor) =
 addFileBrowser [panelWidget]
 where
  addFileBrowser = if isFbDisplayed
    then (drawFileBrowser fb:)
    else id
  seedEditorHasFocus = case focusGetCurrent ring of
    Just (ScenarioConfigControl (ScenarioConfigPanelControl SeedSelector)) -> True
    _ -> False

  highlightIfFocused x =
    if focusGetCurrent ring == Just (ScenarioConfigControl (ScenarioConfigPanelControl x))
      then withAttr highlightAttr
      else id

  mkButton name label = highlightIfFocused name $ str label

  panelWidget =
    centerLayer $
      borderWithLabel (str "Configure scenario launch") $
        hLimit 50 $ padAll 1 $ 
          vBox
            [ padBottom (Pad 1) $ txtWrap "Leaving this field blank will use the default seed for the scenario."
            , padBottom (Pad 1) $ padLeft (Pad 2) $ hBox
                [ mkButton SeedSelector "Seed: "
                , hLimit 10 $
                    overrideAttr E.editFocusedAttr customEditFocusedAttr $
                      renderEditor (txt . mconcat) seedEditorHasFocus seedEditor
                ]
            , padBottom (Pad 1) $ txtWrap "Selecting a script to be run upon start enables eligibility for code size scoring."
            , padBottom (Pad 1) $ padLeft (Pad 2) $ hBox
                [ mkButton ScriptSelector "Script: "
                , str $
                    maybe "<none>" FB.fileInfoSanitizedFilename $
                      listToMaybe $
                        FB.fileBrowserSelection fb
                ]
            , hCenter $ mkButton StartGameButton ">> Launch with these settings <<"
            ]
