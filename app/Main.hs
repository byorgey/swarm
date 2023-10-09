{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
module Main where

import Data.Foldable qualified
import Data.Maybe (fromMaybe)
import Data.Text (Text, pack)
import Data.Text qualified as T
import Data.Text.IO qualified as Text
import GitHash (GitInfo, giBranch, giHash, tGitInfoCwdTry)
import Options.Applicative
import Prettyprinter
import Prettyprinter.Render.Text qualified as RT
import Swarm.App (appMain)
import Swarm.Doc.Gen (EditorType (..), GenerateDocs (..), PageAddress (..), SheetType (..), generateDocs)
import Swarm.Game.World.Render (printScenarioMap, renderScenarioMap)
import Swarm.Language.LSP (lspMain)
import Swarm.Language.Parse (readTerm)
import Swarm.Language.Pretty (ppr)
import Swarm.TUI.Model (AppOpts (..), ColorMode (..))
import Swarm.TUI.Model.UI (defaultInitLgTicksPerSecond)
import Swarm.Util ((?))
import Swarm.Version
import Swarm.Web (defaultPort)
import System.Console.Terminal.Size qualified as Term
import System.Exit (exitFailure)
import System.IO (hPrint, stderr)
import Text.Read (readMaybe)

gitInfo :: Maybe GitInfo
gitInfo = either (const Nothing) Just ($$tGitInfoCwdTry)

commitInfo :: String
commitInfo = case gitInfo of
  Nothing -> ""
  Just git -> " (" <> giBranch git <> "@" <> take 10 (giHash git) <> ")"

type Width = Int

data CLI
  = Run AppOpts
  | Format Input (Maybe Width)
  | DocGen GenerateDocs
  | RenderMap FilePath
  | LSP
  | Version

cliParser :: Parser CLI
cliParser =
  subparser
    ( mconcat
        [ command "format" (info (Format <$> format <*> optional widthOpt <**> helper) (progDesc "Format a file"))
        , command "generate" (info (DocGen <$> docgen <**> helper) (progDesc "Generate docs"))
        , command "map" (info (RenderMap <$> strArgument (metavar "FILE")) (progDesc "Render a scenario world map."))
        , command "lsp" (info (pure LSP) (progDesc "Start the LSP"))
        , command "version" (info (pure Version) (progDesc "Get current and upstream version."))
        ]
    )
    <|> Run
      <$> ( AppOpts
              <$> seed
              <*> scenario
              <*> run
              <*> autoplay
              <*> speedFactor
              <*> cheat
              <*> color
              <*> webPort
              <*> pure gitInfo
          )
 where
  format :: Parser Input
  format =
    (Stdin <$ switch (long "stdin" <> help "Read code from stdin"))
      <|> (File <$> strArgument (metavar "FILE"))
  widthOpt :: Parser Width
  widthOpt = option auto (long "width" <> metavar "COLUMNS" <> help "Use layout with maximum width")
  docgen :: Parser GenerateDocs
  docgen =
    subparser . mconcat $
      [ command "recipes" (info (pure RecipeGraph) $ progDesc "Output graphviz dotfile of entity dependencies based on recipes")
      , command "editors" (info (EditorKeywords <$> editor <**> helper) $ progDesc "Output editor keywords")
      , command "keys" (info (pure SpecialKeyNames) $ progDesc "Output list of recognized special key names")
      , command "cheatsheet" (info (CheatSheet <$> address <*> cheatsheet <**> helper) $ progDesc "Output nice Wiki tables")
      , command "pedagogy" (info (pure TutorialCoverage) $ progDesc "Output tutorial coverage")
      , command "endpoints" (info (pure WebAPIEndpoints) $ progDesc "Generate markdown Web API documentation.")
      ]

  editor :: Parser (Maybe EditorType)
  editor =
    Data.Foldable.asum
      [ pure Nothing
      , Just VSCode <$ switch (long "code" <> help "Generate for the VS Code editor")
      , Just Emacs <$ switch (long "emacs" <> help "Generate for the Emacs editor")
      ]
  address :: Parser PageAddress
  address =
    let replace a b = T.unpack . T.replace a b . T.pack
        opt n =
          fmap (fromMaybe "") . optional $
            option
              str
              ( long n
                  <> metavar "ADDRESS"
                  <> help ("Set the address of " <> replace "-" " " n <> ". Default no link.")
              )
     in PageAddress <$> opt "entities-page" <*> opt "commands-page" <*> opt "capabilities-page" <*> opt "recipes-page"
  cheatsheet :: Parser (Maybe SheetType)
  cheatsheet =
    Data.Foldable.asum
      [ pure Nothing
      , Just Entities <$ switch (long "entities" <> help "Generate entities page (uses data from entities.yaml)")
      , Just Recipes <$ switch (long "recipes" <> help "Generate recipes page (uses data from recipes.yaml)")
      , Just Capabilities <$ switch (long "capabilities" <> help "Generate capabilities page (uses entity map)")
      , Just Commands <$ switch (long "commands" <> help "Generate commands page (uses constInfo, constCaps and inferConst)")
      , Just Scenario <$ switch (long "scenario" <> help "Generate scenario schema page")
      ]
  seed :: Parser (Maybe Int)
  seed = optional $ option auto (long "seed" <> short 's' <> metavar "INT" <> help "Seed to use for world generation")
  webPort :: Parser (Maybe Int)
  webPort =
    optional $
      option
        auto
        ( long "web"
            <> metavar "PORT"
            <> help ("Set the web service port (or disable it with 0). Default to " <> show defaultPort <> ".")
        )
  scenario :: Parser (Maybe String)
  scenario = optional $ strOption (long "scenario" <> short 'i' <> metavar "FILE" <> help "Name of an input scenario to load")
  run :: Parser (Maybe String)
  run = optional $ strOption (long "run" <> short 'r' <> metavar "FILE" <> help "Run the commands in a file at startup")
  autoplay :: Parser Bool
  autoplay = switch (long "autoplay" <> short 'a' <> help "Automatically run the solution defined in the scenario, if there is one. Mutually exclusive with --run.")
  speedFactor :: Parser Int
  speedFactor = option auto (long "speed" <> short 'm' <> value defaultInitLgTicksPerSecond <> help "Initial game speed multiplier")
  cheat :: Parser Bool
  cheat = switch (long "cheat" <> short 'x' <> help "Enable cheat mode. This allows toggling Creative Mode with Ctrl+v and unlocks \"Testing\" scenarios in the menu.")
  color :: Parser (Maybe ColorMode)
  color = optional $ option colorModeParser (long "color" <> short 'c' <> metavar "MODE" <> help "Use none/8/16/full color mode.")
  colorModeParser =
    Data.Foldable.asum
      [ ColorMode8 <$ text "8"
      , ColorMode16 <$ text "16"
      , ColorMode240 <$> maybeReader (\case ('2' : '4' : '0' : '_' : w) -> readMaybe w; _ -> Nothing)
      , FullColor <$ text "full"
      , NoColor <$ text "none"
      ]
  text t = maybeReader (\x -> if x == t then Just x else Nothing)

cliInfo :: ParserInfo CLI
cliInfo =
  info
    (cliParser <**> helper)
    ( header ("Swarm game - " <> version <> commitInfo)
        <> progDesc "To play the game simply run without any command."
        <> fullDesc
    )

data Input = Stdin | File FilePath

getInput :: Input -> IO Text
getInput Stdin = Text.getContents
getInput (File fp) = Text.readFile fp

showInput :: Input -> Text
showInput Stdin = "(input)"
showInput (File fp) = pack fp

-- | Utility function to validate and format swarm-lang code
formatFile :: Input -> Maybe Width -> IO ()
formatFile input mWidth = do
  content <- getInput input
  case readTerm content of
    Right Nothing -> Text.putStrLn ""
    Right (Just ast) -> do
      mWindow <- Term.size
      let mkOpt w = LayoutOptions (AvailablePerLine w 1.0)
      let opt =
            fmap mkOpt mWidth
              ? fmap (\(Term.Window _h w) -> mkOpt w) mWindow
              ? defaultLayoutOptions
      Text.putStrLn . RT.renderStrict . layoutPretty opt $ ppr ast
    Left e -> do
      Text.hPutStrLn stderr $ showInput input <> ":" <> e
      exitFailure

showVersion :: IO ()
showVersion = do
  putStrLn $ "Swarm game - " <> version <> commitInfo
  up <- getNewerReleaseVersion gitInfo
  either (hPrint stderr) (putStrLn . ("New upstream release: " <>)) up

main :: IO ()
main = do
  cli <- execParser cliInfo
  case cli of
    Run opts -> appMain opts
    DocGen g -> generateDocs g
    Format fo w -> formatFile fo w
    RenderMap mapPath -> printScenarioMap =<< renderScenarioMap mapPath
    LSP -> lspMain
    Version -> showVersion
