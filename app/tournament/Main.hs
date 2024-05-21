{-# LANGUAGE OverloadedStrings #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
module Main where

import Control.Monad.Trans.Reader (runReaderT)
import Data.Maybe (fromMaybe)
import Data.Yaml (decodeFileThrow)
import Database.SQLite.Simple (execute_, withConnection)
import Network.Wai.Handler.Warp (Port)
import Options.Applicative
import Swarm.Game.State (Sha1 (..))
import Swarm.Web.Tournament
import Swarm.Web.Tournament.Database.Query
import Swarm.Web.Tournament.Type (UserAlias (..))

data AppOpts = AppOpts
  { userWebPort :: Maybe Port
  -- ^ Explicit port on which to run the web API
  , gameGitVersion :: Sha1
  , deploymentEnv :: DeploymentEnvironment
  }

webPort :: Parser (Maybe Int)
webPort =
  optional $
    option
      auto
      ( long "port"
          <> metavar "PORT"
          <> help ("Set the web service port (or disable it with 0). Default to " <> show defaultPort <> ".")
      )

gameVersion :: Parser Sha1
gameVersion =
  Sha1
    <$> option
      str
      ( long "version"
          <> metavar "VERSION"
          <> help "Set the git version of the game"
      )

parseRunningLocally :: Parser DeploymentEnvironment
parseRunningLocally =
  flag
    ProdDeployment
    (LocalDevelopment $ UserAlias "local-user")
    (long "local" <> help "Running locally for development")

cliParser :: Parser AppOpts
cliParser = AppOpts <$> webPort <*> gameVersion <*> parseRunningLocally

cliInfo :: ParserInfo AppOpts
cliInfo =
  info
    (cliParser <**> helper)
    ( header "Swarm tournament"
        <> progDesc "Hosts a tournament server."
        <> fullDesc
    )

main :: IO ()
main = do
  opts <- execParser cliInfo

  creds <- case deploymentEnv opts of
    LocalDevelopment _ -> return $ GitHubCredentials "" ""
    ProdDeployment -> decodeFileThrow "swarm-github-app-credentials.yaml"

  webMain
    (AppData (gameGitVersion opts) creds persistenceFunctions (deploymentEnv opts))
    (fromMaybe defaultPort $ userWebPort opts)
 where
  persistenceFunctions =
    PersistenceLayer
      { scenarioStorage =
          ScenarioPersistence
            { lookupCache = withConn lookupScenarioSolution
            , storeCache = withConn insertScenario
            , getContent = withConn lookupScenarioContent
            }
      , solutionStorage =
          ScenarioPersistence
            { lookupCache = withConn lookupSolutionSubmission
            , storeCache = withConn insertSolutionSubmission
            , getContent = withConn lookupSolutionContent
            }
      , authenticationStorage =
          AuthenticationStorage
            { usernameFromCookie = withConn getUsernameFromCookie
            , cookieFromUsername = withConn insertCookie
            }
      }
   where
    withConn f x =
      withConnection databaseFilename $ \conn -> do
        execute_ conn "PRAGMA foreign_keys = ON;"
        runReaderT (f x) conn
