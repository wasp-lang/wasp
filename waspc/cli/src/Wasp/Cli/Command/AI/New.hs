{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE QuasiQuotes #-}

module Wasp.Cli.Command.AI.New
  ( new,
  )
where

import Control.Arrow ()
import Control.Monad.Except (MonadError (throwError), MonadIO (liftIO))
import qualified Data.Text as T
import StrongPath (fromAbsDir)
import StrongPath.Operations ()
import System.Directory (setCurrentDirectory)
import System.Environment (lookupEnv)
import Wasp.Cli.Command (Command, CommandError (CommandError))
import qualified Wasp.Cli.Command.AI.CodeAgent as CA
import qualified Wasp.Cli.Command.AI.GenerateNewProject as GNP
import qualified Wasp.Cli.Command.CreateNewProject as CNP

new :: Command ()
new = do
  openAIApiKey <-
    liftIO (lookupEnv "OPENAI_API_KEY")
      >>= maybe throwMissingOpenAIApiKeyEnvVarError pure

  (webAppName, webAppDescription) <- liftIO $ do
    putStrLn "App name (e.g. MyFirstApp):"
    appName <- getLine
    putStrLn "Describe your app in a couple of sentences:"
    desc <- getLine
    return (appName, desc)

  projectInfo <- CNP.parseProjectInfo webAppName

  absWaspProjectDir <- CNP.createEmptyWaspProjectDir projectInfo
  liftIO $ setCurrentDirectory $ fromAbsDir absWaspProjectDir

  let codeAgentConfig =
        CA.CodeAgentConfig
          { CA._openAIApiKey = openAIApiKey,
            -- TODO: Use more appropriate functions here.
            CA._writeFile = \fp c -> putStrLn $ "\nwriteFile:\n" <> show (fp, c) <> "\n",
            CA._writeLog = putStrLn . ("writeLog: " <>) . T.unpack
          }

  let newProjectDetails =
        GNP.NewProjectDetails
          { GNP._projectAppName = webAppName,
            GNP._projectDescription = webAppDescription,
            GNP._projectAuth = GNP.UsernameAndPassword
          }

  liftIO $
    CA.runCodeAgent codeAgentConfig $
      GNP.generateNewProject newProjectDetails

  return ()
  where
    throwMissingOpenAIApiKeyEnvVarError =
      throwError $
        CommandError
          "Missing OPENAI_API_KEY env var"
          "You can obtain this key from your OpenAI profile."
