module Wasp.Cli.Command.CreateNewProject.AI
  ( createNewProjectForHuman,
    createNewProjectForMachine,
  )
where

import Control.Arrow ()
import Control.Monad.Except (MonadError (throwError), MonadIO (liftIO))
import qualified Data.Text as T
import qualified Data.Text.IO as T.IO
import StrongPath (Abs, Dir, Path', fromAbsDir)
import StrongPath.Operations ()
import System.Directory (createDirectory, createDirectoryIfMissing, setCurrentDirectory)
import System.Environment (lookupEnv)
import System.FilePath (takeDirectory)
import System.IO (hFlush, stdout)
import qualified Wasp.AI.CodeAgent as CA
import qualified Wasp.AI.GenerateNewProject as GNP
import Wasp.AI.GenerateNewProject.Common (AuthProvider (..), NewProjectDetails (..))
import Wasp.AI.OpenAI (OpenAIApiKey)
import Wasp.Cli.Command (Command, CommandError (CommandError))
import Wasp.Cli.Command.CreateNewProject.ProjectDescription (NewProjectAppName (..), parseWaspProjectNameIntoAppName)
import Wasp.Cli.Command.CreateNewProject.StarterTemplates (readCoreWaspProjectFiles)
import Wasp.Cli.Common (WaspProjectDir)
import qualified Wasp.Cli.Interactive as Interactive

createNewProjectForHuman :: Path' Abs (Dir WaspProjectDir) -> NewProjectAppName -> Command ()
createNewProjectForHuman waspProjectDir appName = do
  openAIApiKey <- getOpenAIApiKey

  appDescription <- liftIO $ Interactive.askForRequiredInput "Describe your app in a couple of sentences:\n"

  liftIO $ do
    createDirectory $ fromAbsDir waspProjectDir
    setCurrentDirectory $ fromAbsDir waspProjectDir

  let codeAgentConfig =
        CA.CodeAgentConfig
          { CA._openAIApiKey = openAIApiKey,
            CA._writeFile = writeFileToDisk,
            CA._writeLog = forwardLogToStdout
          }

  liftIO $ generateNewProject codeAgentConfig appName appDescription
  where
    writeFileToDisk path content = do
      createDirectoryIfMissing True (takeDirectory path)
      T.IO.writeFile path content
      putStrLn $ "[info] Wrote file at " <> path
      hFlush stdout

    forwardLogToStdout msg = do
      putStrLn . T.unpack $ msg
      hFlush stdout

createNewProjectForMachine :: String -> String -> Command ()
createNewProjectForMachine projectName appDescription = do
  openAIApiKey <- getOpenAIApiKey

  appName <- case parseWaspProjectNameIntoAppName projectName of
    Right appName -> pure appName
    Left err -> throwError $ CommandError "Invalid project name" err

  let codeAgentConfig =
        CA.CodeAgentConfig
          { CA._openAIApiKey = openAIApiKey,
            CA._writeFile = writeFileToStdoutWithDelimiters,
            CA._writeLog = writeLogToStdoutWithDelimiters
          }

  liftIO $ generateNewProject codeAgentConfig appName appDescription
  where
    writeFileToStdoutWithDelimiters path content =
      writeToStdoutWithDelimiters "WRITE FILE" [T.pack path, content]

    writeLogToStdoutWithDelimiters msg =
      writeToStdoutWithDelimiters "LOG" [msg]

    writeToStdoutWithDelimiters delimiterTitle paragraphs = do
      T.IO.putStrLn . ("\n" <>) $ withDelimiter delimiterTitle $ T.intercalate "\n" paragraphs
      hFlush stdout

    withDelimiter title content =
      T.intercalate
        "\n"
        [ "==== WASP AI: " <> title <> " ====",
          content,
          "===/ WASP AI: " <> title <> " ===="
        ]

generateNewProject :: CA.CodeAgentConfig -> NewProjectAppName -> String -> IO ()
generateNewProject codeAgentConfig (NewProjectAppName appName) appDescription = do
  coreWaspProjectFiles <- readCoreWaspProjectFiles
  CA.runCodeAgent codeAgentConfig $
    GNP.generateNewProject (newProjectDetails appName appDescription) coreWaspProjectFiles

getOpenAIApiKey :: Command OpenAIApiKey
getOpenAIApiKey =
  liftIO (lookupEnv "OPENAI_API_KEY")
    >>= maybe throwMissingOpenAIApiKeyEnvVarError pure
  where
    throwMissingOpenAIApiKeyEnvVarError =
      throwError $
        CommandError
          "Missing OPENAI_API_KEY env var"
          "You can obtain this key from your OpenAI profile."

newProjectDetails :: String -> String -> NewProjectDetails
newProjectDetails webAppName webAppDescription =
  NewProjectDetails
    { _projectAppName = webAppName,
      _projectDescription = webAppDescription,
      _projectAuth = UsernameAndPassword
    }
