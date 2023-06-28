module Wasp.Cli.Command.CreateNewProject.AI
  ( createNewProjectInteractiveOnDisk,
    createNewProjectNonInteractiveOnDisk,
    createNewProjectNonInteractiveToStdout,
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
import Wasp.Cli.Command.CreateNewProject.ProjectDescription (NewProjectAppName (..), obtainAvailableProjectDirPath, parseWaspProjectNameIntoAppName)
import Wasp.Cli.Command.CreateNewProject.StarterTemplates (readWaspProjectSkeletonFiles)
import Wasp.Cli.Common (WaspProjectDir)
import qualified Wasp.Cli.Interactive as Interactive

createNewProjectInteractiveOnDisk :: Path' Abs (Dir WaspProjectDir) -> NewProjectAppName -> Command ()
createNewProjectInteractiveOnDisk waspProjectDir appName = do
  openAIApiKey <- getOpenAIApiKey
  appDescription <- liftIO $ Interactive.askForRequiredInput "Describe your app in a couple of sentences:\n"
  liftIO $ createNewProjectOnDisk openAIApiKey waspProjectDir appName appDescription

createNewProjectNonInteractiveOnDisk :: String -> String -> Command ()
createNewProjectNonInteractiveOnDisk projectName appDescription = do
  appName <- case parseWaspProjectNameIntoAppName projectName of
    Right appName -> pure appName
    Left err -> throwError $ CommandError "Invalid project name" err
  waspProjectDir <- obtainAvailableProjectDirPath projectName
  openAIApiKey <- getOpenAIApiKey
  liftIO $ createNewProjectOnDisk openAIApiKey waspProjectDir appName appDescription

createNewProjectOnDisk :: OpenAIApiKey -> Path' Abs (Dir WaspProjectDir) -> NewProjectAppName -> String -> IO ()
createNewProjectOnDisk openAIApiKey waspProjectDir appName appDescription = do
  createDirectory $ fromAbsDir waspProjectDir
  setCurrentDirectory $ fromAbsDir waspProjectDir
  generateNewProject codeAgentConfig appName appDescription
  where
    codeAgentConfig =
      CA.CodeAgentConfig
        { CA._openAIApiKey = openAIApiKey,
          CA._writeFile = writeFileToDisk,
          CA._writeLog = forwardLogToStdout
        }

    writeFileToDisk path content = do
      createDirectoryIfMissing True (takeDirectory path)
      T.IO.writeFile path content
      putStrLn $ "> Wrote file at " <> path
      hFlush stdout

    forwardLogToStdout msg = do
      putStrLn . T.unpack $ msg
      hFlush stdout

-- | Instead of writing files to disk, it will write files (and logs) to the stdout,
-- with delimiters that make it easy to programmaticaly parse the output.
createNewProjectNonInteractiveToStdout :: String -> String -> Command ()
createNewProjectNonInteractiveToStdout projectName appDescription = do
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
  waspProjectSkeletonFiles <- readWaspProjectSkeletonFiles
  CA.runCodeAgent codeAgentConfig $
    GNP.generateNewProject (newProjectDetails appName appDescription) waspProjectSkeletonFiles

getOpenAIApiKey :: Command OpenAIApiKey
getOpenAIApiKey =
  liftIO (lookupEnv "OPENAI_API_KEY")
    >>= maybe throwMissingOpenAIApiKeyEnvVarError pure
  where
    throwMissingOpenAIApiKeyEnvVarError =
      throwError $
        CommandError
          "Missing OPENAI_API_KEY environment variable"
          $ unlines
            [ "Wasp AI uses ChatGPT to generate your project, and therefore requires you to provide it with an OpenAI API key.",
              "You can obtain this key via your OpenAI account's user settings (https://platform.openai.com/account/api-keys).",
              "Then, add",
              "  export OPENAI_API_KEY=<yourkeyhere>",
              "to .bash_profile or .profile, restart your shell, and you should be good to go."
            ]

newProjectDetails :: String -> String -> NewProjectDetails
newProjectDetails webAppName webAppDescription =
  NewProjectDetails
    { _projectAppName = webAppName,
      _projectDescription = webAppDescription,
      _projectAuth = UsernameAndPassword
    }
