module Wasp.Cli.Command.CreateNewProject.AI
  ( createNewProjectInteractiveOnDisk,
    createNewProjectNonInteractiveOnDisk,
    createNewProjectNonInteractiveToStdout,
  )
where

import Control.Arrow ()
import Control.Monad.Except (MonadError (throwError), MonadIO (liftIO))
import Data.Function ((&))
import Data.Functor ((<&>))
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import qualified Data.Text.IO as T.IO
import StrongPath (Abs, Dir, Path', basename, fromAbsDir, fromRelDir)
import StrongPath.Operations ()
import System.Directory (createDirectory, createDirectoryIfMissing, setCurrentDirectory)
import System.Environment (lookupEnv)
import System.FilePath (takeDirectory)
import qualified System.FilePath as FP
import System.IO (hFlush, stdout)
import qualified Wasp.AI.CodeAgent as CA
import qualified Wasp.AI.GenerateNewProject as GNP
import Wasp.AI.GenerateNewProject.Common
  ( NewProjectConfig,
    NewProjectDetails (..),
    emptyNewProjectConfig,
  )
import qualified Wasp.AI.GenerateNewProject.Common as GNP.C
import Wasp.AI.OpenAI (OpenAIApiKey)
import qualified Wasp.AI.OpenAI.ChatGPT as ChatGPT
import Wasp.Cli.Command (Command, CommandError (CommandError))
import Wasp.Cli.Command.CreateNewProject.ProjectDescription
  ( NewProjectAppName (..),
    obtainAvailableProjectDirPath,
    parseWaspProjectNameIntoAppName,
  )
import Wasp.Cli.Command.CreateNewProject.StarterTemplates (readWaspProjectSkeletonFiles)
import Wasp.Cli.Common (WaspProjectDir)
import qualified Wasp.Cli.Interactive as Interactive
import qualified Wasp.Util.Aeson as Utils.Aeson

createNewProjectInteractiveOnDisk :: Path' Abs (Dir WaspProjectDir) -> NewProjectAppName -> Command ()
createNewProjectInteractiveOnDisk waspProjectDir appName = do
  openAIApiKey <- getOpenAIApiKey
  appDescription <- liftIO $ Interactive.askForRequiredInput "Describe your app in a couple of sentences"
  (planningGptModel, codingGptModel) <-
    liftIO $
      Interactive.askToChoose'
        "Choose GPT model(s) you want to use:"
        $ NE.fromList
          [ Interactive.Option
              "gpt-4 (planning) + gpt-3.5-turbo (coding)"
              (Just "Ok results. Cheap and fast. Best cost/benefit ratio.")
              (ChatGPT.GPT_4, ChatGPT.GPT_3_5_turbo),
            Interactive.Option
              "gpt-4 (planning) + gpt-4-1106-preview (coding)"
              (Just "Possibly better results, but somewhat slower and somewhat more expensive (~2-3x).")
              (ChatGPT.GPT_4, ChatGPT.GPT_4_1106_Preview),
            Interactive.Option
              "gpt-4 (planning + coding)"
              (Just "Best results, but quite slower and quite more expensive (~5x).")
              (ChatGPT.GPT_4, ChatGPT.GPT_4)
          ]
  temperature <-
    liftIO $
      Interactive.askToChoose'
        "Choose the creativity level (temperature):"
        $ NE.fromList
          [ Interactive.Option
              "Balanced (0.7)"
              (Just "Optimal trade-off between creativity and possible mistakes.")
              0.7,
            Interactive.Option
              "Conventional (0.4)"
              (Just "Generates sensible code with minimal amount of mistakes.")
              0.4,
            Interactive.Option
              "Creative (1.0)"
              (Just "Generates more creative code, but mistakes are more likely.")
              1.0
          ]
  let projectConfig =
        emptyNewProjectConfig
          { GNP.C.projectPlanningGptModel = Just planningGptModel,
            GNP.C.projectCodingGptModel = Just codingGptModel,
            GNP.C.projectDefaultGptTemperature = Just temperature
          }
  liftIO $ createNewProjectOnDisk openAIApiKey waspProjectDir appName appDescription projectConfig

createNewProjectNonInteractiveOnDisk :: String -> String -> String -> Command ()
createNewProjectNonInteractiveOnDisk projectName appDescription projectConfigJson = do
  appName <- case parseWaspProjectNameIntoAppName projectName of
    Right appName -> pure appName
    Left err -> throwError $ CommandError "Invalid project name" err
  projectConfig <-
    Utils.Aeson.decodeFromString projectConfigJson
      & either (throwError . CommandError "Invalid project config" . ("Failed to parse JSON: " <>)) pure
  waspProjectDir <- obtainAvailableProjectDirPath projectName
  openAIApiKey <- getOpenAIApiKey
  liftIO $ createNewProjectOnDisk openAIApiKey waspProjectDir appName appDescription projectConfig

createNewProjectOnDisk ::
  OpenAIApiKey ->
  Path' Abs (Dir WaspProjectDir) ->
  NewProjectAppName ->
  String ->
  NewProjectConfig ->
  IO ()
createNewProjectOnDisk openAIApiKey waspProjectDir appName appDescription projectConfig = do
  createDirectory $ fromAbsDir waspProjectDir
  setCurrentDirectory $ fromAbsDir waspProjectDir
  generateNewProject codeAgentConfig appName appDescription projectConfig
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
      putStrLn $ "> Wrote to file: " <> fromRelDir (basename waspProjectDir) FP.</> path
      hFlush stdout

    forwardLogToStdout msg = do
      putStrLn . T.unpack $ msg
      hFlush stdout

-- | Instead of writing files to disk, it will write files (and logs) to the stdout,
-- with delimiters that make it easy to programmaticaly parse the output.
createNewProjectNonInteractiveToStdout :: String -> String -> String -> Command ()
createNewProjectNonInteractiveToStdout projectName appDescription projectConfigJsonStr = do
  openAIApiKey <- getOpenAIApiKey

  appName <- case parseWaspProjectNameIntoAppName projectName of
    Right appName -> pure appName
    Left err -> throwError $ CommandError "Invalid project name" err

  projectConfig <-
    Utils.Aeson.decodeFromString projectConfigJsonStr
      & either (throwError . CommandError "Invalid project config" . ("Failed to parse JSON: " <>)) pure

  let codeAgentConfig =
        CA.CodeAgentConfig
          { CA._openAIApiKey = openAIApiKey,
            CA._writeFile = writeFileToStdoutWithDelimiters,
            CA._writeLog = writeLogToStdoutWithDelimiters
          }

  liftIO $ generateNewProject codeAgentConfig appName appDescription projectConfig
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

generateNewProject :: CA.CodeAgentConfig -> NewProjectAppName -> String -> NewProjectConfig -> IO ()
generateNewProject codeAgentConfig (NewProjectAppName appName) appDescription projectConfig = do
  waspProjectSkeletonFiles <- readWaspProjectSkeletonFiles
  CA.runCodeAgent codeAgentConfig $ do
    GNP.generateNewProject (newProjectDetails projectConfig appName appDescription) waspProjectSkeletonFiles

getOpenAIApiKey :: Command OpenAIApiKey
getOpenAIApiKey =
  liftIO (lookupEnv "OPENAI_API_KEY" <&> (>>= validateKey))
    >>= maybe throwMissingOpenAIApiKeyEnvVarError pure
  where
    validateKey "" = Nothing
    validateKey k = Just k

    throwMissingOpenAIApiKeyEnvVarError =
      throwError $
        CommandError
          "Missing OPENAI_API_KEY environment variable"
          $ unlines
            [ "Wasp AI uses ChatGPT to generate your project, and therefore requires you to provide it with an OpenAI API key.",
              "You can obtain this key via your OpenAI account's user settings (https://platform.openai.com/account/api-keys).",
              "Then, set OPENAI_API_KEY env var to it and wasp CLI will read from it.",
              "",
              "To persist the OPENAI_API_KEY env var, add",
              "  export OPENAI_API_KEY=<yourkeyhere>",
              "to your .bash_profile (or .profile or .zprofile or whatever your machine is using), restart your shell, and you should be good to go."
            ]

newProjectDetails :: NewProjectConfig -> String -> String -> NewProjectDetails
newProjectDetails projectConfig webAppName webAppDescription =
  NewProjectDetails
    { _projectAppName = webAppName,
      _projectDescription = webAppDescription,
      _projectConfig = projectConfig
    }
