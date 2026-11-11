module Wasp.Cli.Command.Task
  ( listTasks,
    runTask,
  )
where

import qualified Control.Monad.Except as E
import Control.Monad.IO.Class (liftIO)
import Data.List (intercalate)
import StrongPath ((</>))
import qualified Wasp.AppSpec as AS
import qualified Wasp.AppSpec.Task as AS.Task
import Wasp.Cli.Command (Command, CommandError (CommandError), require)
import Wasp.Cli.Command.Compile (analyzeWithDiagnosticsOnStderr, defaultCompileOptions, withSetupGoals)
import Wasp.Cli.Command.Message (cliSendMessageC)
import Wasp.Cli.Command.Require.InWaspProject (InWaspProject (InWaspProject))
import Wasp.Cli.Command.Require.ValidNodeAndNpm (ValidNodeAndNpm (ValidNodeAndNpm))
import Wasp.Cli.Command.Require.WaspSpecAvailable (WaspSpecAvailable (WaspSpecAvailable))
import Wasp.Cli.Compile (waitEnsureCompile)
import Wasp.Cli.ProjectRunConfig (makeDefaultDevProjectRunConfig)
import qualified Wasp.Generator.ServerGenerator.RunConfig as ServerGenerator
import Wasp.Generator.ServerGenerator.TaskRunner (TaskRunner (AppTasks))
import qualified Wasp.Generator.ServerGenerator.TaskRunner.Jobs as TaskRunner.Jobs
import Wasp.Generator.Setup (SetupGoal (PrismaCliReady, SdkReady))
import qualified Wasp.Job as J
import qualified Wasp.Message as Msg
import Wasp.Project.Common (generatedAppDirInWaspProjectDir)
import Wasp.Util (checkExitSuccess)
import qualified Wasp.Util.Terminal as Term

-- | Prints the app's tasks and their descriptions.
listTasks :: Command ()
listTasks = do
  ValidNodeAndNpm <- require
  InWaspProject waspProjectDir <- require
  WaspSpecAvailable <- require
  appSpec <- analyzeWithDiagnosticsOnStderr waspProjectDir
  case AS.getTasks appSpec of
    [] -> cliSendMessageC $ Msg.Info noTasksDefinedMessage
    tasks -> liftIO $ putStr $ unlines $ map (showTask $ maximum $ map (length . AS.Task.name) tasks) tasks
  where
    showTask nameColumnWidth task =
      Term.applyStyles [Term.Bold] task.name
        <> maybe "" (\description -> replicate (nameColumnWidth - length task.name + 2) ' ' <> description) task.description

-- | Runs the app's task with the given name in development, passing it the
-- given arguments.
--
-- Tasks import the SDK.
runTask :: String -> [String] -> Command ()
runTask taskName taskArgs = do
  InWaspProject waspProjectDir <- require
  WaspSpecAvailable <- require
  appSpec <- waitEnsureCompile $ withSetupGoals [PrismaCliReady, SdkReady] $ defaultCompileOptions waspProjectDir
  throwIfNoTaskNamed taskName appSpec
  ValidNodeAndNpm <- require
  projectRunConfig <- liftIO $ makeDefaultDevProjectRunConfig appSpec
  let genProjectDir = waspProjectDir </> generatedAppDirInWaspProjectDir

  cliSendMessageC $ Msg.Start $ "Running task " <> taskName <> "..."

  let job = TaskRunner.Jobs.runTask AppTasks (ServerGenerator.makeEnvVars projectRunConfig) genProjectDir taskName taskArgs
  liftIO (checkExitSuccess "Running the task" <$> J.run job) >>= \case
    Left errorMsg -> E.throwError $ CommandError ("Task " <> taskName <> " failed") errorMsg
    Right () -> cliSendMessageC $ Msg.Success $ "Task " <> taskName <> " finished successfully!"

throwIfNoTaskNamed :: String -> AS.AppSpec -> Command ()
throwIfNoTaskNamed taskName appSpec =
  case AS.getTasks appSpec of
    [] -> E.throwError $ CommandError "No tasks defined" noTasksDefinedMessage
    tasks
      | taskName `elem` taskNames -> return ()
      | otherwise ->
          E.throwError
            $ CommandError "Invalid task name"
            $ "There is no task with the name "
              <> taskName
              <> ".\n"
              <> "Valid task names are: "
              <> intercalate ", " taskNames
              <> "."
      where
        taskNames = map AS.Task.name tasks

noTasksDefinedMessage :: String
noTasksDefinedMessage =
  "You haven't defined any tasks, so there is nothing to run!\n"
    <> "To do so, add tasks to your Wasp spec with the task() constructor."
