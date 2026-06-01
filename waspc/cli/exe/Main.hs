module Main where

import Control.Concurrent (threadDelay)
import qualified Control.Concurrent.Async as Async
import qualified Control.Exception as E
import Control.Monad (void)
import Main.Utf8 (withUtf8)
import qualified System.Environment as Env
import System.Exit (exitFailure)
import Wasp.Cli.Command (runCommand)
import Wasp.Cli.Command.BashCompletion (bashCompletion, printBashCompletionInstruction)
import Wasp.Cli.Command.Build (build)
import Wasp.Cli.Command.BuildStart (buildStart)
import Wasp.Cli.Command.Clean (clean)
import Wasp.Cli.Command.Compile (compile)
import Wasp.Cli.Command.CreateNewProject (createNewProject)
import qualified Wasp.Cli.Command.CreateNewProject.AI as Command.CreateNewProject.AI
import Wasp.Cli.Command.Db (runCommandThatRequiresDbRunning)
import qualified Wasp.Cli.Command.Db.Migrate as Command.Db.Migrate
import qualified Wasp.Cli.Command.Db.Reset as Command.Db.Reset
import qualified Wasp.Cli.Command.Db.Seed as Command.Db.Seed
import qualified Wasp.Cli.Command.Db.Studio as Command.Db.Studio
import Wasp.Cli.Command.Deploy (deploy)
import Wasp.Cli.Command.Deps (deps)
import Wasp.Cli.Command.Dockerfile (printDockerfile)
import Wasp.Cli.Command.Info (info)
import Wasp.Cli.Command.Install (install)
import Wasp.Cli.Command.News (news)
import Wasp.Cli.Command.Start (start)
import qualified Wasp.Cli.Command.Start.Db as Command.Start.Db
import Wasp.Cli.Command.Studio (studio)
import qualified Wasp.Cli.Command.Telemetry as Telemetry
import Wasp.Cli.Command.Test (test)
import Wasp.Cli.Command.Uninstall (uninstall)
import Wasp.Cli.Command.WaspLS (runWaspLS)
import Wasp.Cli.Message (cliSendMessage)
import qualified Wasp.Cli.Parser as Parser
import qualified Wasp.Message as Message
import qualified Wasp.Node.Version as NodeVersion
import Wasp.Util (indent)
import Wasp.Util.InstallMethod (getInstallationCommand)
import Wasp.Version (waspVersion)

main :: IO ()
main = withUtf8 . (`E.catch` handleInternalErrors) $ do
  action <- Parser.parseAction

  telemetryThread <-
    Async.async . runCommand . Telemetry.considerSendingData $
      Parser.actionToCall action

  -- Before calling any command, check that the node requirement is met. Node is
  -- not needed for every command, but checking for every command was decided
  -- to be more robust than trying to only check for commands that require it.
  -- See https://github.com/wasp-lang/wasp/issues/1134#issuecomment-1554065668
  NodeVersion.checkUserNodeAndNpmMeetWaspRequirements >>= \case
    NodeVersion.VersionCheckFail errorMsg -> do
      cliSendMessage $ Message.Failure "Node/NPM requirement not met" errorMsg
      exitFailure
    NodeVersion.VersionCheckSuccess -> pure ()

  setDefaultCliEnvVars

  runAction action

  -- If sending of telemetry data is still not done 1 second since command finished, abort it.
  -- We also make sure here to catch all errors that might get thrown and silence them.
  void $ Async.race (threadDelaySeconds 1) (Async.waitCatch telemetryThread)
  where
    threadDelaySeconds =
      let microsecondsInASecond = 1000000
       in threadDelay . (* microsecondsInASecond)

    handleInternalErrors :: E.ErrorCall -> IO ()
    handleInternalErrors e = do
      putStrLn $ "\nInternal Wasp error (bug in the compiler):\n" ++ indent 2 (show e)
      exitFailure

runAction :: Parser.Action -> IO ()
runAction = \case
  Parser.ANew args -> runCommand $ createNewProject args
  Parser.ANewAi args
    | Command.CreateNewProject.AI.newAiToStdout args ->
        runCommand $
          Command.CreateNewProject.AI.createNewProjectNonInteractiveToStdout
            (Command.CreateNewProject.AI.newAiProjectName args)
            (Command.CreateNewProject.AI.newAiAppDescription args)
            (Command.CreateNewProject.AI.newAiConfigJson args)
    | otherwise ->
        runCommand $
          Command.CreateNewProject.AI.createNewProjectNonInteractiveOnDisk
            (Command.CreateNewProject.AI.newAiProjectName args)
            (Command.CreateNewProject.AI.newAiAppDescription args)
            (Command.CreateNewProject.AI.newAiConfigJson args)
  Parser.AStart -> runCommand start
  Parser.AStartDb args -> runCommand $ Command.Start.Db.start args
  Parser.ADbStart args -> runCommand $ Command.Start.Db.start args
  Parser.ADbReset args -> runCommandThatRequiresDbRunning $ Command.Db.Reset.reset args
  Parser.ADbMigrateDev args -> runCommandThatRequiresDbRunning $ Command.Db.Migrate.migrateDev args
  Parser.ADbSeed maybeName -> runCommandThatRequiresDbRunning $ Command.Db.Seed.seed maybeName
  Parser.ADbStudio -> runCommandThatRequiresDbRunning Command.Db.Studio.studio
  Parser.AClean -> runCommand clean
  Parser.AInstall -> runCommand install
  Parser.AUninstall -> runCommand uninstall
  Parser.ACompile -> runCommand compile
  Parser.ABuild -> runCommand build
  Parser.ABuildStart args -> runCommand $ buildStart args
  Parser.AVersion -> printVersion
  Parser.ATelemetry -> runCommand Telemetry.telemetry
  Parser.ADeps -> runCommand deps
  Parser.ADockerfile -> runCommand printDockerfile
  Parser.AInfo -> runCommand info
  Parser.ANews -> runCommand news
  Parser.AStudio -> runCommand studio
  Parser.APrintBashCompletionInstruction -> runCommand printBashCompletionInstruction
  Parser.ABashCompletionListCommands -> runCommand bashCompletion
  Parser.AWaspLS args -> runWaspLS args
  Parser.ADeploy args -> runCommand $ deploy args
  Parser.ATest args -> runCommand $ test args

-- | Sets env variables that are visible to the commands run by the CLI.
-- For example, we can use this to hide update messages by tools like Prisma.
-- The env variables are visible to our CLI and any child processes spawned by it.
-- The env variables won't be set in the terminal session after the CLI exits.
setDefaultCliEnvVars :: IO ()
setDefaultCliEnvVars = mapM_ (uncurry Env.setEnv) cliEnvVars
  where
    cliEnvVars :: [(String, String)]
    cliEnvVars =
      [ ("PRISMA_HIDE_UPDATE_MESSAGE", "true")
      ]

printVersion :: IO ()
printVersion =
  putStrLn $
    unlines
      [ show waspVersion,
        "",
        "If you wish to install/switch to the latest version of Wasp, do:",
        indent 2 $ getInstallationCommand Nothing,
        "",
        "If you want a specific x.y.z version of Wasp, do:",
        indent 2 $ getInstallationCommand $ Just "x.y.z",
        "",
        "Check https://github.com/wasp-lang/wasp/releases for the list of valid versions, including the latest one."
      ]
