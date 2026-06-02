module Main where

import qualified Control.Exception as E
import Main.Utf8 (withUtf8)
import qualified System.Environment as Env
import System.Exit (exitFailure)
import Wasp.Cli.Message (cliSendMessage)
import qualified Wasp.Cli.Parser as Parser
import qualified Wasp.Message as Message
import qualified Wasp.Node.Version as NodeVersion
import Wasp.Util (indent)

main :: IO ()
main = withUtf8 . (`E.catch` handleInternalErrors) $ do
  -- Parsing yields the IO action to run for the chosen command (it also handles
  -- --help, parse errors, and exiting on its own). Each command reports its own
  -- telemetry, so all Main does is the universal setup and then runs it.
  runSelectedCommand <- Parser.parse

  -- Before calling any command, check that the node requirement is met. Node is
  -- not needed for every command, but checking for every command was decided
  -- to be more robust than trying to only check for commands that require it.
  -- See https://github.com/wasp-lang/wasp/issues/1134#issuecomment-1554065668
  ensureNodeAndNpmMeetWaspRequirements

  setDefaultCliEnvVars

  runSelectedCommand
  where
    handleInternalErrors :: E.ErrorCall -> IO ()
    handleInternalErrors e = do
      putStrLn $ "\nInternal Wasp error (bug in the compiler):\n" ++ indent 2 (show e)
      exitFailure

ensureNodeAndNpmMeetWaspRequirements :: IO ()
ensureNodeAndNpmMeetWaspRequirements =
  NodeVersion.checkUserNodeAndNpmMeetWaspRequirements >>= \case
    NodeVersion.VersionCheckFail errorMsg -> do
      cliSendMessage $ Message.Failure "Node/NPM requirement not met" errorMsg
      exitFailure
    NodeVersion.VersionCheckSuccess -> pure ()

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
