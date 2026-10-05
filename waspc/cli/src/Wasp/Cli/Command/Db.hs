module Wasp.Cli.Command.Db
  ( runCommandThatRequiresDbRunning,
    makeDbCommand,
  )
where

import qualified Wasp.AppSpec as AS
import Wasp.Cli.Command (Command, require, runCommand)
import Wasp.Cli.Command.Call (Arguments)
import Wasp.Cli.Command.Compile (compileWithOptions, defaultCompileOptions, withSetupGoal)
import Wasp.Cli.Command.Require.DbConnectionEstablished (DbConnectionEstablished (DbConnectionEstablished))
import Wasp.Cli.Command.Require.InWaspProject (InWaspProject (InWaspProject))
import Wasp.Cli.Command.Require.WaspSpecAvailable (WaspSpecAvailable (WaspSpecAvailable))
import Wasp.Cli.ProjectLock (withProjectLock)
import Wasp.Cli.Util.Parser (ArgsParser, withArguments)
import Wasp.Generator.Setup (SetupGoal)

runCommandThatRequiresDbRunning :: ArgsParser a -> (a -> Command ()) -> Arguments -> IO ()
runCommandThatRequiresDbRunning parser command args =
  runCommand $ withArguments parser command args

-- | Prepares what a db command needs before it runs: a compile,
--   post-compile setup, and a reachable database.
--
--   All the commands that operate on the db should be created using this function.
makeDbCommand :: SetupGoal -> (AS.AppSpec -> Command a) -> Command a
makeDbCommand setupGoal cmd = withProjectLock $ do
  InWaspProject waspProjectDir <- require
  WaspSpecAvailable <- require
  (_, appSpec) <- compileWithOptions $ withSetupGoal setupGoal $ defaultCompileOptions waspProjectDir
  DbConnectionEstablished <- require
  cmd appSpec
