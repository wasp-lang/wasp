module Wasp.Cli.Command.Db
  ( makeDbCommand,
  )
where

import qualified Wasp.AppSpec as AS
import Wasp.Cli.Command (Command, require)
import Wasp.Cli.Command.Compile (compileWithOptions, defaultCompileOptions, withSetupGoal)
import Wasp.Cli.Command.Require.DbConnectionEstablished (DbConnectionEstablished (DbConnectionEstablished))
import Wasp.Cli.Command.Require.InWaspProject (InWaspProject (InWaspProject))
import Wasp.Cli.Command.Require.WaspSpecAvailable (WaspSpecAvailable (WaspSpecAvailable))
import Wasp.Cli.ProjectLock (withProjectLock)
import Wasp.Generator.Setup (SetupGoal)

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
