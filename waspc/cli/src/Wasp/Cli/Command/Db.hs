module Wasp.Cli.Command.Db
  ( runCommandThatRequiresDbRunning,
  )
where

import Control.Monad (void)
import Wasp.Cli.Command (Command, require, runCommand)
import Wasp.Cli.Command.Compile (compileWithOptions, defaultCompileOptions)
import Wasp.Cli.Command.Message (cliSendMessageC)
import Wasp.Cli.Command.Require.DbConnectionEstablished (DbConnectionEstablished (DbConnectionEstablished))
import Wasp.Cli.Command.Require.InWaspProject (InWaspProject (InWaspProject))
import Wasp.Cli.Command.Require.WaspSpecAvailable (WaspSpecAvailable (WaspSpecAvailable))
import Wasp.Cli.ProjectLock (ProjectAccess (..), withProjectLockOrAlongsideWatcher)
import Wasp.CompileOptions (CompileOptions (generatorWarningsFilter))
import Wasp.Generator.Monad (GeneratorWarning (GeneratorNeedsMigrationWarning))
import qualified Wasp.Message as Msg

runCommandThatRequiresDbRunning :: Command a -> IO ()
runCommandThatRequiresDbRunning = runCommand . makeDbCommand

-- | This function makes sure that all the prerequisites which db commands
--   need are set up (e.g. makes sure Prisma CLI is installed).
--
--   All the commands that operate on db should be created using this function.
makeDbCommand :: Command a -> Command a
makeDbCommand cmd = withProjectLockOrAlongsideWatcher $ \projectAccess -> do
  -- Ensure code is generated and npm dependencies are installed.
  InWaspProject waspProjectDir <- require
  WaspSpecAvailable <- require
  case projectAccess of
    ExclusiveProjectAccess -> void $ compileWithOptions $ compileOptions waspProjectDir
    ProjectAccessAlongsideWatcher watcherProcessId ->
      cliSendMessageC $
        Msg.Info $
          "Skipping compilation, another Wasp command (PID "
            ++ show watcherProcessId
            ++ ") is already keeping this project compiled."
  DbConnectionEstablished <- require
  cmd
  where
    compileOptions waspProjectDir =
      (defaultCompileOptions waspProjectDir)
        { -- Ignore "DB needs migration warnings" during database commands, as that is redundant
          -- for `db migrate-dev` and not helpful for `db studio`.
          generatorWarningsFilter =
            filter
              ( \case
                  GeneratorNeedsMigrationWarning _ -> False
                  _ -> True
              )
        }
