module Wasp.Cli.Command.Db
  ( makeDbCommand,
  )
where

import qualified Wasp.AppSpec as AS
import Wasp.Cli.Command (Command, require)
import Wasp.Cli.Command.Compile (compileWithOptions, defaultCompileOptions, withSetupSteps)
import Wasp.Cli.Command.Require.DbConnectionEstablished (DbConnectionEstablished (DbConnectionEstablished))
import Wasp.Cli.Command.Require.InWaspProject (InWaspProject (InWaspProject))
import Wasp.Cli.Command.Require.WaspSpecAvailable (WaspSpecAvailable (WaspSpecAvailable))
import Wasp.Cli.ProjectLock (withProjectLock)
import Wasp.CompileOptions (CompileOptions (generatorWarningsFilter))
import Wasp.Generator.Monad (GeneratorWarning (GeneratorNeedsMigrationWarning))
import Wasp.Generator.Setup (SetupStep)

-- | Prepares what a db command needs before it runs: a compile, setup steps,
--   and a reachable database.
--
--   All the commands that operate on the db should be created using this function.
makeDbCommand :: [SetupStep] -> (AS.AppSpec -> Command a) -> Command a
makeDbCommand dbCommandSetupSteps cmd = withProjectLock $ do
  InWaspProject waspProjectDir <- require
  WaspSpecAvailable <- require
  (_, appSpec) <- compileWithOptions $ compileOptions waspProjectDir
  DbConnectionEstablished <- require
  cmd appSpec
  where
    compileOptions waspProjectDir =
      withSetupSteps dbCommandSetupSteps $
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
