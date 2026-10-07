module Wasp.Cli.Command.Db
  ( makeDbCommand,
  )
where

import qualified Wasp.AppSpec as AS
import Wasp.Cli.Command (Command, require)
import Wasp.Cli.Command.Compile (compileWithOptions, defaultCompileOptions)
import Wasp.Cli.Command.Require.DbConnectionEstablished (DbConnectionEstablished (DbConnectionEstablished))
import Wasp.Cli.Command.Require.InWaspProject (InWaspProject (InWaspProject))
import Wasp.Cli.Command.Require.WaspSpecAvailable (WaspSpecAvailable (WaspSpecAvailable))
import Wasp.Cli.ProjectLock (withProjectLock)
import Wasp.CompileOptions (CompileOptions (setupSteps))
import Wasp.Generator.Setup (SetupStep (..))

-- | Prepares what a db command needs before it runs: the generated code, the
--   setup steps every db command needs plus the extra ones given, and a
--   reachable database. The command then gets the analyzed spec.
--
--   Only the steps listed here run, so a db command does not build the SDK or
--   type-check the user's code unless it asks for it. That keeps the commands
--   fast and lets them run while the user's code has type errors.
--
--   All the commands that operate on the db should be created using this function.
makeDbCommand :: [SetupStep] -> (AS.AppSpec -> Command a) -> Command a
makeDbCommand extraSetupSteps cmd = withProjectLock $ do
  InWaspProject waspProjectDir <- require
  WaspSpecAvailable <- require
  (_, appSpec) <- compileWithOptions $ compileOptions waspProjectDir
  DbConnectionEstablished <- require
  cmd appSpec
  where
    compileOptions waspProjectDir =
      (defaultCompileOptions waspProjectDir)
        { setupSteps = prismaCliSetupSteps ++ extraSetupSteps
        }

    -- What Prisma needs to run against the generated schema: the npm install
    -- that provides the Prisma CLI, and a formatted schema so the checksums
    -- Wasp keeps next to it stay consistent with the ones `wasp start` writes.
    prismaCliSetupSteps :: [SetupStep]
    prismaCliSetupSteps = [InstallNpmDeps, FormatPrismaSchema]
