module Wasp.Cli.Command.Db
  ( runCommandThatRequiresDbRunning,
  )
where

import Wasp.Cli.Command (Command, runCommand)
import Wasp.Cli.Command.Compile (compileWithOptions, defaultCompileOptions)
import Wasp.Cli.Command.Require (InWaspProject (InWaspProject), require)
import Wasp.Cli.Command.Require.Db (DbConnectionEstablishedFromOutDir (DbConnectionEstablishedFromOutDir))
import Wasp.CompileOptions (CompileOptions (generatorWarningsFilter))
import Wasp.Generator.Monad (GeneratorWarning (GeneratorNeedsMigrationWarning))

runCommandThatRequiresDbRunning :: Command a -> IO ()
runCommandThatRequiresDbRunning = runCommand . makeDbCommand

-- | This function makes sure that all the prerequisites which db commands
--   need are set up (e.g. makes sure Prisma CLI is installed).
--
--   All the commands that operate on db should be created using this function.
makeDbCommand :: Command a -> Command a
makeDbCommand cmd = do
  -- Ensure code is generated and npm dependencies are installed.
  InWaspProject waspProjectDir <- require
  _ <- compileWithOptions $ compileOptions waspProjectDir
  DbConnectionEstablishedFromOutDir <- require
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
