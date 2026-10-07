module Wasp.Cli.Command.Db
  ( runCommandThatRequiresDbRunning,
  )
where

import qualified Options.Applicative as Opt
import Wasp.Cli.Command (Command, require, runCommand)
import Wasp.Cli.Command.Call (Arguments)
import Wasp.Cli.Command.Compile (compileWithOptions, defaultCompileOptions)
import qualified Wasp.Cli.Command.Db.Lifecycle as DbLifecycle
import Wasp.Cli.Command.Db.StartOptions (dbStartOptionsParser)
import Wasp.Cli.Command.Require.DbConnectionEstablished (DbConnectionEstablished (DbConnectionEstablished))
import Wasp.Cli.Command.Require.InWaspProject (InWaspProject (InWaspProject))
import Wasp.Cli.Command.Require.WaspSpecAvailable (WaspSpecAvailable (WaspSpecAvailable))
import Wasp.Cli.ProjectLock (withProjectLock)
import Wasp.Cli.Util.Parser (withArguments)
import Wasp.CompileOptions (CompileOptions (generatorWarningsFilter))
import Wasp.Generator.Monad (GeneratorWarning (GeneratorNeedsMigrationWarning))

runCommandThatRequiresDbRunning :: String -> Opt.Parser a -> (a -> Command ()) -> Arguments -> IO ()
runCommandThatRequiresDbRunning commandName parser command args =
  runCommand $ withArguments commandName ((,) <$> dbStartOptionsParser <*> parser) run args
  where
    run (dbStartOptions, commandArgs) =
      withProjectLock $ DbLifecycle.withManagedDb dbStartOptions $ \_ -> makeDbCommand (command commandArgs)

-- | This function makes sure that all the prerequisites which db commands
--   need are set up (e.g. makes sure Prisma CLI is installed).
--
--   All the commands that operate on db should be created using this function.
makeDbCommand :: Command a -> Command a
makeDbCommand cmd = do
  -- Ensure code is generated and npm dependencies are installed.
  InWaspProject waspProjectDir <- require
  WaspSpecAvailable <- require
  _ <- compileWithOptions $ compileOptions waspProjectDir
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
