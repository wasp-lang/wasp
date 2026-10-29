module Wasp.Cli.Command.Db
  ( makeDbCommand,
    throwIfDbConnectionNotEstablished,
  )
where

import Control.Monad (unless)
import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import qualified Wasp.AppSpec as AS
import Wasp.Cli.Command (Command, CommandError (CommandError), require)
import Wasp.Cli.Command.Compile (defaultCompileOptions, withSetupGoals)
import Wasp.Cli.Command.Require.GeneratedApp (GeneratedAppIsDevelopment (GeneratedAppIsDevelopment))
import Wasp.Cli.Command.Require.InWaspProject (InWaspProject (InWaspProject))
import Wasp.Cli.Command.Require.ValidNodeAndNpm (ValidNodeAndNpm (ValidNodeAndNpm))
import Wasp.Cli.Command.Require.WaspSpecAvailable (WaspSpecAvailable (WaspSpecAvailable))
import Wasp.Cli.Compile (waitEnsureCompile)
import Wasp.Cli.ProjectRunConfig (makeDefaultDevProjectRunConfig)
import Wasp.Generator.DbGenerator.Operations (isDbConnectionPossible, testDbConnection)
import qualified Wasp.Generator.ServerGenerator.RunConfig as ServerGenerator
import Wasp.Generator.Setup (SetupGoal)
import Wasp.Project.RunConfig (ProjectRunConfig)

-- | Prepares what a db command needs before it runs: a compile,
--   post-compile setup, and a reachable database. Gives the command the
--   run config, which tells it how to connect to the database.
--
--   All the commands that operate on the db should be created using this function.
makeDbCommand :: [SetupGoal] -> (AS.AppSpec -> ProjectRunConfig -> Command a) -> Command a
makeDbCommand setupGoals cmd = do
  InWaspProject waspProjectDir <- require
  WaspSpecAvailable <- require
  appSpec <- waitEnsureCompile $ withSetupGoals setupGoals $ defaultCompileOptions waspProjectDir
  projectRunConfig <- liftIO $ makeDefaultDevProjectRunConfig appSpec
  throwIfDbConnectionNotEstablished projectRunConfig
  cmd appSpec projectRunConfig

throwIfDbConnectionNotEstablished :: ProjectRunConfig -> Command ()
throwIfDbConnectionNotEstablished projectRunConfig = do
  -- TODO: Remove `GeneratedAppIsDevelopment` requirement:
  -- https://github.com/wasp-lang/wasp/issues/2858.
  -- The reason why we need it is because a Production build by design does
  -- not have some files like `prisma.schema`, which makes it tricky to
  -- connect to the database. See the linked issue for more details.
  GeneratedAppIsDevelopment outDir <- require
  -- Testing the connection runs Prisma, so it requires Node.js and npm to be
  -- present.
  ValidNodeAndNpm <- require

  dbIsRunning <- liftIO $ isDbConnectionPossible <$> testDbConnection (ServerGenerator.makeEnvVars projectRunConfig) outDir

  unless dbIsRunning $ throwError noDbError
  where
    noDbError =
      CommandError
        "Can not connect to database"
        ( "The database needs to be running in order to execute this command."
            ++ " You can easily start a managed dev database with `wasp start db`."
        )
