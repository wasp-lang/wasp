module Wasp.Cli.Command.Require.DbConnectionEstablished
  ( ensureDbConnectionEstablished,
  )
where

import Control.Monad.Error.Class (throwError)
import Control.Monad.IO.Class (liftIO)
import Wasp.Cli.Command (Command, CommandError (CommandError), require)
import Wasp.Cli.Command.Require.GeneratedApp (GeneratedAppIsDevelopment (GeneratedAppIsDevelopment))
import Wasp.Cli.Command.Require.ValidNodeAndNpm (ValidNodeAndNpm (ValidNodeAndNpm))
import Wasp.Env (EnvVar)
import Wasp.Generator.DbGenerator.Operations (isDbConnectionPossible, testDbConnection)

ensureDbConnectionEstablished :: [EnvVar] -> Command ()
ensureDbConnectionEstablished dbEnvVars = do
  GeneratedAppIsDevelopment outDir <- require
  ValidNodeAndNpm <- require
  dbIsRunning <- liftIO $ isDbConnectionPossible <$> testDbConnection dbEnvVars outDir
  if dbIsRunning
    then return ()
    else throwError noDbError
  where
    noDbError =
      CommandError
        "Can not connect to database"
        ( "The database needs to be running in order to execute this command."
            ++ " You can easily start a managed dev database with `wasp start db`."
        )
