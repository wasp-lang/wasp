module Wasp.Cli.Command.Require.DbConnectionEstablished
  ( DbConnectionEstablished (DbConnectionEstablished),
  )
where

import Control.Monad.Error.Class (throwError)
import Control.Monad.IO.Class (liftIO)
import Data.Data (Typeable)
import Wasp.Cli.Command (CommandError (CommandError), Requirable (checkRequirement), require)
import Wasp.Cli.Command.Require.GeneratedApp (GeneratedAppIsDevelopment (GeneratedAppIsDevelopment))
import Wasp.Generator.DbGenerator.Operations (isDbConnectionPossible, testDbConnection)

data DbConnectionEstablished = DbConnectionEstablished deriving (Typeable)

instance Requirable DbConnectionEstablished where
  checkRequirement = do
    -- TODO: Remove `GeneratedAppIsDevelopment` requirement:
    -- https://github.com/wasp-lang/wasp/issues/2858.
    -- The reason why we need it is because a Production build by design does
    -- not have some files like `.env` or `prisma.schema`, which makes it tricky
    -- to determine the database location. See the linked issue for more
    -- details.
    GeneratedAppIsDevelopment outDir <- require

    dbIsRunning <- liftIO $ isDbConnectionPossible <$> testDbConnection outDir

    if dbIsRunning
      then return DbConnectionEstablished
      else throwError noDbError
    where
      noDbError =
        CommandError
          "Can not connect to database"
          ( "The database needs to be running in order to execute this command."
              ++ " You can easily start a managed dev database with `wasp start db`."
          )
