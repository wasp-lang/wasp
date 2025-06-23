module Wasp.Cli.Command.Require.Db
  ( DbConnectionEstablishedFromOutDir (DbConnectionEstablishedFromOutDir),
  )
where

import Control.Monad.Error.Class (throwError)
import Control.Monad.IO.Class (liftIO)
import Data.Data (Typeable)
import qualified StrongPath as SP
import Wasp.Cli.Command (CommandError (CommandError), Requirable (checkRequirement), require)
import Wasp.Cli.Command.Require (InWaspProject (InWaspProject))
import Wasp.Generator.DbGenerator.Operations (isDbConnectionPossible, testDbConnection)
import qualified Wasp.Project.Common as Project.Common

data DbConnectionEstablishedFromOutDir = DbConnectionEstablishedFromOutDir deriving (Typeable)

-- TODO(carlos): Modify this requirement to work in `.wasp/build` as well
--
-- Why is this requirement specific to `.wasp/out`? For the time being, most of
-- our DB are designed to work in the context of `wasp start`, and the functions
-- end up depending on two specific outputs:
-- 1. the `.env.server` file, as copied to `<WaspProjectDir>/server/.env`
-- 2. the `schema.prisma` file, processed by the Wasp compiler (to add Auth
--   entities), and copied to `<WaspProjectDir>/db/schema.prisma`
--
-- The `wasp start` command generates these files, but `wasp build` does not, as
-- it expects the environment variables to be given to the resulting Docker
-- container. Moreover, the processing of the `schema.prisma` file is done
-- inside the Docker image building process, so the user's local filesystem
-- doesn't see the final schema file, making it unavailable for the compiler
-- running normally in the user's environment.

instance Requirable DbConnectionEstablishedFromOutDir where
  checkRequirement = do
    -- NOTE: 'InWaspProject' does not depend on this requirement, so this
    -- call to 'require' will not result in an infinite loop.
    InWaspProject waspProjectDir <- require
    let outDir =
          waspProjectDir
            SP.</> Project.Common.dotWaspDirInWaspProjectDir
            SP.</> Project.Common.generatedCodeDirInDotWaspDir
    dbIsRunning <- liftIO $ isDbConnectionPossible <$> testDbConnection outDir

    if dbIsRunning
      then return DbConnectionEstablishedFromOutDir
      else throwError noDbError
    where
      noDbError =
        CommandError
          "Can not connect to database"
          ( "The database needs to be running in order to execute this command."
              ++ " You can easily start a managed dev database with `wasp start db`."
          )
