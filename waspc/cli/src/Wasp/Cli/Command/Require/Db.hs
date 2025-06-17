module Wasp.Cli.Command.Require.Db
  ( DbConnectionEstablishedFromBuildDir (..),
    DbConnectionEstablishedFromOutDir (..),
  )
where

import Control.Monad.Error.Class (throwError)
import Control.Monad.IO.Class (liftIO)
import Data.Data (Typeable)
import qualified StrongPath as SP
import Wasp.Cli.Command (Command, CommandError (CommandError), Requirable (checkRequirement), require)
import Wasp.Cli.Command.Require (InWaspProject (InWaspProject))
import Wasp.Generator.Common (ProjectRootDir)
import Wasp.Generator.DbGenerator.Operations (isDbConnectionPossible, testDbConnection)
import Wasp.Job.IO (readJobMessagesAndPrintThemPrefixed)
import qualified Wasp.Project.Common as Project.Common

-- NOTE: 'InWaspProject' does not depend on these requirements, so these
-- 'require' calls will not result in an infinite loop.

data DbConnectionEstablishedFromBuildDir = DbConnectionEstablishedFromBuildDir deriving (Typeable)

data DbConnectionEstablishedFromOutDir = DbConnectionEstablishedFromOutDir deriving (Typeable)

instance Requirable DbConnectionEstablishedFromBuildDir where
  checkRequirement = do
    InWaspProject waspProjectDir <- require
    let buildDir =
          waspProjectDir
            SP.</> Project.Common.dotWaspDirInWaspProjectDir
            SP.</> Project.Common.buildDirInDotWaspDir
    checkConnectionEstablishedFromDir buildDir
    return DbConnectionEstablishedFromBuildDir

instance Requirable DbConnectionEstablishedFromOutDir where
  checkRequirement = do
    InWaspProject waspProjectDir <- require
    let outDir =
          waspProjectDir
            SP.</> Project.Common.dotWaspDirInWaspProjectDir
            SP.</> Project.Common.generatedCodeDirInDotWaspDir
    checkConnectionEstablishedFromDir outDir
    return DbConnectionEstablishedFromOutDir

checkConnectionEstablishedFromDir :: SP.Path' SP.Abs (SP.Dir ProjectRootDir) -> Command ()
checkConnectionEstablishedFromDir projectDir = do
  dbIsRunning <- liftIO $ isDbConnectionPossible <$> testDbConnection projectDir

  either
    ( \chan -> do
        liftIO (readJobMessagesAndPrintThemPrefixed chan)
        throwError noDbError
    )
    (const $ return ())
    dbIsRunning
  where
    noDbError =
      CommandError
        "Can not connect to database"
        ( "The database needs to be running in order to execute this command."
            ++ " You can easily start a managed dev database with `wasp start db`."
        )
