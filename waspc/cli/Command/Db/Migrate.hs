module Command.Db.Migrate
    ( migrate
    ) where

import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)

import StrongPath ((</>))
import Command (Command, CommandError(..))
import Command.Common (findWaspProjectRootFromCwd, waspSays)
import qualified Common
import qualified Generator.DbGenerator.Operations as DbOps

migrate :: String -> Command ()
migrate migrationName = do
    waspRootDir <- findWaspProjectRootFromCwd
    let genProjectRootDir = waspRootDir </> Common.dotWaspDirInWaspProjectDir </>
                         Common.generatedCodeDirInDotWaspDir

    waspSays "Saving db migration..."
    migrateSaveResult <- liftIO $ DbOps.migrateSave genProjectRootDir migrationName
    case migrateSaveResult of
        Left migrateSaveError -> throwError $ CommandError $ "Migrate save failed: " ++ migrateSaveError
        Right () -> waspSays "Migration has been successfully saved."

    waspSays "All done!"

