module Command.Db.Migrate
    ( migrate
    ) where

import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)

import StrongPath ((</>))
import Command (Command, CommandError(..))
import Command.Common (findWaspProjectRootDirFromCwd, waspSaysC)
import qualified Common
import qualified Generator.DbGenerator.Operations as DbOps


migrate :: String -> Command ()
migrate migrationName = do
    waspRootDir <- findWaspProjectRootDirFromCwd
    let genProjectRootDir = waspRootDir </> Common.dotWaspDirInWaspProjectDir </>
                         Common.generatedCodeDirInDotWaspDir

    waspSaysC "Saving db migration..."
    migrateSaveResult <- liftIO $ DbOps.migrateSave genProjectRootDir migrationName
    case migrateSaveResult of
        Left migrateSaveError -> throwError $ CommandError $ "Migrate save failed: " ++ migrateSaveError
        Right () -> waspSaysC "Migration has been successfully saved."

    waspSaysC "All done!"

