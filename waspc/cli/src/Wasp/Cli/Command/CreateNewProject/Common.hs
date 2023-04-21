module Wasp.Cli.Command.CreateNewProject.Common where

import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import StrongPath (Abs, Dir, Path')
import Wasp.Cli.Command (Command, CommandError (..))
import Wasp.Cli.FileSystem (getAbsPathToDirInCwd)
import Wasp.Project (WaspProjectDir)
import qualified Wasp.SemanticVersion as SV
import qualified Wasp.Version as WV

throwProjectCreationError :: String -> Command a
throwProjectCreationError = throwError . CommandError "Project creation failed"

throwInvalidTemplateNameUsedError :: Command a
throwInvalidTemplateNameUsedError = throwProjectCreationError "Are you sure that the template exists? 🤔 Check the list of templates here: https://github.com/wasp-lang/starters"

getAbsPathToNewProjectDirInCwd :: String -> Command (Path' Abs (Dir WaspProjectDir))
getAbsPathToNewProjectDirInCwd projectDir = do
  absPathOrError <- liftIO $ getAbsPathToDirInCwd projectDir

  case absPathOrError of
    Right absPathToNewProjectInCwd ->
      return absPathToNewProjectInCwd
    Left err ->
      throwProjectCreationError $ "Failed to get absolute path to Wasp project dir: " ++ show err

waspVersionBounds :: String
waspVersionBounds = show (SV.backwardsCompatibleWith WV.waspVersion)
