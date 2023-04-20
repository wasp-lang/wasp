module Wasp.Cli.Command.CreateNewProject.Common where

import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import StrongPath (Abs, Dir, Path, System, parseAbsDir)
import System.Directory (getCurrentDirectory)
import qualified System.FilePath as FP
import Wasp.Cli.Command (Command, CommandError (..))
import Wasp.Project (WaspProjectDir)

throwProjectCreationError :: String -> Command a
throwProjectCreationError = throwError . CommandError "Project creation failed"

throwInvalidTemplateNameUsedError :: Command a
throwInvalidTemplateNameUsedError = throwProjectCreationError "Are you sure that the template exists? 🤔 Check the list of templates here: https://github.com/wasp-lang/starters"

getAbsoluteWaspProjectDir :: String -> Command (Path System Abs (Dir WaspProjectDir))
getAbsoluteWaspProjectDir projectName = do
  absCwd <- liftIO getCurrentDirectory
  case parseAbsDir $ absCwd FP.</> projectName of
    Right sp -> return sp
    Left err ->
      throwProjectCreationError $
        "Failed to parse absolute path to wasp project dir: " ++ show err
