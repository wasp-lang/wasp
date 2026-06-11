module Wasp.Cli.Command.CreateNewProject.Common
  ( throwProjectCreationError,
    defaultWaspVersionBounds,
    TemplateOutputDir,
  )
where

import Control.Monad.Except (throwError)
import Wasp.Cli.Command (Command, CommandError (..))
import qualified Wasp.SemanticVersion as SV
import qualified Wasp.Version as WV

data TemplateOutputDir

throwProjectCreationError :: String -> Command a
throwProjectCreationError = throwError . CommandError "Project creation failed"

defaultWaspVersionBounds :: String
defaultWaspVersionBounds = show (SV.backwardsCompatibleWith WV.waspVersion)
