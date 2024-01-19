module Wasp.Cli.Command.CreateNewProject.Common
  ( throwProjectCreationError,
    throwInvalidTemplateNameUsedError,
    defaultWaspVersionBounds,
    printGettingStartedInstructions,
  )
where

import Control.Monad.Except (throwError)
import StrongPath (Abs, Dir, Path')
import qualified StrongPath as SP
import Wasp.Cli.Command (Command, CommandError (..))
import Wasp.Project.Common (WaspProjectDir)
import qualified Wasp.SemanticVersion as SV
import qualified Wasp.Util.Terminal as Term
import qualified Wasp.Version as WV

throwProjectCreationError :: String -> Command a
throwProjectCreationError = throwError . CommandError "Project creation failed"

throwInvalidTemplateNameUsedError :: Command a
throwInvalidTemplateNameUsedError =
  throwProjectCreationError $
    "Are you sure that the template exists?"
      <> " 🤔 Check the list of templates here: https://github.com/wasp-lang/starters"

defaultWaspVersionBounds :: String
defaultWaspVersionBounds = show (SV.backwardsCompatibleWith WV.waspVersion)

-- | This function assumes that the project dir is created inside the current working directory
-- when it prints the instructions.
printGettingStartedInstructions :: Path' Abs (Dir WaspProjectDir) -> IO ()
printGettingStartedInstructions absProjectDir = do
  let projectFolder = init . SP.toFilePath . SP.basename $ absProjectDir
{- ORMOLU_DISABLE -}
  putStrLn $ Term.applyStyles [Term.Green] $ "Created new Wasp app in ./" ++ projectFolder ++ " directory!"
  putStrLn                                   "To run it, do:"
  putStrLn                                   ""
  putStrLn $ Term.applyStyles [Term.Bold] $  "    cd " ++ projectFolder
  putStrLn $ Term.applyStyles [Term.Bold]    "    wasp start"
  putStrLn                                   ""
{- ORMOLU_ENABLE -}
