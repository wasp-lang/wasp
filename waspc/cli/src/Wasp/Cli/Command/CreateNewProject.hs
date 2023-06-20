module Wasp.Cli.Command.CreateNewProject
  ( createNewProject,
    -- TODO: I just exported whatever I needed, I should think more through how to abstract this really.
    createInitialWaspProjectDir,
    parseProjectInfo,
    ProjectInfo (..),
    getAbsoluteWaspProjectDir,
    readCoreWaspProjectFiles,
    createEmptyWaspProjectDir,
  )
where

import Control.Monad (when)
import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import Data.List (intercalate)
import Data.Text (Text)
import Path.IO (copyDirRecur, doesDirExist)
import StrongPath (Abs, Dir, File', Path, Path', Rel, System, fromAbsDir, parseAbsDir, reldir, relfile, (</>))
import StrongPath.Path (toPathAbsDir)
import System.Directory (createDirectory, getCurrentDirectory)
import qualified System.FilePath as FP
import Text.Printf (printf)
import Wasp.Analyzer.Parser (isValidWaspIdentifier)
import Wasp.Cli.Command (Command, CommandError (..))
import qualified Wasp.Data as Data
import Wasp.Project (WaspProjectDir)
import Wasp.Util (indent, kebabToCamelCase)
import Wasp.Util.IO (readFileStrict)
import qualified Wasp.Util.IO as IOUtil
import Control.Monad.IO.Class (liftIO)
import Data.Function ((&))
import StrongPath (Abs, Dir, Path')
import qualified StrongPath as SP
import Wasp.Cli.Command (Command)
import Wasp.Cli.Command.Call (Arguments)
import Wasp.Cli.Command.CreateNewProject.ArgumentsParser (parseNewProjectArgs)
import Wasp.Cli.Command.CreateNewProject.Common (throwProjectCreationError)
import Wasp.Cli.Command.CreateNewProject.ProjectDescription
  ( NewProjectDescription (..),
    obtainNewProjectDescription,
  )
import Wasp.Cli.Command.CreateNewProject.StarterTemplates
  ( StarterTemplateName (..),
    getStarterTemplateNames,
  )
import Wasp.Cli.Command.CreateNewProject.StarterTemplates.Local (createProjectOnDiskFromLocalTemplate)
import Wasp.Cli.Command.CreateNewProject.StarterTemplates.Remote (createProjectOnDiskFromRemoteTemplate)
import Wasp.Cli.Command.Message (cliSendMessageC)
import Wasp.Cli.Common (WaspProjectDir)
import qualified Wasp.Message as Msg
import qualified Wasp.Util.Terminal as Term

-- TODO(merge): what about new ai?
--   One for humans we could offer as an option during interactive choosing of template.
--   One for machine should have its own command. Probably `new:ai` would be fine.
--   I have to figure out how to fit all this in with the new changes.

-- It receives all of the arguments that were passed to the `wasp new` command.
createNewProject :: Arguments -> Command ()
createNewProject args = do
  newProjectArgs <- parseNewProjectArgs args & either throwProjectCreationError return
  starterTemplateNames <- liftIO getStarterTemplateNames

  newProjectDescription <- obtainNewProjectDescription newProjectArgs starterTemplateNames

  createProjectOnDisk newProjectDescription
  liftIO $ printGettingStartedInstructions $ _absWaspProjectDir newProjectDescription
  where
    -- This function assumes that the project dir is created inside the current working directory when it
    -- prints the instructions.
    printGettingStartedInstructions :: Path' Abs (Dir WaspProjectDir) -> IO ()
    printGettingStartedInstructions absProjectDir = do
      let projectFolder = init . SP.toFilePath . SP.basename $ absProjectDir
{- ORMOLU_DISABLE -}
      putStrLn $ Term.applyStyles [Term.Green] $ "Created new Wasp app in ./" ++ projectFolder ++ " directory!"
      putStrLn                                   "To run it, do:"
      putStrLn                                   ""
      putStrLn $ Term.applyStyles [Term.Bold] $  "    cd " ++ projectFolder
      putStrLn $ Term.applyStyles [Term.Bold]    "    wasp start"
{- ORMOLU_ENABLE -}

-- TODO(merge): I think I need this one, do I?
-- | Given project info, creates a new empty wasp app directory with appropriate name and no content
-- in it. Throws if such directory already exists. Returns path to the newly created directory.
createEmptyWaspProjectDir :: ProjectInfo -> Command (Path System Abs (Dir WaspProjectDir))
createEmptyWaspProjectDir projectInfo = do
  waspProjectDir <- determineWaspProjectDirAndThrowIfTaken projectInfo
  liftIO $ createDirectory $ fromAbsDir waspProjectDir
  return waspProjectDir

-- TODO(merge): seems like I need this one for the function above, but check if there
--   is some newer logic that does this.
determineWaspProjectDirAndThrowIfTaken :: ProjectInfo -> Command (Path System Abs (Dir WaspProjectDir))
determineWaspProjectDirAndThrowIfTaken projectInfo = do
  absWaspProjectDir <- getAbsoluteWaspProjectDir projectInfo
  dirExists <- doesDirExist $ toPathAbsDir absWaspProjectDir
  when dirExists $
    throwProjectCreationError $ show absWaspProjectDir ++ " is an existing directory"
  return absWaspProjectDir

-- TODO(merge): This is now repeating what is in templates/new which is not great.
coreWaspProjectFiles :: [Path System (Rel WaspProjectDir) File']
coreWaspProjectFiles =
  [ [relfile|.gitignore|],
    [relfile|.wasproot|],
    [relfile|src/.waspignore|],
    [relfile|src/client/tsconfig.json|],
    [relfile|src/client/vite-env.d.ts|],
    [relfile|src/server/tsconfig.json|],
    [relfile|src/shared/tsconfig.json|]
  ]

-- TODO(merge): Reorganize Cli/templates/new(or basic) into two dirs:
--   1. templates/core
--   2. templates/basic
--   Core would contain only the most neccessary files to get started.
--   Other templates, like basic, would build on top of it.
--   So creating new wasp project from local template would first copy files from "core",
--   then from the actual template (e.g. "basic").
readCoreWaspProjectFiles :: IO [(Path System (Rel WaspProjectDir) File', Text)]
readCoreWaspProjectFiles = do
  dataDir <- Data.getAbsDataDirPath
  let templatesNewDir = dataDir </> [reldir|Cli/templates/new|]
  contents <- mapM (readFileStrict . (templatesNewDir </>)) coreWaspProjectFiles
  return $ zip coreWaspProjectFiles contents

=======
createProjectOnDisk :: NewProjectDescription -> Command ()
createProjectOnDisk
  NewProjectDescription
    { _projectName = projectName,
      _appName = appName,
      _templateName = templateName,
      _absWaspProjectDir = absWaspProjectDir
    } = do
    cliSendMessageC $ Msg.Start $ "Creating your project from the " ++ show templateName ++ " template..."
    case templateName of
      RemoteStarterTemplate remoteTemplateName ->
        createProjectOnDiskFromRemoteTemplate absWaspProjectDir projectName appName remoteTemplateName
      LocalStarterTemplate localTemplateName ->
        liftIO $ createProjectOnDiskFromLocalTemplate absWaspProjectDir projectName appName localTemplateName
>>>>>>> main
