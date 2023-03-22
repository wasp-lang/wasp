module Wasp.Cli.Command.CreateNewProject
  ( createNewProject,
  )
where

import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import Data.List (intercalate)
import Path.IO (copyDirRecur, doesDirExist)
import StrongPath (Abs, Dir, Path, Path', System, parseAbsDir, reldir, relfile, (</>))
import StrongPath.Path (toPathAbsDir)
import System.Directory (getCurrentDirectory)
import qualified System.FilePath as FP
import Text.Printf (printf)
import Wasp.Analyzer.Parser (isValidWaspIdentifier)
import Wasp.Cli.Command (Command, CommandError (..))
import qualified Wasp.Data as Data
import Wasp.Project (WaspProjectDir)
import Wasp.Util (indent, kebabToCamelCase)
import qualified Wasp.Util.IO as IOUtil
import qualified Wasp.Util.Terminal as Term
import qualified Wasp.Version as WV

data ProjectInfo = ProjectInfo
  { _projectName :: String,
    _appName :: String
  }

createNewProject :: String -> Command ()
createNewProject projectNameCandidate = do
  projectInfo <- parseProjectInfo projectNameCandidate
  createWaspProjectDir projectInfo
  liftIO $ printGettingStartedInstructions $ _projectName projectInfo
  where
    printGettingStartedInstructions :: String -> IO ()
    printGettingStartedInstructions projectName = do
      putStrLn $ Term.applyStyles [Term.Green] ("Created new Wasp app in ./" ++ projectName ++ " directory!")
      putStrLn "To run it, do:"
      putStrLn ""
      putStrLn $ Term.applyStyles [Term.Bold] ("    cd " ++ projectName)
      putStrLn $ Term.applyStyles [Term.Bold] "    wasp start"

-- Takes a project name String
-- Returns either the ProjectInfo type that contains both the Project name
-- and the App name (which might be the same), or an error describing why the name is invalid
parseProjectInfo :: String -> Command ProjectInfo
parseProjectInfo name
  | isValidWaspIdentifier appName = return $ ProjectInfo name appName
  | otherwise =
      throwProjectCreationError $
        intercalate
          "\n"
          [ "The project's name is not in the valid format!",
            indent 2 "- It can start with a letter or an underscore.",
            indent 2 "- It can contain only letters, numbers, dashes, or underscores.",
            indent 2 "- It can't be a Wasp keyword."
          ]
  where
    appName = kebabToCamelCase name

createWaspProjectDir :: ProjectInfo -> Command ()
createWaspProjectDir projectInfo = do
  absWaspProjectDir <- getAbsoluteWaspProjectDir projectInfo
  dirExists <- doesDirExist $ toPathAbsDir absWaspProjectDir
  if dirExists
    then throwProjectCreationError $ show absWaspProjectDir ++ " is an existing directory"
    else liftIO $ do
      initializeProjectFromSkeleton absWaspProjectDir
      writeMainWaspFile absWaspProjectDir projectInfo

getAbsoluteWaspProjectDir :: ProjectInfo -> Command (Path System Abs (Dir WaspProjectDir))
getAbsoluteWaspProjectDir (ProjectInfo projectName _) = do
  absCwd <- liftIO getCurrentDirectory
  case parseAbsDir $ absCwd FP.</> projectName of
    Right sp -> return sp
    Left err ->
      throwProjectCreationError $
        "Failed to parse absolute path to wasp project dir: " ++ show err

-- Copies prepared files to the new project directory.
initializeProjectFromSkeleton :: Path' Abs (Dir WaspProjectDir) -> IO ()
initializeProjectFromSkeleton absWaspProjectDir = do
  dataDir <- Data.getAbsDataDirPath
  let absSkeletonDir = dataDir </> [reldir|Cli/templates/new|]
  copyDirRecur (toPathAbsDir absSkeletonDir) (toPathAbsDir absWaspProjectDir)

writeMainWaspFile :: Path System Abs (Dir WaspProjectDir) -> ProjectInfo -> IO ()
writeMainWaspFile waspProjectDir (ProjectInfo projectName appName) = IOUtil.writeFile absMainWaspFile mainWaspFileContent
  where
    absMainWaspFile = waspProjectDir </> [relfile|main.wasp|]
    mainWaspFileContent =
      unlines
        [ "app %s {" `printf` appName,
          "  wasp: {",
          "    version: \"^%s\"" `printf` show WV.waspVersion,
          "  },",
          "  title: \"%s\"" `printf` projectName,
          "}",
          "",
          "route RootRoute { path: \"/\", to: MainPage }",
          "page MainPage {",
          "  component: import Main from \"@client/MainPage.jsx\"",
          "}"
        ]

throwProjectCreationError :: String -> Command a
throwProjectCreationError = throwError . CommandError "Project creation failed"
