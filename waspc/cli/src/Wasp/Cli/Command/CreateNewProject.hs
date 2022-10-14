module Wasp.Cli.Command.CreateNewProject
  ( createNewProject,
  )
where

import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import Data.List (intercalate)
import StrongPath (Abs, Dir, Path, Path', System, reldir, relfile, (</>))
import qualified StrongPath as SP
import System.Directory (getCurrentDirectory)
import qualified System.FilePath as FP
import Text.Printf (printf)
import Wasp.Analyzer.Parser (isValidWaspIdentifier)
import Wasp.Cli.Command (Command, CommandError (..))
import qualified Wasp.Cli.Command.Common as Command.Common
import Wasp.Cli.Common (WaspProjectDir)
import qualified Wasp.Cli.Common as Common
import qualified Wasp.Data as Data
import Wasp.Generator.FileDraft.WriteableMonad (WriteableMonad (copyDirectoryRecursive))
import Wasp.Util (indent, kebabToCamelCase)
import qualified Wasp.Util.Terminal as Term

data ProjectInfo = ProjectInfo
  { _projectName :: String,
    _appName :: String
  }

createNewProject :: String -> Command ()
createNewProject projectName = do
  projectInfo <- parseProjectInfo projectName
  createAndPopulateWaspProjectDir projectInfo
  liftIO $ printInfoMessages projectInfo

-- Takes a project name String
-- Returns either the ProjectInfo type that contains both the Project name
-- and the App name (which might be the same), or an error describing why the name is invalid
parseProjectInfo :: String -> Command ProjectInfo
parseProjectInfo name
  | isValidWaspIdentifier appName = return $ ProjectInfo name appName
  | otherwise =
    throwError $
      CommandError "Project creation failed" $
        intercalate
          "\n"
          [ "The project's name is not in the valid format!",
            indent 2 "- It can start with a letter or an underscore.",
            indent 2 "- It can contain only letters, numbers, dashes, or underscores.",
            indent 2 "- It can't be a Wasp keyword."
          ]
  where
    appName = kebabToCamelCase name

createAndPopulateWaspProjectDir :: ProjectInfo -> Command ()
createAndPopulateWaspProjectDir projectInfo = do
  absWaspProjectDir <- getAbsoluteWaspProjectDir projectInfo
  liftIO $ initializeProjectFromSkeleton absWaspProjectDir
  liftIO $ writeMainWaspFile absWaspProjectDir projectInfo

printInfoMessages :: ProjectInfo -> IO ()
printInfoMessages (ProjectInfo projectName _) = do
  putStrLn $ Term.applyStyles [Term.Green] ("Created new Wasp app in ./" ++ projectName ++ " directory!")
  putStrLn "To run it, do:"
  putStrLn ""
  putStrLn $ Term.applyStyles [Term.Bold] ("    cd " ++ projectName)
  putStrLn $ Term.applyStyles [Term.Bold] "    wasp start"
  putStrLn ""
  putStrLn Command.Common.alphaWarningMessage

getAbsoluteWaspProjectDir :: ProjectInfo -> Command (Path System Abs (Dir WaspProjectDir))
getAbsoluteWaspProjectDir (ProjectInfo projectName _) = do
  absCwd <- liftIO getCurrentDirectory
  case SP.parseAbsDir $ absCwd FP.</> projectName of
    Right sp -> return sp
    Left err ->
      throwError $
        CommandError
          "Project creation failed"
          ("Failed to parse absolute path to wasp project dir: " ++ show err)

initializeProjectFromSkeleton :: Path' Abs (Dir Common.WaspProjectDir) -> IO ()
initializeProjectFromSkeleton absWaspProjectDir = do
  dataDir <- Data.getAbsDataDirPath
  let absSkeletonDir = dataDir </> [reldir|Cli/templates/new|]
  copyDirectoryRecursive absSkeletonDir absWaspProjectDir

writeMainWaspFile :: Path System Abs (Dir WaspProjectDir) -> ProjectInfo -> IO ()
writeMainWaspFile waspProjectDir (ProjectInfo projectName appName) = writeFile mainWaspFileAbsPath mainWaspFileContent
  where
    mainWaspFileAbsPath = SP.fromAbsFile $ waspProjectDir </> [relfile|main.wasp|]
    mainWaspFileContent =
      unlines
        [ "app %s {" `printf` appName,
          "  title: \"%s\"" `printf` projectName,
          "}",
          "",
          "route RootRoute { path: \"/\", to: MainPage }",
          "page MainPage {",
          "  component: import Main from \"@ext/MainPage.js\"",
          "}"
        ]