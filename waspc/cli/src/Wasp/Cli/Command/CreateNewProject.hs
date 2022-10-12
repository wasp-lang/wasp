module Wasp.Cli.Command.CreateNewProject
  ( createNewProject,
  )
where

import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import Data.List (intercalate)
import StrongPath (Abs, Dir, Dir', File', Path', Rel, reldir, relfile, (</>))
import qualified StrongPath as SP
import System.Directory (createDirectory, getCurrentDirectory)
import qualified System.FilePath as FP
import Text.Printf (printf)
import Wasp.Analyzer.Parser (isValidWaspIdentifier)
import Wasp.AppSpec.ExternalCode (SourceExternalCodeDir)
import Wasp.Cli.Command (Command, CommandError (..))
import qualified Wasp.Cli.Command.Common as Command.Common
import qualified Wasp.Cli.Common as Common
import qualified Wasp.Data as Data
import Wasp.Generator.FileDraft.WriteableMonad (WriteableMonad (copyDirectoryRecursive))
import Wasp.Util (indent, kebabToCamelCase)
import qualified Wasp.Util.Terminal as Term

data ProjectInfo = ProjectInfo
  { _projectName :: String,
    _appName :: String
  }

-- Takes a project name String
-- Returns either the ProjectInfo type that contains both the Project name
-- and the App name (which might be the same), or an error describing why the name is invalid
parseProjectInfo :: String -> Either String ProjectInfo
parseProjectInfo name
  | isValidWaspIdentifier appName = Right (ProjectInfo name appName)
  | otherwise =
    Left $
      intercalate
        "\n"
        [ "The project's name is not in the valid format!",
          indent 2 "- It can start with a letter or an underscore.",
          indent 2 "- It can contain only letters, numbers, dashes, or underscores.",
          indent 2 "- It can't be a Wasp keyword."
        ]
  where
    appName = kebabToCamelCase name

createNewProject :: String -> Command ()
createNewProject name =
  case parseProjectInfo name of
    Right projectInfo -> createNewProject' projectInfo
    Left parsedError ->
      throwError $
        CommandError "Project creation failed" parsedError

createNewProject' :: ProjectInfo -> Command ()
createNewProject' (ProjectInfo projectName appName) = do
  absCwd <- liftIO getCurrentDirectory
  waspProjectDir <- case SP.parseAbsDir $ absCwd FP.</> projectName of
    Left err ->
      throwError $
        CommandError
          "Project creation failed"
          ( "Failed to parse absolute path to wasp project dir: "
              ++ show err
          )
    Right sp -> return sp
  liftIO $ do
    createDirectorySP waspProjectDir
    writeFileSP (waspProjectDir </> mainWaspFileInWaspProjectDir) mainWaspFileContent
    writeFileSP (waspProjectDir </> gitignoreFileInWaspProjectDir) gitignoreFileContent
    writeFileSP
      (waspProjectDir </> Common.dotWaspRootFileInWaspProjectDir)
      "File marking the root of Wasp project."

  liftIO $ initializeExternalCodeDirs waspProjectDir

  liftIO $ do
    putStrLn $ Term.applyStyles [Term.Green] ("Created new Wasp app in ./" ++ projectName ++ " directory!")
    putStrLn "To run it, do:"
    putStrLn ""
    putStrLn $ Term.applyStyles [Term.Bold] ("    cd " ++ projectName)
    putStrLn $ Term.applyStyles [Term.Bold] "    wasp start"
    putStrLn ""
    putStrLn Command.Common.alphaWarningMessage
  where
    mainWaspFileInWaspProjectDir :: Path' (Rel Common.WaspProjectDir) File'
    mainWaspFileInWaspProjectDir = [relfile|main.wasp|]

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

    gitignoreFileInWaspProjectDir :: Path' (Rel Common.WaspProjectDir) File'
    gitignoreFileInWaspProjectDir = [relfile|.gitignore|]

    gitignoreFileContent =
      unlines
        [ "/.wasp/",
          "/.env.server",
          "/.env.client"
        ]

initializeExternalCodeDirs :: Path' Abs (Dir Common.WaspProjectDir) -> IO ()
initializeExternalCodeDirs waspProjectDir = do
  dataDir <- Data.getAbsDataDirPath
  let copySkeletonDir' = copySkeletonDir dataDir waspProjectDir
  copySkeletonDir' serverSkeletonDirInTemplatesDir Common.extServerCodeDirInWaspProjectDir
  copySkeletonDir' clientSkeletonDirInTemplatesDir Common.extClientCodeDirInWaspProjectDir

copySkeletonDir ::
  Path' Abs (Dir Data.DataDir) ->
  Path' Abs (Dir Common.WaspProjectDir) ->
  Path' (Rel Common.CliTemplatesDir) (Dir a) ->
  Path' (Rel Common.WaspProjectDir) (Dir SourceExternalCodeDir) ->
  IO ()
copySkeletonDir dataDir waspProjectDir skeletonDirinTemplatesDir extCodeDirInWaspProjectDir = do
  let skeletonAbs = dataDir </> cliTemplatesDirInDataDir </> skeletonDirinTemplatesDir
  let extCodeDirAbs = waspProjectDir </> extCodeDirInWaspProjectDir
  copyDirectoryRecursive skeletonAbs extCodeDirAbs

writeFileSP :: Path' Abs (SP.File f) -> String -> IO ()
writeFileSP = writeFile . SP.fromAbsFile

createDirectorySP :: Path' Abs (Dir d) -> IO ()
createDirectorySP = createDirectory . SP.fromAbsDir

cliTemplatesDirInDataDir :: Path' (Rel Data.DataDir) (Dir Common.CliTemplatesDir)
cliTemplatesDirInDataDir = [reldir|Cli/templates|]

serverSkeletonDirInTemplatesDir :: Path' (Rel Common.CliTemplatesDir) Dir'
serverSkeletonDirInTemplatesDir = [reldir|new/server|]

clientSkeletonDirInTemplatesDir :: Path' (Rel Common.CliTemplatesDir) Dir'
clientSkeletonDirInTemplatesDir = [reldir|new/client|]