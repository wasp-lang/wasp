module Wasp.Cli.Command.CreateNewProject
  ( createNewProject,
  )
where

import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import Data.List (intercalate)
import StrongPath (Abs, Dir, File', Path', Rel, reldir, relfile, (</>))
import qualified StrongPath as SP
import System.Directory (createDirectory, getCurrentDirectory)
import qualified System.Directory
import qualified System.FilePath as FP
import Text.Printf (printf)
import Wasp.Analyzer.Parser (isValidWaspIdentifier)
import Wasp.AppSpec.ExternalCode (SourceExternalCodeDir)
import Wasp.Cli.Command (Command, CommandError (..))
import qualified Wasp.Cli.Command.Common as Command.Common
import qualified Wasp.Cli.Common as Common
import qualified Wasp.Data
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

  let extCodeDir = waspProjectDir </> Common.extCodeDirInWaspProjectDir
  liftIO $ do
    createDirectorySP extCodeDir
    dataDir <- Wasp.Data.getAbsDataDirPath

    let copyTemplateFile' = copyTemplateFile dataDir extCodeDir

    writeFileSP (extCodeDir </> waspignoreFileInExtCodeDir) waspignoreFileContent

    copyTemplateFile'
      [relfile|new/ext/MainPage.js|]
      mainPageJsFileInExtCodeDir

    copyTemplateFile'
      [relfile|new/ext/Main.css|]
      mainCssFileInExtCodeDir

    copyTemplateFile'
      [relfile|new/ext/waspLogo.png|]
      waspLogoFileInExtCodeDir

  liftIO $ do
    putStrLn $ Term.applyStyles [Term.Green] ("Created new Wasp app in ./" ++ projectName ++ " directory!")
    putStrLn "To run it, do:"
    putStrLn ""
    putStrLn $ Term.applyStyles [Term.Bold] ("    cd " ++ projectName)
    putStrLn $ Term.applyStyles [Term.Bold] "    wasp start"
    putStrLn ""
    putStrLn Command.Common.alphaWarningMessage
  where
    copyTemplateFile ::
      Path' Abs (Dir Wasp.Data.DataDir) ->
      Path' Abs (Dir SourceExternalCodeDir) ->
      Path' (Rel Common.CliTemplatesDir) File' ->
      Path' (Rel SourceExternalCodeDir) File' ->
      IO ()
    copyTemplateFile dataDir extCodeDir srcTmplFile dstExtDirFile =
      System.Directory.copyFile
        (SP.fromAbsFile (dataDir </> cliTemplatesDirInDataDir </> srcTmplFile))
        (SP.fromAbsFile (extCodeDir </> dstExtDirFile))

    cliTemplatesDirInDataDir :: Path' (Rel Wasp.Data.DataDir) (Dir Common.CliTemplatesDir)
    cliTemplatesDirInDataDir = [reldir|Cli/templates|]

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
          "/.env"
        ]

    waspignoreFileInExtCodeDir :: Path' (Rel SourceExternalCodeDir) File'
    waspignoreFileInExtCodeDir = [relfile|.waspignore|]

    waspignoreFileContent =
      unlines
        [ "# Ignore editor tmp files",
          "**/*~",
          "**/#*#"
        ]

    mainPageJsFileInExtCodeDir :: Path' (Rel SourceExternalCodeDir) File'
    mainPageJsFileInExtCodeDir = [relfile|MainPage.js|]

    mainCssFileInExtCodeDir :: Path' (Rel SourceExternalCodeDir) File'
    mainCssFileInExtCodeDir = [relfile|Main.css|]

    waspLogoFileInExtCodeDir :: Path' (Rel SourceExternalCodeDir) File'
    waspLogoFileInExtCodeDir = [relfile|waspLogo.png|]

    writeFileSP = writeFile . SP.fromAbsFile
    createDirectorySP = createDirectory . SP.fromAbsDir
