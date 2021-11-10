module Command.CreateNewProject
  ( createNewProject,
  )
where

import qualified Cli.Common as Common
import Command (Command, CommandError (..))
import qualified Command.Common
import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import Data.Char (isLetter)
import StrongPath (Abs, Dir, File', Path', Rel, reldir, relfile, (</>))
import qualified StrongPath as SP
import System.Directory (createDirectory, getCurrentDirectory)
import qualified System.Directory
import qualified System.FilePath as FP
import Text.Printf (printf)
import Wasp.AppSpec.ExternalCode (SourceExternalCodeDir)
import qualified Wasp.Data
import qualified Wasp.Util.Terminal as Term

newtype ProjectName = ProjectName {_projectName :: String}

createNewProject :: String -> Command ()
createNewProject projectNameStr = do
  case parseProjectName projectNameStr of
    Left err -> throwError $ CommandError err
    Right projectName -> createNewProject' projectName
  where
    parseProjectName name =
      if all isLetter name
        then Right $ ProjectName name
        else Left "Please use only letters for project name."

createNewProject' :: ProjectName -> Command ()
createNewProject' (ProjectName projectName) = do
  absCwd <- liftIO getCurrentDirectory
  waspProjectDir <- case SP.parseAbsDir $ absCwd FP.</> projectName of
    Left err ->
      throwError $
        CommandError
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
        [ "app %s {" `printf` projectName,
          "  title: \"%s\"" `printf` projectName,
          "}",
          "",
          "route \"/\" -> page Main",
          "page Main {",
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
