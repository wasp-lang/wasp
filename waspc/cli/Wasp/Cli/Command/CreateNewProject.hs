{-# LANGUAGE ViewPatterns #-}

module Wasp.Cli.Command.CreateNewProject
  ( createNewProject,
  )
where

import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import Data.Char (isLetter)
import Data.List (intercalate)
import StrongPath (Abs, Dir, File', Path', Rel, reldir, relfile, (</>))
import qualified StrongPath as SP
import System.Directory (createDirectory, getCurrentDirectory)
import qualified System.Directory
import qualified System.FilePath as FP
import Text.Printf (printf)
import Wasp.AppSpec.ExternalCode (SourceExternalCodeDir)
import Wasp.Cli.Command (Command, CommandError (..))
import qualified Wasp.Cli.Command.Common as Command.Common
import qualified Wasp.Cli.Common as Common
import qualified Wasp.Data
import Wasp.Lexer (reservedNames)
import qualified Wasp.Util.Terminal as Term

newtype ProjectName = ProjectName {_projectName :: String}

createNewProject :: String -> Command ()
createNewProject (all isLetter -> False) =
  throwError $ CommandError "Project creation failed" "Please use only letters for a new project's name."
createNewProject ((`elem` reservedNames) -> True) =
  throwError $
    CommandError
      "Project creation failed"
      ( "Please pick a project name not one of these reserved words:\n\t" ++ intercalate "\n\t" reservedNames
      )
createNewProject name = createNewProject' (ProjectName name)

createNewProject' :: ProjectName -> Command ()
createNewProject' (ProjectName projectName) = do
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
        [ "app %s {" `printf` projectName,
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
