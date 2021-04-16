module Command.CreateNewProject
    ( createNewProject
    ) where

import           Control.Monad.Except   (throwError)
import           Control.Monad.IO.Class (liftIO)
import qualified Path                   as P
import           System.Directory       (createDirectory, getCurrentDirectory)
import qualified System.Directory
import qualified System.FilePath        as FP
import           Text.Printf            (printf)

import qualified Cli.Common             as Common
import           Command                (Command, CommandError (..))
import qualified Command.Common
import qualified Data
import           ExternalCode           (SourceExternalCodeDir)
import           StrongPath             (Abs, Dir, File, Path, Rel, (</>))
import qualified StrongPath             as SP
import qualified Util.Terminal          as Term


createNewProject :: String -> Command ()
createNewProject projectName = do
    absCwd <- liftIO getCurrentDirectory
    waspProjectDir <- case SP.parseAbsDir $ absCwd FP.</> projectName of
        Left err -> throwError $ CommandError ("Failed to parse absolute path to wasp project dir: "
                                               ++ show err)
        Right sp -> return sp
    liftIO $ do
        createDirectorySP waspProjectDir
        writeFileSP (waspProjectDir </> mainWaspFileInWaspProjectDir) mainWaspFileContent
        writeFileSP (waspProjectDir </> gitignoreFileInWaspProjectDir) gitignoreFileContent
        writeFileSP (waspProjectDir </> Common.dotWaspRootFileInWaspProjectDir)
            "File marking the root of Wasp project."

    let extCodeDir = waspProjectDir </> Common.extCodeDirInWaspProjectDir
    liftIO $ do
        createDirectorySP extCodeDir
        dataDir <- Data.getAbsDataDirPath

        let copyTemplateFile' = copyTemplateFile dataDir extCodeDir

        writeFileSP (extCodeDir </> waspignoreFileInExtCodeDir) waspignoreFileContent

        copyTemplateFile'
            (SP.fromPathRelFile [P.relfile|new/ext/MainPage.js|])
            mainPageJsFileInExtCodeDir

        copyTemplateFile'
            (SP.fromPathRelFile [P.relfile|new/ext/Main.css|])
            mainCssFileInExtCodeDir

        copyTemplateFile'
            (SP.fromPathRelFile [P.relfile|new/ext/waspLogo.png|])
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
      copyTemplateFile
          :: Path Abs (Dir Data.DataDir)
          -> Path Abs (Dir SourceExternalCodeDir)
          -> Path (Rel Common.CliTemplatesDir) File
          -> Path (Rel SourceExternalCodeDir) File
          -> IO ()
      copyTemplateFile dataDir extCodeDir srcTmplFile dstExtDirFile = System.Directory.copyFile
          (SP.toFilePath (dataDir </> cliTemplatesDirInDataDir </> srcTmplFile))
          (SP.toFilePath (extCodeDir </> dstExtDirFile))

      cliTemplatesDirInDataDir :: Path (Rel Data.DataDir) (Dir Common.CliTemplatesDir)
      cliTemplatesDirInDataDir = SP.fromPathRelDir [P.reldir|Cli/templates|]

      mainWaspFileInWaspProjectDir :: Path (Rel Common.WaspProjectDir) File
      mainWaspFileInWaspProjectDir = SP.fromPathRelFile [P.relfile|main.wasp|]

      mainWaspFileContent = unlines
          [ "app %s {" `printf` projectName
          , "  title: \"%s\"" `printf` projectName
          , "}"
          , ""
          , "route \"/\" -> page Main"
          , "page Main {"
          , "  component: import Main from \"@ext/MainPage.js\""
          , "}"
          ]

      gitignoreFileInWaspProjectDir :: Path (Rel Common.WaspProjectDir) File
      gitignoreFileInWaspProjectDir = SP.fromPathRelFile [P.relfile|.gitignore|]

      gitignoreFileContent = unlines
          [ "/.wasp/"
          , "/.env"
          ]

      waspignoreFileInExtCodeDir :: Path (Rel SourceExternalCodeDir) File
      waspignoreFileInExtCodeDir = SP.fromPathRelFile [P.relfile|.waspignore|]

      waspignoreFileContent = unlines
          [ "# Ignore editor tmp files"
          , "**/*~"
          , "**/#*#"
          ]

      mainPageJsFileInExtCodeDir :: Path (Rel SourceExternalCodeDir) File
      mainPageJsFileInExtCodeDir = SP.fromPathRelFile [P.relfile|MainPage.js|]

      mainCssFileInExtCodeDir :: Path (Rel SourceExternalCodeDir) File
      mainCssFileInExtCodeDir = SP.fromPathRelFile [P.relfile|Main.css|]

      waspLogoFileInExtCodeDir :: Path (Rel SourceExternalCodeDir) File
      waspLogoFileInExtCodeDir = SP.fromPathRelFile [P.relfile|waspLogo.png|]

      writeFileSP = writeFile . SP.toFilePath
      createDirectorySP = createDirectory . SP.toFilePath
