module Command.CreateNewProject
    ( createNewProject
    ) where

import qualified System.Directory
import           Control.Monad.Except   (throwError)
import           Control.Monad.IO.Class (liftIO)
import qualified Path                   as P
import           System.Directory       (createDirectory, getCurrentDirectory)
import qualified System.FilePath        as FP
import           Text.Printf            (printf)

import           Command                (Command, CommandError (..))
import qualified Common
import           ExternalCode           (SourceExternalCodeDir)
import           StrongPath             (File, Path, Rel, (</>), Abs, Dir)
import qualified StrongPath             as SP
import qualified Util.Terminal          as Term

-- TODO(matija): is it ok to import this from here, should we extract it somewhere?
import Generator.Templates (DataDir)

import qualified Paths_waspc

-- Should this be defined somewhere else?
data CliTemplatesDir

getAbsDataDirPath :: IO (Path Abs (Dir DataDir))
getAbsDataDirPath = Paths_waspc.getDataDir >>= SP.parseAbsDir

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
        dataDir <- getAbsDataDirPath

        -- Scaffold MainPage.js
        System.Directory.copyFile
            (SP.toFilePath (dataDir </> cliTemplatesDirInDataDir </>
                            SP.fromPathRelFile [P.relfile|new/ext/MainPage.js|]))
            (SP.toFilePath (extCodeDir </> mainPageJsFileInExtCodeDir))

        -- Scaffold Main.css
        System.Directory.copyFile
            (SP.toFilePath (dataDir </> cliTemplatesDirInDataDir </>
                            SP.fromPathRelFile [P.relfile|new/ext/Main.css|]))
            (SP.toFilePath (extCodeDir </> mainCssFileInExtCodeDir))

        -- Scaffold waspLogo.png
        System.Directory.copyFile
            (SP.toFilePath (dataDir </> cliTemplatesDirInDataDir </>
                            SP.fromPathRelFile [P.relfile|new/ext/waspLogo.png|]))
            (SP.toFilePath (extCodeDir </> waspLogoFileInExtCodeDir))

    liftIO $ do
        putStrLn $ Term.applyStyles [Term.Green] ("Created new Wasp app in ./" ++ projectName ++ " directory!")
        putStrLn "To run it, do:"
        putStrLn ""
        putStrLn $ Term.applyStyles [Term.Bold] ("    cd " ++ projectName)
        putStrLn $ Term.applyStyles [Term.Bold] "    wasp start"
        putStrLn ""
  where
      cliTemplatesDirInDataDir :: Path (Rel DataDir) (Dir CliTemplatesDir)
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
          ]

      mainPageJsFileInExtCodeDir :: Path (Rel SourceExternalCodeDir) File
      mainPageJsFileInExtCodeDir = SP.fromPathRelFile [P.relfile|MainPage.js|]

      mainCssFileInExtCodeDir :: Path (Rel SourceExternalCodeDir) File
      mainCssFileInExtCodeDir = SP.fromPathRelFile [P.relfile|Main.css|]
      
      waspLogoFileInExtCodeDir :: Path (Rel SourceExternalCodeDir) File
      waspLogoFileInExtCodeDir = SP.fromPathRelFile [P.relfile|waspLogo.png|]

      writeFileSP = writeFile . SP.toFilePath
      createDirectorySP = createDirectory . SP.toFilePath
