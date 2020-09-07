module Command.CreateNewProject
    ( createNewProject
    ) where

import System.Directory (createDirectory, getCurrentDirectory)
import qualified System.FilePath as FP
import qualified Path as P
import Text.Printf (printf)
import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)

import StrongPath (Path, Rel, File, (</>))
import qualified StrongPath as SP
import ExternalCode (SourceExternalCodeDir)
import Command (Command, CommandError(..))
import qualified Common


createNewProject :: String -> Command ()
createNewProject projectName = do
    absCwd <- liftIO getCurrentDirectory
    waspProjectDir <- case SP.parseAbsDir $ absCwd FP.</> projectName of
        Left err -> throwError $ CommandError ("Failed to parse absolute path to wasp project dir: " ++ show err)
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
        writeFileSP (extCodeDir </> mainPageJsFileInExtCodeDir) mainPageJsFileContent

    liftIO $ putStrLn $ "Created new Wasp project in ./" ++ projectName ++ " directory!"
  where
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
      mainPageJsFileContent = unlines
          [ "import React from 'react'"
          , ""
          , "const MainPage = () => {"
          , "  return <p>Hello world!</p>"
          , "}"
          , ""
          , "export default MainPage"
          ]

      writeFileSP = writeFile . SP.toFilePath
      createDirectorySP = createDirectory . SP.toFilePath
