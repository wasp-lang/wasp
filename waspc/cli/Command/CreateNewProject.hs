module Command.CreateNewProject
    ( createNewProject
    ) where

import           Control.Monad.Except   (throwError)
import           Control.Monad.IO.Class (liftIO)
import qualified Path                   as P
import           System.Directory       (createDirectory, getCurrentDirectory)
import qualified System.FilePath        as FP
import           Text.Printf            (printf)

import           Command                (Command, CommandError (..))
import qualified Common
import           ExternalCode           (SourceExternalCodeDir)
import           StrongPath             (File, Path, Rel, (</>))
import qualified StrongPath             as SP
import qualified Util.Terminal          as Term


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

    liftIO $ do
        putStrLn $ Term.applyStyles [Term.Green] $ "Created new Wasp project in ./" ++ projectName ++ " directory!"
        putStrLn $ "Move into created directory and type '"
            ++ (Term.applyStyles [Term.Bold] "wasp start")
            ++ "' to run the app."
        putStrLn ""
        putStrLn "Check the docs for any further information: https://wasp-lang.github.io/web/docs."
        putStrLn $ "If you are new to Wasp, we recommend starting with Todo App tutorial:"
            ++ " https://wasp-lang.github.io/web/docs/tutorials/todo-app."

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
