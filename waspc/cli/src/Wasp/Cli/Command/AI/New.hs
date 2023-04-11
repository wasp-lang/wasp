module Wasp.Cli.Command.AI.New
  ( new,
  )
where

import Control.Arrow ()
import Control.Monad.Except (MonadIO (liftIO))
import Data.Text (Text)
import StrongPath (Abs, Dir, Path', fromAbsDir, fromRelFile, relfile, (</>))
import StrongPath.Operations ()
import System.Directory (getFileSize, setCurrentDirectory)
import qualified Wasp.AppSpec.Valid as ASV
import Wasp.Cli.Command (Command)
import Wasp.Cli.Command.Common (findWaspProjectRootDirFromCwd, readWaspCompileInfo)
import Wasp.Cli.Command.Compile (analyze)
import Wasp.Cli.Command.CreateNewProject (createNewProject)
import qualified Wasp.Cli.Command.CreateNewProject as CNP
import Wasp.Cli.Command.Message (cliSendMessageC)
import Wasp.Cli.Command.Start.Db (getDbSystem)
import qualified Wasp.Message as Msg
import Wasp.Project (WaspProjectDir)
import Wasp.Util.IO (removeFile)
import qualified Wasp.Util.IO as IOUtil
import qualified Wasp.Util.Terminal as Term

new :: Command ()
new = do
  (webAppTitle, webAppDescription) <- liftIO $ do
    putStrLn "Describe the web app you want to create:"
    putStrLn "Title:"
    title <- getLine
    putStrLn "What it should work like:"
    desc <- getLine
    return (title, desc)

  absWaspProjectDir <- createNewEmptyProject webAppTitle
  liftIO $ setCurrentDirectory $ fromAbsDir absWaspProjectDir

  waspFileContent <- aiWriteWaspFile absWaspProjectDir webAppTitle webAppDescription

  aiWriteWaspPages absWaspProjectDir waspFileContent
  aiWriteWaspOperations absWaspProjectDir waspFileContent
  -- Maybe write something else also: setupFn or something.

  return ()
  where
    -- appSpec <- analyze waspDir
    -- let (appName, app) = ASV.getApp appSpec

    createNewEmptyProject :: String -> Command (Path' Abs (Dir WaspProjectDir))
    createNewEmptyProject webAppTitle = do
      projectInfo <- CNP.parseProjectInfo webAppTitle
      CNP.createWaspProjectDir projectInfo
      absWaspProjectDir <- CNP.getAbsoluteWaspProjectDir projectInfo
      -- Delete existing source files that we generate in the new project.
      -- TODO: Instead of deleting files, I should instead have a function that generates
      --   the very basic skeleton for the Wasp app, and then the normal "new app" would
      --   just add files to it.
      liftIO $ do
        removeFile $ absWaspProjectDir </> [relfile|main.wasp|]
        removeFile $ absWaspProjectDir </> [relfile|src/client/Main.css|]
        removeFile $ absWaspProjectDir </> [relfile|src/client/MainPage.jsx|]
        removeFile $ absWaspProjectDir </> [relfile|src/client/waspLogo.png|]
      return absWaspProjectDir

    -- Writes wasp file to disk, but also returns its content.
    -- TODO: Also check if it compiles and if not, send errors to GPT.
    aiWriteWaspFile :: Path' Abs (Dir WaspProjectDir) -> String -> String -> Command Text
    aiWriteWaspFile absWaspProjectDir appTitle appDesc = do
      error "TODO"

    aiWriteWaspPages :: Path' Abs (Dir WaspProjectDir) -> Text -> Command ()
    aiWriteWaspPages absWaspProjectDir waspFileContent = do
      error "TODO"

    aiWriteWaspOperations :: Path' Abs (Dir WaspProjectDir) -> Text -> Command ()
    aiWriteWaspOperations absWaspProjectDir waspFileContent = do
      error "TODO"

sendPromptToGpt :: String -> IO ()
sendPromptToGpt prompt = do
  -- TODO: https://platform.openai.com/docs/guides/chat/introduction
  -- TODO: https://www.reddit.com/r/haskell/comments/1263sfl/haskellai_stable_package_set_shell_and/ -> also mentions openai-hs and a fork of it which is more up to date it seems.
  undefined
