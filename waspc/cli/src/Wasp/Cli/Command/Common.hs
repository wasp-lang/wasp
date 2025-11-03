module Wasp.Cli.Command.Common
  ( readWaspCompileInfo,
    throwIfExeIsNotAvailable,
    deleteDirectoryIfExistsVerbosely,
  )
where

import qualified Control.Monad.Except as E
import Control.Monad.IO.Class (liftIO)
import StrongPath (Abs, Dir, Path')
import qualified StrongPath as SP
import StrongPath.Operations
import System.Directory
  ( findExecutable,
  )
import Wasp.Cli.Command (Command, CommandError (..))
import Wasp.Cli.Command.Message (cliSendMessageC)
import qualified Wasp.Message as Msg
import Wasp.Project (WaspProjectDir)
import qualified Wasp.Project.Common as Project.Common
import Wasp.Util (ifM)
import qualified Wasp.Util.IO as IOUtil

readWaspCompileInfo :: Path' Abs (Dir WaspProjectDir) -> IO String
readWaspCompileInfo waspDir =
  ifM
    (IOUtil.doesFileExist dotWaspInfoFile)
    (IOUtil.readFile dotWaspInfoFile)
    (return "No compile information found")
  where
    dotWaspInfoFile =
      waspDir
        </> Project.Common.dotWaspDirInWaspProjectDir
        </> Project.Common.generatedCodeDirInDotWaspDir
        </> Project.Common.dotWaspInfoFileInGeneratedCodeDir

throwIfExeIsNotAvailable :: String -> String -> Command ()
throwIfExeIsNotAvailable exeName explanationMsg = do
  liftIO (findExecutable exeName) >>= \case
    Just _ -> return ()
    Nothing ->
      E.throwError $
        CommandError ("Couldn't find `" <> exeName <> "` executable") explanationMsg

deleteDirectoryIfExistsVerbosely :: Path' Abs (Dir d) -> Command ()
deleteDirectoryIfExistsVerbosely dir = do
  cliSendMessageC $ Msg.Start $ "Deleting the " ++ dirName ++ " directory..."
  dirExist <- liftIO $ IOUtil.doesDirectoryExist dir
  if dirExist
    then do
      liftIO $ IOUtil.removeDirectory dir
      cliSendMessageC $ Msg.Success $ "Deleted the " ++ dirName ++ " directory."
    else do
      cliSendMessageC $ Msg.Success $ "Nothing to delete: The " ++ dirName ++ " directory does not exist."
  where
    dirName = SP.toFilePath $ basename dir
