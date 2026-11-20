module Wasp.Cli.Command.Common
  ( throwIfExeIsNotAvailable,
    deleteDirectoryIfExistsVerbosely,
    runTaskOrAbort,
  )
where

import qualified Control.Monad.Except as E
import Control.Monad.IO.Class (liftIO)
import StrongPath (Abs, Dir, Path')
import qualified StrongPath as SP
import StrongPath.Operations
import System.Directory (findExecutable)
import System.IO.Error (tryIOError)
import Wasp.Cli.Command (Command, CommandError (..))
import Wasp.Cli.Command.Message (cliSendMessageC)
import Wasp.Cli.Message (cliSendMessage)
import qualified Wasp.Message as Msg
import qualified Wasp.Util.IO as IOUtil
import Wasp.Util.TaskStatus (runTaskWithStatusReport)
import Wasp.Util.Terminal (styleCode, styleLiteral)

throwIfExeIsNotAvailable :: String -> String -> Command ()
throwIfExeIsNotAvailable exeName explanationMsg = do
  liftIO (findExecutable exeName) >>= \case
    Just _ -> return ()
    Nothing ->
      E.throwError $
        CommandError ("Couldn't find " <> styleCode exeName <> " executable") explanationMsg

deleteDirectoryIfExistsVerbosely :: Path' Abs (Dir d) -> Command ()
deleteDirectoryIfExistsVerbosely dir = do
  dirExists <- liftIO $ IOUtil.doesDirectoryExist dir
  if dirExists
    then
      runTaskOrAbort
        ("Deleting the " ++ dirName ++ " directory")
        ("Deleted the " ++ dirName ++ " directory")
        ("Failed to delete the " ++ dirName ++ " directory")
        (either (Left . (: []) . Msg.Text . show) Right <$> tryIOError (IOUtil.removeDirectory dir))
    else cliSendMessageC $ Msg.Success $ "The " ++ dirName ++ " directory is already gone"
  where
    dirName = styleLiteral $ SP.toFilePath $ basename dir

-- | The task reports its own failure, so the command only has to stop.
runTaskOrAbort :: String -> String -> String -> IO (Either [Msg.Part] a) -> Command a
runTaskOrAbort runningMessage successMessage failureMessage task =
  liftIO (runTaskWithStatusReport cliSendMessage runningMessage successMessage failureMessage task)
    >>= either (const $ E.throwError CommandAborted) return
