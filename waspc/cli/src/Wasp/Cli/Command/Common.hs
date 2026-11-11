module Wasp.Cli.Command.Common
  ( throwIfExeIsNotAvailable,
    deleteDirectoryIfExistsVerbosely,
  )
where

import qualified Control.Monad.Except as E
import Control.Monad.IO.Class (liftIO)
import StrongPath (Abs, Dir, Path')
import qualified StrongPath as SP
import StrongPath.Operations
import System.Directory (findExecutable)
import System.Exit (exitFailure)
import System.IO.Error (tryIOError)
import Wasp.Cli.Command (Command, CommandError (..))
import Wasp.Cli.Command.Message (cliSendMessageC)
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
      liftIO $
        runTaskWithStatusReport
          ("Deleting the " ++ dirName ++ " directory")
          ("Deleted the " ++ dirName ++ " directory")
          ("Failed to delete the " ++ dirName ++ " directory")
          (either (Left . show) Right <$> tryIOError (IOUtil.removeDirectory dir))
          >>= either (const exitFailure) return
    else cliSendMessageC $ Msg.Success $ "The " ++ dirName ++ " directory is already gone"
  where
    dirName = styleLiteral $ SP.toFilePath $ basename dir
