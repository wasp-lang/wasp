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
import Wasp.Cli.Command (Command, makeCommandError)
import Wasp.Cli.Command.Message (cliSendMessageC)
import qualified Wasp.Diagnostic as Diagnostic
import Wasp.Diagnostic.ErrorId (ErrorId (WaspExecutableNotFound))
import qualified Wasp.Message as Msg
import qualified Wasp.Util.IO as IOUtil

throwIfExeIsNotAvailable :: String -> String -> Command ()
throwIfExeIsNotAvailable exeName explanationMsg = do
  liftIO (findExecutable exeName) >>= \case
    Just _ -> return ()
    Nothing ->
      E.throwError $
        makeCommandError WaspExecutableNotFound ("Couldn't find `" <> exeName <> "` executable") explanationMsg

deleteDirectoryIfExistsVerbosely :: Path' Abs (Dir d) -> Command ()
deleteDirectoryIfExistsVerbosely dir = do
  cliSendMessageC $ Msg.Start $ "Deleting the " ++ dirName ++ " directory..."
  dirExist <- liftIO $ IOUtil.doesDirectoryExist dir
  if dirExist
    then do
      liftIO $ IOUtil.removeDirectory dir
      cliSendMessageC $ Msg.Diagnostic $ Diagnostic.makeSuccess $ "Deleted the " ++ dirName ++ " directory."
    else do
      cliSendMessageC $ Msg.Diagnostic $ Diagnostic.makeSuccess $ "Nothing to delete: The " ++ dirName ++ " directory does not exist."
  where
    dirName = SP.toFilePath $ basename dir
