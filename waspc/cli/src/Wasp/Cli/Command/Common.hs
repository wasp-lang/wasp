module Wasp.Cli.Command.Common
  ( throwIfExeIsNotAvailable,
    deleteDirectoryIfExistsVerbosely,
  )
where

import Control.Monad (when)
import qualified Control.Monad.Except as E
import Control.Monad.IO.Class (liftIO)
import StrongPath (Abs, Dir, Path')
import qualified StrongPath as SP
import StrongPath.Operations
import System.Directory (findExecutable)
import Wasp.Cli.Command (Command, commandError)
import Wasp.Cli.Command.Message (runTaskC)
import qualified Wasp.Util.IO as IOUtil
import Wasp.Util.TextStyle (styleCode, styleLiteral)

throwIfExeIsNotAvailable :: String -> String -> Command ()
throwIfExeIsNotAvailable exeName explanationMsg = do
  liftIO (findExecutable exeName) >>= \case
    Just _ -> return ()
    Nothing ->
      E.throwError $
        commandError ("Couldn't find the " <> styleCode exeName <> " executable") explanationMsg

deleteDirectoryIfExistsVerbosely :: Path' Abs (Dir d) -> Command ()
deleteDirectoryIfExistsVerbosely dir = do
  dirExist <- liftIO $ IOUtil.doesDirectoryExist dir
  runTaskC ("Deleting the " ++ dirName ++ " directory") (successText dirExist)
    $ when dirExist
    $ liftIO
    $ IOUtil.removeDirectory dir
  where
    successText dirExist
      | dirExist = "Deleted the " ++ dirName ++ " directory"
      | otherwise = "The " ++ dirName ++ " directory is already gone"

    dirName = styleLiteral $ SP.toFilePath $ basename dir
