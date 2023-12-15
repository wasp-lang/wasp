module Wasp.Cli.Command.Common
  ( readWaspCompileInfo,
    throwIfExeIsNotAvailable,
    deleteDotWaspDirIfExists,
    deleteDirectoryIfExists,
  )
where

import Control.Monad.Except
import qualified Control.Monad.Except as E
import StrongPath (Abs, Dir, Path')
import qualified StrongPath as SP
import StrongPath.Operations
import System.Directory
  ( findExecutable,
  )
import Wasp.Cli.Command (Command, CommandError (..))
import Wasp.Cli.Command.Message (cliSendMessageC)
import Wasp.Cli.Common (WaspProjectDir)
import qualified Wasp.Cli.Common as Cli.Common
import qualified Wasp.Message as Msg
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
      waspDir </> Cli.Common.dotWaspDirInWaspProjectDir
        </> Cli.Common.generatedCodeDirInDotWaspDir
        </> Cli.Common.dotWaspInfoFileInGeneratedCodeDir

throwIfExeIsNotAvailable :: String -> String -> Command ()
throwIfExeIsNotAvailable exeName explanationMsg = do
  liftIO (findExecutable exeName) >>= \case
    Just _ -> return ()
    Nothing ->
      E.throwError $
        CommandError ("Couldn't find `" <> exeName <> "` executable") explanationMsg

deleteDirectoryIfExists :: Path' Abs (Dir d) -> Command ()
deleteDirectoryIfExists dir = do
  cliSendMessageC $ Msg.Start $ "Deleting the " ++ dirName ++ " directory..."
  dirExist <- liftIO $ IOUtil.doesDirectoryExist dir
  if dirExist
    then deleteDir
    else -- todo(filip): do we need to report this? If not, we can simply call the function from IOUtil.
      cliSendMessageC $ Msg.Success $ "Nothing to delete: The " ++ dirName ++ " directory does not exist."
  where
    dirName = SP.toFilePath $ basename dir
    deleteDir = do
      liftIO $ IOUtil.removeDirectory dir
      cliSendMessageC $ Msg.Success $ "Deleted the " ++ dirName ++ " directory."

deleteDotWaspDirIfExists :: Path' Abs (Dir WaspProjectDir) -> Command ()
deleteDotWaspDirIfExists waspProjectDir = deleteDirectoryIfExists dotWaspDir
  where
    dotWaspDir = waspProjectDir SP.</> Cli.Common.dotWaspDirInWaspProjectDir