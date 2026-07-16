module Wasp.Cli.Command.Common
  ( readWaspCompileInfo,
    throwIfExeIsNotAvailable,
    deleteDirectoryIfExistsVerbosely,
    deleteFileIfExistsVerbosely,
    deleteDirectoryContentsVerboselyExcept,
  )
where

import Control.Monad (forM_, unless)
import qualified Control.Monad.Except as E
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as T
import NeatInterpolation (trimming)
import StrongPath (Abs, Dir, File, Path', Rel)
import qualified StrongPath as SP
import StrongPath.Operations
import System.Directory (findExecutable)
import Wasp.Cli.Command (Command, CommandError (..))
import Wasp.Cli.Command.Message (cliSendMessageC)
import qualified Wasp.Generator.WaspInfo as WI
import qualified Wasp.Message as Msg
import Wasp.Project (WaspProjectDir)
import qualified Wasp.Project.Common as Project.Common
import Wasp.Util (whenM)
import qualified Wasp.Util.IO as IOUtil

readWaspCompileInfo :: Path' Abs (Dir WaspProjectDir) -> IO String
readWaspCompileInfo waspDir =
  either showError showWaspInfo
    <$> WI.safeRead generatedAppDir
  where
    showError WI.NotFound = "No compile information found"
    showError WI.IncompatibleFormat = "Incompatible compile information"

    showWaspInfo waspInfo =
      T.unpack
        [trimming|
          ${buildType} build, generated at ${generatedAt}, by Wasp ${waspVersion}.
        |]
      where
        buildType = T.pack $ show $ WI.buildType waspInfo
        generatedAt = T.pack $ show $ WI.generatedAt waspInfo
        waspVersion = T.pack $ WI.waspVersion waspInfo

    generatedAppDir =
      waspDir
        </> Project.Common.dotWaspDirInWaspProjectDir
        </> Project.Common.generatedAppDirInDotWaspDir

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

deleteFileIfExistsVerbosely :: Path' Abs (File d) -> Command ()
deleteFileIfExistsVerbosely file = do
  cliSendMessageC $ Msg.Start $ "Deleting the " ++ fileBasename ++ " file..."
  fileExist <- liftIO $ IOUtil.doesFileExist file
  if fileExist
    then do
      liftIO $ IOUtil.removeFile file
      cliSendMessageC $ Msg.Success $ "Deleted the " ++ fileBasename ++ " file."
    else do
      cliSendMessageC $ Msg.Success $ "Nothing to delete: The " ++ fileBasename ++ " file does not exist."
  where
    fileBasename = SP.toFilePath $ basename file

deleteDirectoryContentsVerboselyExcept :: Path' Abs (Dir d) -> ([Path' (Rel d) (File a)], [Path' (Rel d) (Dir b)]) -> Command ()
deleteDirectoryContentsVerboselyExcept dirPath (filesToKeep, dirsToKeep) =
  whenM (liftIO $ IOUtil.doesDirectoryExist dirPath) $ do
    (files, dirs) <- liftIO $ IOUtil.listDirectory dirPath

    let nothingToKeep =
          filesToKeep `isNotPresentIn` files
            && dirsToKeep `isNotPresentIn` dirs

    if nothingToKeep
      then deleteDirectoryIfExistsVerbosely dirPath
      else do
        checkEachBeforeDelete deleteFileIfExistsVerbosely files filesToKeep
        checkEachBeforeDelete deleteDirectoryIfExistsVerbosely dirs dirsToKeep
  where
    checkEachBeforeDelete deleteFn items itemsToKeep =
      forM_ items $ \item ->
        unless (item `elem` itemsToKeep) $
          deleteFn (dirPath </> item)

    xs `isNotPresentIn` ys = all (`notElem` ys) xs
