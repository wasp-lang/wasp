module Wasp.Cli.Command.Uninstall
  ( uninstall,
  )
where

import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import StrongPath (fromAbsDir, fromAbsFile, (</>))
import System.Exit (die)
import Wasp.Cli.Command (Command)
import Wasp.Cli.Command.Message (cliSendMessageC)
import Wasp.Cli.Command.Start.Db (waspDevDbDockerVolumePrefix)
import Wasp.Cli.FileSystem
  ( getHomeDir,
    getUserCacheDir,
    getWaspCacheDir,
    waspExecutableInHomeDir,
    waspInstallationDirInHomeDir,
  )
import qualified Wasp.Message as Msg
import Wasp.Util.IO (deleteDirectoryIfExists, deleteFileIfExists)

-- | Removes Wasp from the system.
uninstall :: Command ()
uninstall = do
  cliSendMessageC $ Msg.Start "Uninstalling Wasp ..."
  liftIO removeWaspFiles
  cliSendMessageC $ Msg.Success "Uninstalled Wasp."
  cliSendMessageC $
    Msg.Info $
      "If you have used Wasp to run dev database for you, you might want to make sure you also"
        <> " deleted all the docker volumes it might have created."
        <> (" You can easily list them by doing `docker volume ls | grep " <> waspDevDbDockerVolumePrefix <> "`.")

removeWaspFiles :: IO ()
removeWaspFiles = do
  homeDir <- getHomeDir
  userCacheDir <- getUserCacheDir

  let waspInstallationDir = homeDir </> waspInstallationDirInHomeDir
      waspExecutableFile = homeDir </> waspExecutableInHomeDir
      waspCacheDir = getWaspCacheDir userCacheDir

  putStr $
    unlines
      [ "We will remove the following directories:",
        "  " ++ fromAbsDir waspInstallationDir,
        "  " ++ fromAbsDir waspCacheDir,
        "",
        "We will also remove the following files:",
        "  " ++ fromAbsFile waspExecutableFile,
        "",
        "Are you sure you want to continue? [y/N]"
      ]

  answer <- getLine
  when (answer /= "y") $ die "Aborted."
  deleteDirectoryIfExists waspInstallationDir
  deleteFileIfExists waspExecutableFile
  deleteDirectoryIfExists waspCacheDir
