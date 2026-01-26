module Wasp.Cli.Command.Uninstall
  ( uninstall,
  )
where

import Control.Monad (filterM, unless, when)
import Control.Monad.IO.Class (liftIO)
import StrongPath (Abs, Dir', File', Path', (</>))
import qualified StrongPath as SP
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
import Wasp.Message (Message)
import qualified Wasp.Message as Msg
import Wasp.Util (indent)
import Wasp.Util.IO
  ( deleteDirectoryIfExists,
    deleteFileIfExists,
    doesDirectoryExist,
    doesFileExist,
  )
import Wasp.Util.InstallMethod (CliInstallMethod (..), npmPackageName, waspCliInstallMethod)

-- | Removes Wasp from the system.
uninstall :: Command ()
uninstall =
  case waspCliInstallMethod of
    BinaryInstaller -> installerUninstall
    NpmPackage -> npmUninstall

installerUninstall :: Command ()
installerUninstall = do
  cliSendMessageC $ Msg.Start "Uninstalling Wasp..."
  liftIO removeWaspFiles
  cliSendMessageC $ Msg.Success "Uninstalled Wasp."
  cliSendMessageC dockerVolumeMsg

npmUninstall :: Command ()
npmUninstall = do
  cliSendMessageC $ Msg.Start "Removing Wasp data..."
  liftIO removeWaspFiles
  cliSendMessageC $ Msg.Success "Removed Wasp data."
  cliSendMessageC $ Msg.Info $ "To uninstall the Wasp CLI, please run 'npm uninstall -g " ++ npmPackageName ++ "'."
  cliSendMessageC $ Msg.Info ""
  cliSendMessageC dockerVolumeMsg

dockerVolumeMsg :: Message
dockerVolumeMsg =
  Msg.Info $
    "If you have used Wasp to run dev database for you, you might want to make sure you also"
      <> " deleted all the docker volumes it might have created."
      <> (" You can easily list them by doing `docker volume ls | grep " <> waspDevDbDockerVolumePrefix <> "`.")

removeWaspFiles :: IO ()
removeWaspFiles = do
  dirsToRemove <- filterM doesDirectoryExist =<< getWaspDirectories
  filesToRemove <- filterM doesFileExist =<< getWaspFiles

  let allPathsToRemove =
        (SP.fromAbsDir <$> dirsToRemove)
          ++ (SP.fromAbsFile <$> filesToRemove)

  unless (null allPathsToRemove) $ do
    putStr $
      unlines
        [ "We will remove the following files and directories:",
          indent 2 $ unlines allPathsToRemove,
          "Are you sure you want to continue? [y/N]"
        ]

    answer <- getLine
    when (answer /= "y") $ die "Aborted."

    mapM_ deleteDirectoryIfExists dirsToRemove
    mapM_ deleteFileIfExists filesToRemove

getWaspDirectories :: IO [Path' Abs Dir']
getWaspDirectories = do
  homeDir <- getHomeDir
  userCacheDir <- getUserCacheDir

  return
    [ homeDir </> waspInstallationDirInHomeDir,
      SP.castDir $ getWaspCacheDir userCacheDir
    ]

getWaspFiles :: IO [Path' Abs File']
getWaspFiles = do
  homeDir <- getHomeDir

  return [homeDir </> waspExecutableInHomeDir]
