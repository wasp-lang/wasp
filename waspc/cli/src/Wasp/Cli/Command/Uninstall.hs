module Wasp.Cli.Command.Uninstall
  ( uninstall,
  )
where

import Control.Monad (filterM, unless)
import Control.Monad.IO.Class (liftIO)
import qualified Options.Applicative as Opt
import StrongPath (Abs, Dir', File', Path', (</>))
import qualified StrongPath as SP
import System.Exit (die)
import Wasp.Cli.Command (Command)
import Wasp.Cli.Command.Call (Arguments)
import Wasp.Cli.Command.Message (cliSendMessageC)
import Wasp.Cli.FileSystem
  ( getHomeDir,
    getUserCacheDir,
    getWaspCacheDir,
    waspExecutableInHomeDir,
    waspInstallationDirInHomeDir,
  )
import Wasp.Cli.Interactive (NonInteractiveHint (NonInteractiveHint))
import qualified Wasp.Cli.Interactive as Interactive
import Wasp.Cli.Util.Parser (ArgsParser (..), getParserHelpMessage, withArguments)
import Wasp.Message (Message)
import qualified Wasp.Message as Msg
import Wasp.Project.Db.Dev.Postgres (waspDevDbDockerVolumePrefix)
import Wasp.Util (indent)
import Wasp.Util.IO
  ( deleteDirectoryIfExists,
    deleteFileIfExists,
    doesDirectoryExist,
    doesFileExist,
  )
import Wasp.Util.InstallMethod (uninstallationCommand)

-- | Removes Wasp from the system.
uninstall :: Arguments -> Command ()
uninstall = withArguments uninstallArgsParser $ \UninstallArgs {force = skipConfirmation} -> do
  waspPathsToRemove <- liftIO findWaspPathsToRemove

  if null $ toFilePaths waspPathsToRemove
    then cliSendMessageC $ Msg.Info "No Wasp data was found on this system."
    else do
      cliSendMessageC $ Msg.Start "Removing Wasp data..."
      liftIO $ removeWaspPaths skipConfirmation waspPathsToRemove
      cliSendMessageC $ Msg.Success "Removed Wasp data."

  cliSendMessageC $ Msg.Info ""
  cliSendMessageC $ Msg.Info "To uninstall the Wasp CLI, please run:"
  cliSendMessageC $ Msg.Info $ indent 2 uninstallationCommand
  cliSendMessageC $ Msg.Info ""
  cliSendMessageC dockerVolumeMsg

newtype UninstallArgs = UninstallArgs
  { force :: Bool
  }

nonInteractiveHint :: NonInteractiveHint
nonInteractiveHint = NonInteractiveHint $ getParserHelpMessage uninstallArgsParser

uninstallArgsParser :: ArgsParser UninstallArgs
uninstallArgsParser =
  ArgsParser "wasp uninstall" $
    UninstallArgs
      <$> Opt.switch
        ( Opt.long "force"
            <> Opt.help "Skip the confirmation prompt"
        )

dockerVolumeMsg :: Message
dockerVolumeMsg =
  Msg.Info $
    "If you have used Wasp to run dev database for you, you might want to make sure you also"
      <> " deleted all the docker volumes it might have created."
      <> (" You can easily list them by doing `docker volume ls | grep " <> waspDevDbDockerVolumePrefix <> "`.")

type PathsToRemove = ([Path' Abs Dir'], [Path' Abs File'])

findWaspPathsToRemove :: IO PathsToRemove
findWaspPathsToRemove =
  (,)
    <$> (filterM doesDirectoryExist =<< getWaspDirectories)
    <*> (filterM doesFileExist =<< getWaspFiles)
  where
    getWaspDirectories = do
      homeDir <- getHomeDir
      userCacheDir <- getUserCacheDir
      return
        [ homeDir </> waspInstallationDirInHomeDir,
          SP.castDir $ getWaspCacheDir userCacheDir
        ]

    getWaspFiles = do
      homeDir <- getHomeDir
      return
        [ homeDir </> waspExecutableInHomeDir
        ]

removeWaspPaths :: Bool -> PathsToRemove -> IO ()
removeWaspPaths skipConfirmation pathsToRemove = do
  putStr $
    unlines
      [ "We will remove the following files and directories:",
        indent 2 $ unlines $ toFilePaths pathsToRemove
      ]

  unless skipConfirmation $ do
    confirmed <- Interactive.askForConfirmation "Are you sure you want to continue?" nonInteractiveHint
    unless confirmed $ die "Aborted."

  mapM_ deleteDirectoryIfExists $ fst pathsToRemove
  mapM_ deleteFileIfExists $ snd pathsToRemove

toFilePaths :: PathsToRemove -> [FilePath]
toFilePaths (dirs, files) =
  (SP.fromAbsDir <$> dirs) ++ (SP.fromAbsFile <$> files)
