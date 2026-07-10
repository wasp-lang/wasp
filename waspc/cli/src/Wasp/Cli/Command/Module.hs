module Wasp.Cli.Command.Module
  ( module_,
  )
where

import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import StrongPath (Abs, Dir, Path', (</>))
import qualified StrongPath as SP
import System.Directory (getCurrentDirectory)
import Wasp.Cli.Command (Command, CommandError (CommandError), require)
import Wasp.Cli.Command.Call (Arguments)
import Wasp.Cli.Command.Message (cliSendMessageC)
import Wasp.Cli.Command.Require (ValidNodeAndNpm (ValidNodeAndNpm))
import qualified Wasp.Message as Msg
import qualified Wasp.Project.Module as ProjectModule

module_ :: Arguments -> Command ()
module_ = \case
  ["new", packageName] -> new packageName
  ["install"] -> install
  ["build"] -> build
  _ ->
    throwError $
      CommandError
        "Unknown module command"
        "Usage: wasp module new <name> | wasp module install | wasp module build"

new :: String -> Command ()
new packageName = do
  currentDir <- getCurrentDir
  moduleDirName <- case SP.parseRelDir $ ProjectModule.packageNameToDirName packageName of
    Just dirName -> return dirName
    Nothing -> throwError $ CommandError "Invalid module name" $ "Couldn't derive a directory name from " ++ show packageName ++ "."
  let moduleDir = currentDir </> moduleDirName

  cliSendMessageC $ Msg.Start "Creating Wasp module scaffold..."
  liftIO (ProjectModule.createModuleOnDisk moduleDir packageName) >>= \case
    Right () -> cliSendMessageC $ Msg.Success "Wasp module scaffold created."
    Left errorMessage -> throwError $ CommandError "Failed to create Wasp module scaffold" errorMessage

install :: Command ()
install = do
  ValidNodeAndNpm <- require
  moduleDir <- getCurrentDir

  cliSendMessageC $ Msg.Start "Installing Wasp module dependencies..."
  liftIO (ProjectModule.installModuleIO moduleDir) >>= \case
    Right () -> cliSendMessageC $ Msg.Success "Wasp module dependencies installed."
    Left errorMessage -> throwError $ CommandError "Failed to install Wasp module dependencies" errorMessage

build :: Command ()
build = do
  ValidNodeAndNpm <- require
  moduleDir <- getCurrentDir

  cliSendMessageC $ Msg.Start "Building Wasp module..."
  liftIO (ProjectModule.buildModuleIO moduleDir) >>= \case
    Right () -> cliSendMessageC $ Msg.Success "Wasp module built."
    Left errorMessage -> throwError $ CommandError "Failed to build Wasp module" errorMessage

getCurrentDir :: Command (Path' Abs (Dir dirType))
getCurrentDir = do
  currentDirFilePath <- liftIO getCurrentDirectory
  case SP.parseAbsDir currentDirFilePath of
    Just currentDir -> return currentDir
    Nothing -> throwError $ CommandError "Invalid current directory" $ "Couldn't parse the current directory path: " ++ currentDirFilePath
