module Wasp.Cli.Command.Module
  ( module_,
  )
where

import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (fromJust)
import StrongPath (Abs, Dir, Path')
import qualified StrongPath as SP
import System.Directory (getCurrentDirectory)
import qualified System.FilePath as FP
import Wasp.Cli.Command (Command, CommandError (CommandError), require)
import Wasp.Cli.Command.Call (Arguments)
import Wasp.Cli.Command.Message (cliSendMessageC)
import Wasp.Cli.Command.Require (ValidNodeAndNpm (ValidNodeAndNpm))
import Wasp.Project.Common (WaspProjectDir)
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
        "Usage: wasp module new <name> | wasp module <install|build>"

new :: String -> Command ()
new packageName = do
  currentDir <- liftIO getCurrentDirectory
  let moduleDir = currentDir FP.</> ProjectModule.packageNameToDirName packageName

  cliSendMessageC $ Msg.Start "Creating Wasp module scaffold..."
  liftIO (ProjectModule.createModuleOnDisk moduleDir packageName) >>= \case
    Right () -> cliSendMessageC $ Msg.Success "Wasp module scaffold created."
    Left errorMessage -> throwError $ CommandError "Failed to create Wasp module scaffold" errorMessage

install :: Command ()
install = do
  ValidNodeAndNpm <- require
  moduleDir <- getCurrentModuleDir

  cliSendMessageC $ Msg.Start "Installing Wasp module dependencies..."
  liftIO (ProjectModule.installModuleIO moduleDir) >>= \case
    Right () -> cliSendMessageC $ Msg.Success "Wasp module dependencies installed."
    Left errorMessage -> throwError $ CommandError "Failed to install Wasp module dependencies" errorMessage

build :: Command ()
build = do
  ValidNodeAndNpm <- require
  moduleDir <- getCurrentModuleDir

  cliSendMessageC $ Msg.Start "Building Wasp module..."
  liftIO (ProjectModule.buildModuleIO moduleDir) >>= \case
    Right () -> cliSendMessageC $ Msg.Success "Wasp module built."
    Left errorMessage -> throwError $ CommandError "Failed to build Wasp module" errorMessage

getCurrentModuleDir :: Command (Path' Abs (Dir WaspProjectDir))
getCurrentModuleDir = SP.castDir . fromJust . SP.parseAbsDir <$> liftIO getCurrentDirectory
