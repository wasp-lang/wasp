module Wasp.Cli.Command.Uninstall
  ( uninstall,
  )
where

import Control.Exception (SomeException, try)
import Control.Monad (when)
import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (fromJust)
import qualified StrongPath as SP
import System.Directory
  ( doesDirectoryExist,
    doesFileExist,
    getHomeDirectory,
    removeDirectoryRecursive,
    removeFile,
  )
import Wasp.Cli.Command (Command, CommandError (..))
import Wasp.Cli.Command.Message (cliSendMessageC)
import qualified Wasp.Message as Msg

-- Types
data HomeDir

-- | Removes Wasp CLI from the system.
-- It removes the follwing:
-- {home}/.local/share/wasp-lang
-- {home}/.local/bin/wasp
uninstall :: Command ()
uninstall = do
  cliSendMessageC $
    Msg.Info "Uninstalling Wasp CLI ..."

  uninstallResult <- liftIO (try removeWaspFiles :: IO (Either SomeException ()))
  either
    (throwError . CommandError "Unable to uninstall" . show)
    pure
    uninstallResult

  cliSendMessageC $ Msg.Success "Uninstalled Wasp CLI"

removeWaspFiles :: IO ()
removeWaspFiles = do
  homeDir <- getHomeDir
  let waspDir = getWaspBinariesDir homeDir
      waspBin = getWaspScript homeDir

  -- Deleting dir with Wasp binaries
  doesWaspDirExist <- liftIO $ doesDirectoryExist $ SP.fromAbsDir waspDir
  when doesWaspDirExist $ do
    removeDirectoryRecursive $ SP.toFilePath waspDir

  -- Deleting the script referencing Wasp binary
  doesWaspScriptExist <- liftIO $ doesFileExist $ SP.fromAbsFile waspBin
  when doesWaspScriptExist $ do
    removeFile $ SP.fromAbsFile waspBin

getHomeDir :: IO (SP.Path' SP.Abs (SP.Dir HomeDir))
getHomeDir = fromJust . SP.parseAbsDir <$> getHomeDirectory

getWaspBinariesDir :: SP.Path' SP.Abs (SP.Dir HomeDir) -> SP.Path' SP.Abs (SP.Dir String)
getWaspBinariesDir homeDir =
  homeDir
    SP.</> [SP.reldir|.local/share/wasp-lang|]

getWaspScript :: SP.Path' SP.Abs (SP.Dir HomeDir) -> SP.Path' SP.Abs (SP.File String)
getWaspScript homeDir =
  homeDir
    SP.</> [SP.relfile|.local/bin/wasp|]
