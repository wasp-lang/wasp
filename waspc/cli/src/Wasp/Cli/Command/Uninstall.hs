module Wasp.Cli.Command.Uninstall
  ( uninstall,
  )
where

import Control.Monad (when)
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
import Wasp.Cli.Command (Command)
import Wasp.Cli.Command.Message (cliSendMessageC)
import qualified Wasp.Message as Msg

-- Types
data HomeDir

-- | Removes Wasp CLI from the system.
-- It removes the follwing:
-- {home}/.local/share/wasp-lang/{version}
-- {home}/.local/bin/wasp
uninstall :: Command ()
uninstall = do
  cliSendMessageC $
    Msg.Info $ "Uninstalling Wasp CLI ..."

  homeDir <- liftIO getHomeDir
  let waspDir = getWaspBinariesDir homeDir
      waspBin = getWaspScript homeDir

  -- Deleting dir with Wasp binaries
  doesWaspDirExist <- liftIO $ doesDirectoryExist $ SP.fromAbsDir waspDir
  when doesWaspDirExist $ do
    liftIO $ removeDirectoryRecursive $ SP.toFilePath waspDir

  -- Deleting the script referencing Wasp binary
  doesWaspScriptExist <- liftIO $ doesFileExist $ SP.fromAbsFile waspBin
  when doesWaspScriptExist $ do
    liftIO $ removeFile $ SP.fromAbsFile waspBin

  cliSendMessageC $ Msg.Success "Uninstalled Wasp CLI"

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
