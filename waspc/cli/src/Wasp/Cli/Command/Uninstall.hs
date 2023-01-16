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
import Wasp.Version (waspVersion)

-- Types
data HomeDir

-- | Removes Wasp CLI from the system.
-- It removes the follwing:
-- {home}/.local/share/wasp-lang/{version}
-- {home}/.local/bin/wasp
uninstall :: Command ()
uninstall = do
  let version = show waspVersion
  cliSendMessageC $
    Msg.Info $ "Uninstalling Wasp CLI " ++ version ++ " ..."

  homeDir <- liftIO getHomeDir
  let waspDir = getWaspBinariesDir homeDir version
      waspBin = getWaspScript homeDir

  -- Deleting dir with Wasp binaries
  doesWaspDirExist <- liftIO $ doesDirectoryExist $ SP.fromAbsDir waspDir
  when doesWaspDirExist $ do
    liftIO $ removeDirectoryRecursive $ SP.toFilePath waspDir

  -- Deleting the script referencing Wasp binary
  doesWaspScriptExist <- liftIO $ doesFileExist $ SP.fromAbsFile waspBin
  when doesWaspScriptExist $ do
    liftIO $ removeFile $ SP.fromAbsFile waspBin

  cliSendMessageC $ Msg.Success $ "Uninstalled Wasp CLI " ++ version

getHomeDir :: IO (SP.Path' SP.Abs (SP.Dir HomeDir))
getHomeDir = fromJust . SP.parseAbsDir <$> getHomeDirectory

getWaspBinariesDir :: SP.Path' SP.Abs (SP.Dir HomeDir) -> String -> SP.Path' SP.Abs (SP.Dir String)
getWaspBinariesDir homeDir version =
  homeDir
    SP.</> [SP.reldir|.local/share/wasp-lang|]
    SP.</> fromJust (SP.parseRelDir version)

getWaspScript :: SP.Path' SP.Abs (SP.Dir HomeDir) -> SP.Path' SP.Abs (SP.File String)
getWaspScript homeDir =
  homeDir
    SP.</> [SP.relfile|.local/bin/wasp|]
