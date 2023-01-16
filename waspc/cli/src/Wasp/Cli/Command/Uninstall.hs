module Wasp.Cli.Command.Uninstall
  ( uninstall,
  )
where

import Control.Monad.IO.Class (liftIO)
import Data.Maybe (fromJust)
import qualified StrongPath as SP
import System.Directory
  ( getHomeDirectory,
    removeDirectoryRecursive,
    removeFile,
  )
import Wasp.Cli.Command (Command)
import Wasp.Cli.Command.Message (cliSendMessageC)
import qualified Wasp.Message as Msg
import Wasp.Version (waspVersion)

-- | Removes Wasp CLI from the system.
-- It removes the follwing:
-- {home}/.local/share/wasp-lang/{version}
-- {home}/.local/bin/wasp
uninstall :: Command ()
uninstall = do
  let version = show waspVersion
  cliSendMessageC $
    Msg.Info $ "Uninstall Wasp CLI " <> version
  home <- liftIO getHomeDirectory
  let homeDir = fromJust $ SP.parseAbsDir home
  let waspDir = homeDir SP.</> [SP.reldir|.local/share/wasp-lang|] SP.</> fromJust (SP.parseRelFile version)
      waspBin = homeDir SP.</> [SP.relfile|.local/bin/wasp|]
  liftIO $ removeDirectoryRecursive $ SP.toFilePath waspDir
  liftIO $ removeFile $ SP.fromAbsFile waspBin
  cliSendMessageC $ Msg.Success $ "Uninstalled Wasp CLI " <> version
