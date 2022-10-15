module Wasp.Cli.Command.Update
  ( update,
  )
where

import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import System.Environment (getExecutablePath)
import System.Exit (ExitCode (..), exitWith)
import System.IO (IOMode(WriteMode), hIsWritable, withFile)
import System.Info (arch, os)
import System.Process (system)
import Wasp.Cli.Command (Command)
import Wasp.Cli.Command.Message (cliSendMessageC)
import qualified Wasp.Message as Msg

update :: Command ()
update = do
  cliSendMessageC $ Msg.Start "Updating wasp..."
  ensureWritability
  status <- liftIO $ system $ "curl -sSL " <> archiveUrl <> " | tar xvzf - -C ~/.local/share/wasp"
  if status == ExitSuccess
    then cliSendMessageC $ Msg.Success "Wasp updated!"
    else do
      cliSendMessageC $ Msg.Success "Update failure"
      liftIO $ exitWith status

ensureWritability :: Command ()
ensureWritability = do
  exe <- liftIO getExecutablePath
  isWritable <- liftIO $ withFile exe WriteMode hIsWritable
  unless isWritable $
    failedOn "unable to rewrite wasp binary"

failedOn :: String -> Command ()
failedOn reason = do
  cliSendMessageC $ Msg.Success $ "Update failure: " <> reason
  liftIO $ exitWith $ ExitFailure 1

archiveUrl :: String
archiveUrl = "https://github.com/wasp-lang/wasp/releases/latest/download/" <> archiveName

archiveName :: String
archiveName = "wasp-" <> os <> "-" <> arch <> ".tar.gz"
