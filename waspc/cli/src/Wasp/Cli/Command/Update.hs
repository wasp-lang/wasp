module Wasp.Cli.Command.Update
  ( update,
  )
where

import Control.Monad.IO.Class (liftIO)
import System.Environment
import System.Exit (ExitCode (..), exitWith)
import System.IO
import System.Info
import Wasp.Cli.Command (Command)
import Wasp.Cli.Command.Message (cliSendMessageC)
import qualified Wasp.Cli.Common as Common
import qualified Wasp.Message as Msg

update :: Command ()
update = do
  cliSendMessageC $ Msg.Start "Updating wasp..."
  ensureWritability
  status <- liftIO $ system "curl -sSL " <> archiveUrl <> " | tar xvzf - -C ~/.local/share/wasp"
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
  liftIO $ exitWith status

archiveUrl :: String
archiveUrl = "https://github.com/wasp-lang/wasp/releases/latest/download/" <> archiveName

archiveName :: String
archiveName = "wasp-" <> os <> "-" <> arch <> ".tar.gz"
