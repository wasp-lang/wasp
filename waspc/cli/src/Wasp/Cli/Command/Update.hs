module Wasp.Cli.Command.Update
  ( update,
  )
where

import Control.Monad.IO.Class (liftIO)
import System.Exit (ExitCode (..), exitWith)
import System.Process (system)
import Wasp.Cli.Command (Command)
import Wasp.Cli.Command.Message (cliSendMessageC)
import qualified Wasp.Cli.Common as Common
import qualified Wasp.Message as Msg

update :: Command ()
update = do
  cliSendMessageC $ Msg.Start "Updating wasp..."
  status <- liftIO $ system "curl -sSL https://get.wasp-lang.dev/installer.sh | sh"
  if status == ExitSuccess
    then cliSendMessageC $ Msg.Success "Wasp updated!"
    else do
      cliSendMessageC $ Msg.Success "Update failure"
      liftIO $ exitWith status
