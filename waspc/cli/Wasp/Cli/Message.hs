module Wasp.Cli.Message (cliSendMessage) where

import Wasp.Cli.Common (waspSays, waspScreams, waspWarns)
import Wasp.Cli.Terminal
  ( asWaspFailureMessage,
    asWaspStartMessage,
    asWaspSuccessMessage,
    asWaspWarningMessage,
  )
import qualified Wasp.Message as Msg

-- | Send a message using the CLI
cliSendMessage :: Msg.SendMessage
cliSendMessage (Msg.Start msg) =
  waspSays $ asWaspStartMessage msg
cliSendMessage (Msg.Success msg) =
  waspSays $ asWaspSuccessMessage msg
cliSendMessage (Msg.Failure msg) =
  waspScreams $ asWaspFailureMessage msg
cliSendMessage (Msg.Warning msg) =
  waspWarns $ asWaspWarningMessage msg