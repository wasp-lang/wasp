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
cliSendMessage (Msg.Info msg) =
  waspSays msg
cliSendMessage (Msg.Start msg) =
  waspSays $ asWaspStartMessage msg
cliSendMessage (Msg.Success msg) =
  waspSays $ asWaspSuccessMessage msg
cliSendMessage (Msg.Failure title msg) =
  waspScreams $ asWaspFailureMessage (title ++ ":") ++ msg
cliSendMessage (Msg.Warning title msg) =
  waspWarns $ asWaspWarningMessage (title ++ ":") ++ msg