module Wasp.Cli.Message (cliSendMessage, cliSendWarningToStderr) where

import Wasp.Cli.Common (waspSays, waspScreams, waspWarns, waspWarnsOnStderr)
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

-- | Like 'cliSendMessage' for a warning, but to stderr, so it doesn't mix with
-- the data some commands print to stdout, e.g. `wasp show spec --json`.
cliSendWarningToStderr :: String -> String -> IO ()
cliSendWarningToStderr title msg =
  waspWarnsOnStderr $ asWaspWarningMessage (title ++ ":") ++ msg
