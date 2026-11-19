module Wasp.Cli.Message (cliSendMessage, cliSendWarningToStderr) where

import Data.Maybe (fromMaybe)
import Wasp.Cli.Common (waspSays, waspScreams, waspWarns, waspWarnsOnStderr)
import Wasp.Cli.Terminal
  ( asWaspFailureMessage,
    asWaspStartMessage,
    asWaspSuccessMessage,
    asWaspWarningMessage,
  )
import Wasp.Diagnostic (Diagnostic (..), DiagnosticKind (..))
import qualified Wasp.Message as Msg

-- | Send a message using the CLI
cliSendMessage :: Msg.SendMessage
cliSendMessage (Msg.Info msg) =
  waspSays msg
cliSendMessage (Msg.Start msg) =
  waspSays $ asWaspStartMessage msg
cliSendMessage (Msg.Diagnostic diagnostic) = case diagnostic.kind of
  Success -> waspSays $ asWaspSuccessMessage diagnostic.title
  Error _ -> waspScreams $ asWaspFailureMessage (diagnostic.title ++ ":") ++ detailsText
  Warning -> waspWarns $ formatWarning diagnostic
  where
    detailsText = fromMaybe "" diagnostic.details

-- | Like 'cliSendMessage' for a warning, but to stderr, so it doesn't mix with
-- the data some commands print to stdout, e.g. `wasp show build --json`.
cliSendWarningToStderr :: Diagnostic -> IO ()
cliSendWarningToStderr = waspWarnsOnStderr . formatWarning

formatWarning :: Diagnostic -> String
formatWarning diagnostic =
  asWaspWarningMessage (diagnostic.title ++ ":") ++ fromMaybe "" diagnostic.details
