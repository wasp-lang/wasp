module Wasp.Cli.Message (cliSendMessage) where

import Data.Maybe (fromMaybe)
import Wasp.Cli.Common (waspSays, waspScreams, waspWarns)
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
  Warning -> waspWarns $ asWaspWarningMessage (diagnostic.title ++ ":") ++ detailsText
  where
    detailsText = fromMaybe "" diagnostic.details
