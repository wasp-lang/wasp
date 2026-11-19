module Wasp.Cli.Command.Studio
  ( studio,
  )
where

import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import Wasp.Cli.Command (Command, CommandError (CommandError), require)
import Wasp.Cli.Command.Message (cliSendMessageC)
import Wasp.Cli.Command.Require.InWaspProject (InWaspProject (InWaspProject))
import Wasp.Cli.Command.Require.ValidNodeAndNpm (ValidNodeAndNpm (ValidNodeAndNpm))
import qualified Wasp.Message as Msg
import qualified Wasp.Project.Studio

studio :: Command ()
studio = do
  ValidNodeAndNpm <- require
  InWaspProject _waspDir <- require

  cliSendMessageC . Msg.Info $
    unlines
      [ "✨ Starting Wasp Studio ✨",
        "",
        "➜ Open in your browser: " ++ Wasp.Project.Studio.studioUrl,
        "",
        "Wasp Studio visualises your app and lets you understand how different parts of your app are connected.",
        "It shows the app that `wasp start` is running, so keep it running in another terminal."
      ]

  result <- liftIO Wasp.Project.Studio.startStudio

  either (throwError . CommandError "Studio command failed") return result
