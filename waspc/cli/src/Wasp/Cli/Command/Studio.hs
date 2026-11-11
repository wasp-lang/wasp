module Wasp.Cli.Command.Studio
  ( studio,
  )
where

import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import Wasp.Cli.Command (Command, CommandError (CommandError), require)
import Wasp.Cli.Command.Compile (compileCommand)
import Wasp.Cli.Command.Message (cliSendMessageC)
import Wasp.Cli.Command.Require.InWaspProject (InWaspProject (InWaspProject))
import qualified Wasp.Message as Msg
import qualified Wasp.Project.Studio

studio :: Command ()
studio = do
  InWaspProject waspDir <- require

  -- Compiling writes the file Studio reads, and installs Studio itself.
  _ <- compileCommand

  cliSendMessageC . Msg.Info $
    unlines
      [ "✨ Starting Wasp Studio ✨",
        "",
        "➜ Open in your browser: http://localhost:4000",
        "",
        "Wasp Studio visualises your app and lets you understand how different parts of your app are connected.",
        "It is also available as a panel in your app's DevTools while `wasp start` is running."
      ]

  result <- liftIO $ Wasp.Project.Studio.startStudio waspDir

  either (throwError . CommandError "Studio command failed") return result
