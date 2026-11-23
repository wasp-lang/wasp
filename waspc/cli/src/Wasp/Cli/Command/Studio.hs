module Wasp.Cli.Command.Studio
  ( studio,
  )
where

import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy as BSL
import Network.Socket (PortNumber)
import qualified Options.Applicative as Opt
import StrongPath (relfile, (</>))
import qualified StrongPath as SP
import qualified System.Directory as Dir
import Wasp.AppSpec.Inspectable (InspectableAppSpec (InspectableAppSpec))
import Wasp.Cli.Command (Command, CommandError (CommandError), require)
import Wasp.Cli.Command.Call (Arguments)
import Wasp.Cli.Command.Compile (analyze)
import Wasp.Cli.Command.Message (cliSendMessageC)
import Wasp.Cli.Command.Require.InWaspProject (InWaspProject (InWaspProject))
import Wasp.Cli.Command.Require.ValidNodeAndNpm (ValidNodeAndNpm (ValidNodeAndNpm))
import Wasp.Cli.Command.Require.WaspSpecAvailable (WaspSpecAvailable (WaspSpecAvailable))
import Wasp.Cli.Port (resolvePort)
import Wasp.Cli.Util.Parser (ArgsParser (..), withArguments)
import Wasp.Cli.Util.PortArgument (portParser)
import qualified Wasp.Message as Msg
import Wasp.Project.Common (dotWaspDirInWaspProjectDir, generatedAppDirInDotWaspDir)
import qualified Wasp.Project.Studio

newtype StudioArgs = StudioArgs
  { port :: Maybe PortNumber
  }

studioArgsParser :: ArgsParser StudioArgs
studioArgsParser =
  ArgsParser "wasp studio" $
    StudioArgs
      <$> Opt.optional (portParser "port" ("Port to run Wasp Studio on (default: " ++ show defaultStudioPort ++ ")"))

studio :: Arguments -> Command ()
studio = withArguments studioArgsParser $ \args -> do
  ValidNodeAndNpm <- require
  InWaspProject waspDir <- require
  WaspSpecAvailable <- require

  appSpec <- analyze waspDir

  let generatedProjectDir =
        waspDir
          </> dotWaspDirInWaspProjectDir
          </> generatedAppDirInDotWaspDir

  -- The same JSON that the app's server serves to the Wasp Studio panel in
  -- Vite DevTools, so the Wasp Studio page reads both the same way.
  let waspStudioDataJsonFilePath = generatedProjectDir </> [relfile|.wasp-studio-data.json|]
  liftIO $ do
    Dir.createDirectoryIfMissing True $ SP.fromAbsDir $ SP.parent waspStudioDataJsonFilePath
    BSL.writeFile (SP.fromAbsFile waspStudioDataJsonFilePath) (encodePretty $ InspectableAppSpec appSpec)

  studioPort <-
    resolvePort
      args.port
      defaultStudioPort
      []
      "Choose a different port with --port, or free up this one."
      "Free at least one of those ports by exiting the program listening on it, or choose the port yourself with --port."

  cliSendMessageC . Msg.Info $
    unlines
      [ "✨ Starting Wasp Studio ✨",
        "",
        "➜ Open in your browser: http://localhost:" ++ show studioPort,
        "",
        "Wasp Studio visualises your app and lets you understand how different parts of your app are connected."
      ]

  result <- liftIO $ do
    Wasp.Project.Studio.startStudio (SP.toFilePath waspStudioDataJsonFilePath) studioPort

  either (throwError . CommandError "Studio command failed") return result

-- | The port Wasp Studio runs on when it isn't given one.
defaultStudioPort :: PortNumber
defaultStudioPort = 4000
