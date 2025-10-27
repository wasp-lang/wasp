module Wasp.Cli.Command.Studio
  ( studio,
  )
where

import Control.Arrow ()
import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy as BSL
import StrongPath (relfile, (</>))
import qualified StrongPath as SP
import StrongPath.Operations ()
import qualified System.Directory as Dir
import Wasp.Cli.Command (Command, CommandError (CommandError))
import Wasp.Cli.Command.Compile (analyze)
import Wasp.Cli.Command.Message (cliSendMessageC)
import Wasp.Cli.Command.Require (InWaspProject (InWaspProject), require)
import qualified Wasp.Message as Msg
import Wasp.Project.Common (dotWaspDirInWaspProjectDir, generatedCodeDirInDotWaspDir)
import qualified Wasp.Project.Studio

studio :: Command ()
studio = do
  InWaspProject waspDir <- require
  appSpec <- analyze waspDir

  let generatedProjectDir =
        waspDir </> dotWaspDirInWaspProjectDir </> generatedCodeDirInDotWaspDir

  let waspStudioDataJsonFilePath = generatedProjectDir </> [relfile|.wasp-studio-data.json|]
  liftIO $ do
    Dir.createDirectoryIfMissing True $ SP.fromAbsDir $ SP.parent waspStudioDataJsonFilePath
    BSL.writeFile (SP.fromAbsFile waspStudioDataJsonFilePath) (encodePretty appSpec)

  cliSendMessageC . Msg.Info $
    unlines
      [ "✨ Starting Wasp Studio ✨",
        "",
        "➜ Open in your browser: http://localhost:4000",
        "",
        "Wasp Studio visualises your app and lets you understand how different parts of your app are connected."
      ]

  result <- liftIO $ do
    Wasp.Project.Studio.startStudio $ SP.toFilePath waspStudioDataJsonFilePath

  either (throwError . CommandError "Studio command failed") return result
