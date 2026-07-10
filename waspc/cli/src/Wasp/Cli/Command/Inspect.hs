module Wasp.Cli.Command.Inspect
  ( inspect,
    renderInspection,
  )
where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson (object, (.=))
import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy as BSL
import Data.List (intercalate)
import System.IO (stdout)
import Wasp.AppSpec (AppSpec)
import qualified Wasp.AppSpec as AS
import Wasp.AppSpec.Core.Inspectable (InspectionEntry (..))
import Wasp.AppSpec.Inspect (InspectionSection (..), inspectAppSpec)
import Wasp.Cli.Command (Command)
import Wasp.Cli.Command.Call (Arguments)
import Wasp.Cli.Command.Compile (analyzeWithWarningsOnStderr)
import Wasp.Cli.Command.Inspect.ArgumentsParser (InspectArgs (..), inspectArgsParser)
import Wasp.Cli.Command.Require (InWaspProject (InWaspProject), ValidNodeAndNpm (ValidNodeAndNpm), WaspSpecAvailable (WaspSpecAvailable), require)
import Wasp.Cli.Terminal (title)
import Wasp.Cli.Util.Parser (withArguments)
import Wasp.Util (alignColumns)
import Wasp.Version (waspVersion)

-- | Prints the evaluated app spec: as a human-readable overview by default,
-- or as full JSON with --json. Only the payload goes to stdout (warnings and
-- errors go to stderr), so `wasp inspect --json` is safe to pipe.
inspect :: Arguments -> Command ()
inspect = withArguments "wasp inspect" inspectArgsParser $ \args -> do
  ValidNodeAndNpm <- require
  InWaspProject waspDir <- require
  WaspSpecAvailable <- require
  appSpec <- analyzeWithWarningsOnStderr waspDir
  liftIO $
    if json args
      then do
        BSL.hPutStr stdout $ encodeAppSpecJson appSpec
        putStrLn ""
      else putStr $ renderInspection $ inspectAppSpec appSpec

encodeAppSpecJson :: AppSpec -> BSL.ByteString
encodeAppSpecJson appSpec =
  encodePretty $
    object
      [ "waspVersion" .= show waspVersion,
        "decls" .= AS.decls appSpec
      ]

renderInspection :: [InspectionSection] -> String
renderInspection sections = intercalate "\n" $ renderSection <$> nonEmptySections
  where
    nonEmptySections = filter (not . null . sectionEntries) sections
    renderSection section =
      unlines $
        title (sectionTitle section)
          : map ("  " ++) (alignColumns $ entryCells <$> sectionEntries section)
