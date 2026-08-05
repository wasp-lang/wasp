module Wasp.Cli.Command.Show.Build
  ( showBuild,
  )
where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson (object, (.=))
import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy.Char8 as BSL8
import Wasp.AppSpec.Core.Inspectable (InspectionEntry (InspectionEntry))
import Wasp.Cli.Command (Command, require)
import Wasp.Cli.Command.Common (readDirectorySizeMB, readWaspCompileInfo)
import Wasp.Cli.Command.Require.InWaspProject (InWaspProject (InWaspProject))
import Wasp.Cli.Command.Show.ArgumentsParser (ShowSubcommandArgs (..))
import Wasp.Cli.Command.Show.Table (renderEntriesAsTables)

-- | Prints information about the project's current build: as a human-readable
-- overview by default, or as JSON with --json.
showBuild :: ShowSubcommandArgs -> Command ()
showBuild args = do
  InWaspProject waspDir <- require
  lastCompile <- liftIO $ readWaspCompileInfo waspDir
  projectDirSize <- liftIO $ readDirectorySizeMB waspDir
  liftIO $ putStr $ renderFn args.json lastCompile projectDirSize
  where
    renderFn isJson
      | isJson = buildAsJson
      | otherwise = buildAsTable

buildAsTable :: String -> String -> String
buildAsTable lastCompile projectDirSize =
  renderEntriesAsTables
    [ InspectionEntry
        "Build"
        [ ("Last compile", lastCompile),
          ("Project dir size", projectDirSize)
        ]
    ]

buildAsJson :: String -> String -> String
buildAsJson lastCompile projectDirSize =
  BSL8.unpack $ encodePretty outputObject <> "\n"
  where
    outputObject =
      object
        [ "lastCompile" .= lastCompile,
          "projectDirSize" .= projectDirSize
        ]
