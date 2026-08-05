module Wasp.Cli.Command.Show.Build
  ( buildShowSubcommand,
  )
where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson (object, (.=))
import qualified Data.Aeson as Aeson
import StrongPath ((</>))
import Wasp.AppSpec.Core.Inspectable (InspectionDatapoint, InspectionEntry (InspectionEntry))
import Wasp.Cli.Command (Command, require)
import Wasp.Cli.Command.Common (readDirectorySizeMB)
import Wasp.Cli.Command.Require.InWaspProject (InWaspProject (InWaspProject))
import Wasp.Cli.Command.Show.Subcommand (ShowSubcommand (..))
import qualified Wasp.Generator.WaspInfo as WI
import qualified Wasp.Project.Common as Project.Common

-- | Shows information about the project's current build: as a human-readable
-- overview by default, or as JSON with --json.
buildShowSubcommand :: ShowSubcommand
buildShowSubcommand =
  ShowSubcommand
    { name = "build",
      description = "Prints information about your app's current build",
      jsonHelp = "Print the build information as JSON.",
      getInspectionEntries = buildAsEntries <$> getBuildInfo,
      getJson = buildAsJson <$> getBuildInfo
    }

-- | The compile information of the current build (if any), and the project dir
-- size.
type BuildInfo = (WI.ReadResult, String)

getBuildInfo :: Command BuildInfo
getBuildInfo = do
  InWaspProject waspDir <- require
  waspInfoOrError <- liftIO $ WI.safeRead $ generatedAppDir waspDir
  projectDirSize <- liftIO $ readDirectorySizeMB waspDir
  return (waspInfoOrError, projectDirSize)
  where
    generatedAppDir waspDir =
      waspDir
        </> Project.Common.dotWaspDirInWaspProjectDir
        </> Project.Common.generatedAppDirInDotWaspDir

buildAsEntries :: BuildInfo -> [InspectionEntry]
buildAsEntries (waspInfoOrError, projectDirSize) =
  [ InspectionEntry "Build" $
      lastCompileDatapoints ++ [("Project dir size", projectDirSize)]
  ]
  where
    lastCompileDatapoints :: [InspectionDatapoint]
    lastCompileDatapoints = case waspInfoOrError of
      Left WI.NotFound -> [("Last compile", "No compile information found")]
      Left WI.IncompatibleFormat -> [("Last compile", "Incompatible compile information")]
      Right waspInfo ->
        [ ("Type", show $ WI.buildType waspInfo),
          ("Generated at", show $ WI.generatedAt waspInfo),
          ("Wasp version", WI.waspVersion waspInfo)
        ]

buildAsJson :: BuildInfo -> Aeson.Value
buildAsJson (waspInfoOrError, projectDirSize) =
  object
    [ "lastCompile" .= either (const Nothing) Just waspInfoOrError,
      "projectDirSize" .= projectDirSize
    ]
