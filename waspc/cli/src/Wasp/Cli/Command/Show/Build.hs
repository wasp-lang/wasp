{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Wasp.Cli.Command.Show.Build
  ( buildShowSubcommand,
  )
where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson (ToJSON)
import GHC.Generics (Generic)
import StrongPath (Abs, Dir, Path', (</>))
import qualified StrongPath as SP
import System.Directory (getFileSize)
import System.IO (hPutStrLn, stderr)
import Wasp.Cli.Command (Command, require)
import Wasp.Cli.Command.Require.InWaspProject (InWaspProject (InWaspProject))
import Wasp.Cli.Command.Show.Subcommand (ShowSubcommand (..))
import qualified Wasp.Generator.WaspInfo as WI
import Wasp.Inspectable
  ( Inspectable (inspect),
    InspectionEntry (InspectionEntry),
  )
import Wasp.Project.Common
  ( WaspProjectDir,
    dotWaspDirInWaspProjectDir,
    generatedAppDirInDotWaspDir,
  )
import qualified Wasp.Util.IO as IOUtil

buildShowSubcommand :: ShowSubcommand
buildShowSubcommand =
  ShowSubcommand
    { name = "build",
      description = "Information about the last build, if any.",
      getData = getBuildInfo
    }

data BuildInfo = BuildInfo
  { lastCompile :: Maybe WI.WaspInfo,
    projectDirSize :: String
  }
  deriving (Generic, ToJSON)

instance Inspectable BuildInfo where
  inspect BuildInfo {lastCompile = maybeWaspInfo, projectDirSize} =
    InspectionEntry
      "Project"
      [ ("Project dir size", projectDirSize)
      ]
      : maybe [] inspect maybeWaspInfo

getBuildInfo :: Command BuildInfo
getBuildInfo = do
  InWaspProject waspDir <- require

  projectDirSize <- liftIO $ readDirectorySizeMB waspDir

  let generatedAppDir = waspDir </> dotWaspDirInWaspProjectDir </> generatedAppDirInDotWaspDir
  waspInfoOrError <- liftIO $ WI.safeRead generatedAppDir

  lastCompile <- case waspInfoOrError of
    Left WI.NotFound ->
      liftIO (hPutStrLn stderr "No compile information found")
        >> return Nothing
    Left WI.IncompatibleFormat ->
      liftIO (hPutStrLn stderr "Incompatible compile information")
        >> return Nothing
    Right waspInfo -> return $ Just waspInfo

  return
    BuildInfo
      { lastCompile = lastCompile,
        projectDirSize = projectDirSize
      }

readDirectorySizeMB :: Path' Abs (Dir WaspProjectDir) -> IO String
readDirectorySizeMB path = (++ " MB") . show . (`div` 1000000) . sum <$> allFileSizes
  where
    allFileSizes = IOUtil.listDirectoryDeep path >>= mapM (getFileSize . SP.fromRelFile)
