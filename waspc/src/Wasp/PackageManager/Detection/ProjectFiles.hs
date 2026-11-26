module Wasp.PackageManager.Detection.ProjectFiles
  ( detectFromProjectFiles,
  )
where

import Control.Monad.Extra (findM, firstJustM, ifM, (||^))
import Data.Aeson (Value)
import Data.Foldable (asum)
import StrongPath (Abs, Dir, File', Path', relfile, (</>))
import qualified StrongPath as SP
import System.Directory (doesPathExist)
import Wasp.PackageManager.Detection.Common (Detection (..), DetectionSource (..), PackageRootDir)
import Wasp.PackageManager.Detection.MarkerFiles (MarkerFile (..), installMetadataFiles, lockfilesAndWorkspaceFiles)
import Wasp.PackageManager.Detection.PackageJson (hasWorkspacesField, parseDevEnginesField, parsePackageManagerField, readPackageJsonIfValid)
import Wasp.Util.IO (doesFileExist)

-- | Checks the given directory, and then the workspace roots above it, so a
-- package inside a monorepo finds the root's lockfile.
detectFromProjectFiles :: Path' Abs (Dir PackageRootDir) -> IO (Maybe Detection)
detectFromProjectFiles startDir = do
  packageJson <- readPackageJsonIfValid startDir
  firstJustM id [detectInDir startDir packageJson, detectInWorkspaceRootsAbove startDir]

-- | Walks up one directory at a time, nearest first, and stops at the first
-- detection or at the repository root.
detectInWorkspaceRootsAbove :: Path' Abs (Dir PackageRootDir) -> IO (Maybe Detection)
detectInWorkspaceRootsAbove dir =
  ifM
    (pure (parentDir == dir) ||^ isRepositoryRoot dir)
    (return Nothing)
    (firstJustM id [detectInWorkspaceRoot parentDir, detectInWorkspaceRootsAbove parentDir])
  where
    parentDir = SP.parent dir

    -- @.git@ is a directory in a normal clone and a file in a git worktree.
    isRepositoryRoot = doesPathExist . SP.fromAbsFile . (</> [relfile|.git|])

detectInWorkspaceRoot :: Path' Abs (Dir PackageRootDir) -> IO (Maybe Detection)
detectInWorkspaceRoot dir = do
  packageJson <- readPackageJsonIfValid dir
  ifM (isWorkspaceRoot packageJson) (detectInDir dir packageJson) (return Nothing)
  where
    isWorkspaceRoot packageJson =
      pure (maybe False hasWorkspacesField packageJson)
        ||^ doesFileExist (dir </> [relfile|pnpm-workspace.yaml|])

detectInDir :: Path' Abs (Dir PackageRootDir) -> Maybe Value -> IO (Maybe Detection)
detectInDir dir packageJson =
  firstJustM
    id
    [ return $ packageJson >>= detectFromPackageJson (dir </> [relfile|package.json|]),
      detectFromMarkerFiles LockfileOrWorkspaceFile lockfilesAndWorkspaceFiles dir,
      detectFromMarkerFiles InstallMetadata installMetadataFiles dir
    ]

detectFromPackageJson :: Path' Abs File' -> Value -> Maybe Detection
detectFromPackageJson packageJsonPath packageJson =
  asum
    [ (`Detection` PackageManagerField packageJsonPath) <$> parsePackageManagerField packageJson,
      (`Detection` DevEnginesField packageJsonPath) <$> parseDevEnginesField packageJson
    ]

detectFromMarkerFiles ::
  (Path' Abs File' -> DetectionSource) ->
  [MarkerFile] ->
  Path' Abs (Dir PackageRootDir) ->
  IO (Maybe Detection)
detectFromMarkerFiles toSource markerFiles dir =
  fmap toDetection <$> findM (doesFileExist . absMarkerFilePath) markerFiles
  where
    absMarkerFilePath markerFile = dir </> markerFile.path
    toDetection markerFile = Detection markerFile.packageManager (toSource $ absMarkerFilePath markerFile)
