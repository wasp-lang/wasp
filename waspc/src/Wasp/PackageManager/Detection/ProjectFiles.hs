module Wasp.PackageManager.Detection.ProjectFiles
  ( detectFromProjectFiles,
  )
where

import Control.Applicative ((<|>))
import Control.Monad.Extra (findM, firstJustM, ifM, (||^))
import Data.Aeson (Value)
import Data.Foldable (asum, find)
import Data.Maybe (listToMaybe)
import StrongPath (Abs, Dir, File', Path', relfile, (</>))
import qualified StrongPath as SP
import System.Directory (doesPathExist)
import Wasp.PackageManager.Detection.Common (Detection (..), DetectionSource (..))
import Wasp.PackageManager.MarkerFiles (MarkerFile (..), PackageRootDir, installMetadataFiles, lockfilesAndWorkspaceFiles)
import Wasp.PackageManager.PackageJson (hasWorkspacesField, parseDevEnginesField, parsePackageManagerField)
import Wasp.PackageManager.PackageManager (supportedPackageManagers)
import Wasp.Util.IO (doesFileExist)
import Wasp.Util.Json (readJsonFile)

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

detectInWorkspaceRoot :: Path' Abs (Dir PackageRootDir) -> IO (Maybe Detection)
detectInWorkspaceRoot dir = do
  packageJson <- readPackageJsonIfValid dir
  ifM (isWorkspaceRoot dir packageJson) (detectInDir dir packageJson) (return Nothing)

detectInDir :: Path' Abs (Dir PackageRootDir) -> Maybe Value -> IO (Maybe Detection)
detectInDir dir packageJson =
  firstJustM
    id
    [ return $ packageJson >>= detectFromPackageJson (packageJsonPathIn dir),
      detectFromMarkerFiles LockfileOrWorkspaceFile lockfilesAndWorkspaceFiles dir,
      detectFromMarkerFiles InstallMetadata installMetadataFiles dir
    ]

detectFromPackageJson :: Path' Abs File' -> Value -> Maybe Detection
detectFromPackageJson packageJsonPath packageJson =
  asum
    [ (`Detection` PackageManagerField packageJsonPath) <$> parsePackageManagerField packageJson,
      (`Detection` DevEnginesField packageJsonPath) <$> pickAlternative (parseDevEnginesField packageJson)
    ]
  where
    -- The project accepts any of the alternatives, so one Wasp supports wins.
    pickAlternative alternatives =
      find (`elem` supportedPackageManagers) alternatives <|> listToMaybe alternatives

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

packageJsonPathIn :: Path' Abs (Dir PackageRootDir) -> Path' Abs File'
packageJsonPathIn dir = dir </> [relfile|package.json|]

-- | 'Nothing' for an invalid package.json too, so the other files in the
-- directory still get checked.
readPackageJsonIfValid :: Path' Abs (Dir PackageRootDir) -> IO (Maybe Value)
readPackageJsonIfValid dir =
  ifM (doesFileExist packageJsonPath) (readJsonFile packageJsonPath) (return Nothing)
  where
    packageJsonPath = packageJsonPathIn dir

-- | @.git@ is a directory in a normal clone and a file in a git worktree.
isRepositoryRoot :: Path' Abs (Dir PackageRootDir) -> IO Bool
isRepositoryRoot dir = doesPathExist $ SP.fromAbsFile $ dir </> [relfile|.git|]

isWorkspaceRoot :: Path' Abs (Dir PackageRootDir) -> Maybe Value -> IO Bool
isWorkspaceRoot dir packageJson =
  pure (maybe False hasWorkspacesField packageJson)
    ||^ doesFileExist (dir </> [relfile|pnpm-workspace.yaml|])
