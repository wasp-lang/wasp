module Wasp.PackageManager.Detection.ProjectFiles
  ( detectFromProjectFiles,
  )
where

import Control.Monad.Extra (findM, firstJustM)
import Data.Aeson (Value)
import Data.Foldable (asum)
import StrongPath (Abs, Dir, File', Path', relfile, (</>))
import qualified StrongPath as SP
import System.Directory (doesPathExist)
import Wasp.PackageManager.Detection.Common (Detection (..), DetectionSource (..))
import Wasp.PackageManager.MarkerFiles (MarkerFile (..), PackageRootDir, installMetadataFiles, lockfilesAndWorkspaceFiles)
import Wasp.PackageManager.PackageJson (hasWorkspacesField, parseDevEnginesField, parsePackageManagerField)
import Wasp.Util.IO (doesFileExist)
import Wasp.Util.Json (readJsonFile)

-- | Checks the given directory, and then the ancestors that are workspace
-- roots, so a package inside a monorepo finds the root's lockfile.
-- Never goes past the repository root.
detectFromProjectFiles :: Path' Abs (Dir PackageRootDir) -> IO (Maybe Detection)
detectFromProjectFiles startDir = do
  maybeStartPackageJson <- readPackageJsonIfValid startDir
  firstJustM id [detectInDir startDir maybeStartPackageJson, detectInWorkspaceRootAbove startDir]
  where
    detectInWorkspaceRootAbove dir = do
      isAtRepositoryRoot <- isRepositoryRoot dir
      let parentDir = SP.parent dir
      if isAtRepositoryRoot || parentDir == dir
        then return Nothing
        else do
          maybeParentPackageJson <- readPackageJsonIfValid parentDir
          isParentWorkspaceRoot <- isWorkspaceRoot parentDir maybeParentPackageJson
          detectionInParent <-
            if isParentWorkspaceRoot
              then detectInDir parentDir maybeParentPackageJson
              else return Nothing
          maybe (detectInWorkspaceRootAbove parentDir) (return . Just) detectionInParent

detectInDir :: Path' Abs (Dir PackageRootDir) -> Maybe Value -> IO (Maybe Detection)
detectInDir dir maybePackageJson =
  firstJustM
    id
    [ return $ maybePackageJson >>= detectFromPackageJson (packageJsonPathIn dir),
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

packageJsonPathIn :: Path' Abs (Dir PackageRootDir) -> Path' Abs File'
packageJsonPathIn dir = dir </> [relfile|package.json|]

-- | 'Nothing' for an invalid package.json too, so the other files in the
-- directory still get checked.
readPackageJsonIfValid :: Path' Abs (Dir PackageRootDir) -> IO (Maybe Value)
readPackageJsonIfValid dir = do
  exists <- doesFileExist $ packageJsonPathIn dir
  if exists then readJsonFile $ packageJsonPathIn dir else return Nothing

-- | @.git@ is a directory in a normal clone and a file in a git worktree.
isRepositoryRoot :: Path' Abs (Dir PackageRootDir) -> IO Bool
isRepositoryRoot dir = doesPathExist $ SP.fromAbsFile $ dir </> [relfile|.git|]

isWorkspaceRoot :: Path' Abs (Dir PackageRootDir) -> Maybe Value -> IO Bool
isWorkspaceRoot dir maybePackageJson = do
  hasPnpmWorkspaceFile <- doesFileExist $ dir </> [relfile|pnpm-workspace.yaml|]
  return $ hasPnpmWorkspaceFile || maybe False hasWorkspacesField maybePackageJson
