module Wasp.Project.PackageManager
  ( PackageManager (..),
    supportedPackageManagers,
    getPackageManagerName,
    parsePackageManagerName,
    getLockfileName,
    getSettingsFileNames,
    pnpmWorkspaceFileName,
    detectPackageManager,
    detectPackageManagerOrDefault,
    readPinnedPackageManagerVersion,
    defaultPackageManager,
    UnsupportedPackageManagerError (..),
    showUnsupportedPackageManagerError,
    makeInstallCommand,
    makeLockedInstallCommand,
    makeAddPackagesCommand,
    makeRunScriptCommand,
    DependencyType (..),

    -- * Exported for testing
    PackageManagerEvidence (..),
    choosePackageManager,
  )
where

import Control.Monad (filterM)
import Control.Monad.Extra (firstJustM)
import qualified Data.Aeson as Aeson
import Data.Either (fromRight)
import Data.List (find, intercalate)
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
import StrongPath (Abs, Dir, Path', fromAbsDir)
import System.Directory (doesFileExist)
import qualified System.FilePath as FP
import Wasp.Project.Common (WaspProjectDir)

-- | The package manager that installs a Wasp project's packages. Wasp leaves
-- installing to the user, but it has to know which one the project uses: for
-- the commands it tells the user to run, for the lockfile, and for the
-- Dockerfile.
data PackageManager = Npm | Pnpm | Bun
  deriving (Eq, Show, Enum, Bounded)

supportedPackageManagers :: [PackageManager]
supportedPackageManagers = [minBound .. maxBound]

defaultPackageManager :: PackageManager
defaultPackageManager = Npm

-- This module is where the package managers' names live, and it doesn't run
-- them, so the rule against writing out npm's name doesn't apply.
{- HLINT ignore getPackageManagerName "Avoid npm and npx" -}

getPackageManagerName :: PackageManager -> String
getPackageManagerName = \case
  Npm -> "npm"
  Pnpm -> "pnpm"
  Bun -> "bun"

parsePackageManagerName :: String -> Maybe PackageManager
parsePackageManagerName name = find ((== name) . getPackageManagerName) supportedPackageManagers

-- | The lockfile that the package manager writes.
getLockfileName :: PackageManager -> FilePath
getLockfileName = \case
  Npm -> "package-lock.json"
  Pnpm -> "pnpm-lock.yaml"
  Bun -> "bun.lock"

-- | The files next to package.json, other than the lockfile, that change what
-- the package manager installs.
getSettingsFileNames :: PackageManager -> [FilePath]
getSettingsFileNames = \case
  Npm -> []
  -- Holds the overrides and which packages may run install scripts.
  Pnpm -> [pnpmWorkspaceFileName]
  Bun -> []

pnpmWorkspaceFileName :: FilePath
pnpmWorkspaceFileName = "pnpm-workspace.yaml"

-- | The files that tell a dir is installed with the package manager.
getMarkerFileNames :: PackageManager -> [FilePath]
getMarkerFileNames packageManager =
  getLockfileName packageManager : case packageManager of
    Npm -> []
    -- A pnpm workspace root has this file even before the first install
    -- writes the lockfile.
    Pnpm -> [pnpmWorkspaceFileName]
    -- The lockfile of Bun versions before 1.2.
    Bun -> ["bun.lockb"]

-- | A package manager that the project uses, but Wasp doesn't work with.
newtype UnsupportedPackageManagerError = UnsupportedPackageManagerError {unsupportedPackageManagerName :: String}
  deriving (Eq, Show)

showUnsupportedPackageManagerError :: UnsupportedPackageManagerError -> String
showUnsupportedPackageManagerError (UnsupportedPackageManagerError name) =
  "Your project uses "
    ++ name
    ++ ", but Wasp only works with "
    ++ intercalate ", " (getPackageManagerName <$> supportedPackageManagers)
    ++ "."

-- | What a project says about its package manager.
data PackageManagerEvidence = PackageManagerEvidence
  { -- | The name in the `packageManager` field of the project's package.json,
    -- e.g. "pnpm" for "pnpm@10.0.0".
    packageManagerFieldName :: Maybe String,
    -- | The names of the package managers' marker files in the nearest dir
    -- that has any. For a workspace member that is its workspace root.
    markerFileNamesInNearestDir :: [FilePath]
  }

-- | Finds the project's package manager the way package managers do it
-- themselves: by the `packageManager` field of its package.json, then by the
-- nearest lockfile, and npm otherwise.
detectPackageManager :: Path' Abs (Dir WaspProjectDir) -> IO (Either UnsupportedPackageManagerError PackageManager)
detectPackageManager waspProjectDir = do
  fieldName <- fmap pinnedName <$> readPackageManagerField waspProjectDir
  markerFileNames <- fromMaybe [] <$> firstJustM findMarkerFileNames (getDirAndItsAncestors projectDir)
  return $
    choosePackageManager
      PackageManagerEvidence
        { packageManagerFieldName = fieldName,
          markerFileNamesInNearestDir = markerFileNames
        }
  where
    projectDir = fromAbsDir waspProjectDir

    findMarkerFileNames :: FilePath -> IO (Maybe [FilePath])
    findMarkerFileNames dir = do
      existingMarkerFileNames <- filterM (doesFileExist . (dir FP.</>)) knownMarkerFileNames
      return $ if null existingMarkerFileNames then Nothing else Just existingMarkerFileNames

    getDirAndItsAncestors dir =
      let parentDir = FP.takeDirectory dir
       in if parentDir == dir then [dir] else dir : getDirAndItsAncestors parentDir

-- | For telling the user which command to run, when a wrong guess does less
-- harm than no command at all.
detectPackageManagerOrDefault :: Path' Abs (Dir WaspProjectDir) -> IO PackageManager
detectPackageManagerOrDefault waspProjectDir =
  fromRight defaultPackageManager <$> detectPackageManager waspProjectDir

choosePackageManager :: PackageManagerEvidence -> Either UnsupportedPackageManagerError PackageManager
choosePackageManager evidence =
  case (packageManagerFieldName evidence, markerFileNamesInNearestDir evidence) of
    (Just fieldName, _) -> maybe (Left $ UnsupportedPackageManagerError fieldName) Right $ parsePackageManagerName fieldName
    (Nothing, []) -> Right defaultPackageManager
    (Nothing, markerFileNames) ->
      maybe (Left $ UnsupportedPackageManagerError yarnName) Right
        $ listToMaybe
        $ mapMaybe (findPackageManagerOfAnyMarkerFile markerFileNames) packageManagersByMarkerFilePriority
  where
    findPackageManagerOfAnyMarkerFile markerFileNames packageManager
      | any (`elem` getMarkerFileNames packageManager) markerFileNames = Just packageManager
      | otherwise = Nothing

    -- A project that moved off npm often still has npm's lockfile around.
    packageManagersByMarkerFilePriority = [Pnpm, Bun, Npm]

knownMarkerFileNames :: [FilePath]
knownMarkerFileNames = yarnLockfileName : concatMap getMarkerFileNames supportedPackageManagers

-- | Wasp doesn't work with Yarn: its default mode installs no `node_modules`.
-- We know its lockfile, so we can tell the user instead of assuming npm.
yarnLockfileName :: FilePath
yarnLockfileName = "yarn.lock"

yarnName :: String
yarnName = "yarn"

-- | The version that the project pins its package manager to, if any.
readPinnedPackageManagerVersion :: Path' Abs (Dir WaspProjectDir) -> IO (Maybe String)
readPinnedPackageManagerVersion waspProjectDir = (>>= pinnedVersion) <$> readPackageManagerField waspProjectDir

-- | What the `packageManager` field of a package.json says, e.g.
-- "pnpm@10.0.0+sha512.abc".
data PinnedPackageManager = PinnedPackageManager
  { pinnedName :: String,
    pinnedVersion :: Maybe String
  }

readPackageManagerField :: Path' Abs (Dir WaspProjectDir) -> IO (Maybe PinnedPackageManager)
readPackageManagerField waspProjectDir = do
  doesPackageJsonExist <- doesFileExist packageJsonFile
  if doesPackageJsonExist
    then (>>= fmap parseFieldValue . packageManagerFieldValue) <$> Aeson.decodeFileStrict' packageJsonFile
    else return Nothing
  where
    packageJsonFile = fromAbsDir waspProjectDir FP.</> "package.json"

    parseFieldValue :: String -> PinnedPackageManager
    parseFieldValue fieldValue =
      let (name, versionWithSeparator) = break (== '@') fieldValue
          -- A hash of the package manager's files can follow the version.
          version = takeWhile (/= '+') $ drop 1 versionWithSeparator
       in PinnedPackageManager {pinnedName = name, pinnedVersion = if null version then Nothing else Just version}

newtype PackageManagerField = PackageManagerField {packageManagerFieldValue :: Maybe String}

instance Aeson.FromJSON PackageManagerField where
  parseJSON = Aeson.withObject "PackageJson" $ \packageJson ->
    PackageManagerField <$> packageJson Aeson..:? "packageManager"

-- | The command that installs the project's packages.
makeInstallCommand :: PackageManager -> String
makeInstallCommand packageManager = getPackageManagerName packageManager ++ " install"

-- | The command that installs exactly what the lockfile says, and fails when
-- the lockfile doesn't match the package.json.
makeLockedInstallCommand :: PackageManager -> String
makeLockedInstallCommand = \case
  Npm -> getPackageManagerName Npm ++ " ci"
  Pnpm -> makeInstallCommand Pnpm ++ " --frozen-lockfile"
  Bun -> makeInstallCommand Bun ++ " --frozen-lockfile"

data DependencyType = Runtime | Development
  deriving (Eq, Show)

-- | The command that adds the packages, given like `name@version`, to the
-- project's package.json and installs them.
makeAddPackagesCommand :: PackageManager -> DependencyType -> [String] -> String
makeAddPackagesCommand packageManager dependencyType packageSpecifiers =
  unwords $ [getPackageManagerName packageManager, subcommand] ++ developmentFlag ++ packageSpecifiers
  where
    subcommand = case packageManager of
      Npm -> "install"
      Pnpm -> "add"
      Bun -> "add"

    developmentFlag = case (dependencyType, packageManager) of
      (Runtime, _) -> []
      (Development, Bun) -> ["--dev"]
      (Development, _) -> ["--save-dev"]

-- | The command that runs a script of the project's package.json with the
-- arguments.
makeRunScriptCommand :: PackageManager -> String -> [String] -> String
makeRunScriptCommand packageManager scriptName scriptArgs =
  unwords $ [getPackageManagerName packageManager, "run", scriptName] ++ separator ++ scriptArgs
  where
    -- npm keeps the flags that come before `--` for itself.
    separator = [argsSeparator | packageManager == Npm && not (null scriptArgs)]
    argsSeparator = "--"
