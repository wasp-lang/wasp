{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Wasp.ExternalConfig.Npm.PackageJson
  ( PackageJson (..),
    BinField (..),
    WaspConfig (..),
    DependenciesMap,
    PackageName,
    PackageVersion,
    getDependencies,
    getDevDependencies,
    getOverriddenDeps,
    getBinPaths,
    PackageJsonFile,
    parsePackageJsonFile,
  )
where

import Data.Aeson (FromJSON (..), Value (..), decode)
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Either.Extra (maybeToEither)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import GHC.Generics (Generic)
import StrongPath (Abs, File, Path')
import Wasp.ExternalConfig.Npm.Dependency (Dependency)
import qualified Wasp.ExternalConfig.Npm.Dependency as D
import qualified Wasp.Util.IO as IOUtil

-- | Partial model of npm's package.json format.
-- Only the fields Wasp needs are included. Required/optional matches the
-- npm spec: https://docs.npmjs.com/cli/v10/configuring-npm/package-json
data PackageJson = PackageJson
  { name :: !String,
    dependencies :: !(Maybe DependenciesMap),
    devDependencies :: !(Maybe DependenciesMap),
    workspaces :: !(Maybe [String]),
    bin :: !(Maybe BinField),
    -- Wasp-specific extension, not part of the npm spec.
    wasp :: !(Maybe WaspConfig)
  }
  deriving (Show, Generic, FromJSON)

-- | The "bin" field in package.json can be a single path string
-- (e.g. @"./dist/run.js"@) or an object mapping command names to paths
-- (e.g. @{"my-cmd": "./dist/run.js"}@).
data BinField
  = BinString !FilePath
  | BinObject !(Map String FilePath)
  deriving (Show)

instance FromJSON BinField where
  parseJSON (String s) = pure $ BinString (T.unpack s)
  parseJSON (Object o) = BinObject . M.fromList <$> mapM parsePair (KeyMap.toList o)
    where
      parsePair (k, String v) = pure (Key.toString k, T.unpack v)
      parsePair (k, _) = fail $ "Expected string value for bin entry " ++ Key.toString k
  parseJSON _ = fail "Expected string or object for bin field"

-- | Configuration for Wasp-specific features in package.json.
data WaspConfig = WaspConfig
  { -- | Users can provide a map of dependencies they want to override. We
    -- require them to specify which existing Wasp-required version they want to
    -- override -- their desired version is instead defined in the
    -- `package.json#dependencies`. This ensures users consciously acknowledge
    -- they're deviating from tested versions, and must update their overrides
    -- when Wasp's requirements change.
    overriddenDeps :: !(Maybe DependenciesMap)
  }
  deriving (Show, Generic, FromJSON)

type DependenciesMap = Map PackageName PackageVersion

type PackageName = String

type PackageVersion = String

getDependencies :: PackageJson -> [Dependency]
getDependencies = maybe [] (D.fromList . M.toList) . dependencies

getDevDependencies :: PackageJson -> [Dependency]
getDevDependencies = maybe [] (D.fromList . M.toList) . devDependencies

getOverriddenDeps :: PackageJson -> [Dependency]
getOverriddenDeps pkgJson =
  maybe [] (D.fromList . M.toList) $ overriddenDeps =<< wasp pkgJson

-- | Returns the file paths referenced in the "bin" field.
getBinPaths :: PackageJson -> [FilePath]
getBinPaths pkgJson = case bin pkgJson of
  Just (BinString path) -> [path]
  Just (BinObject pathMap) -> M.elems pathMap
  Nothing -> []

class PackageJsonFile f

parsePackageJsonFile :: (PackageJsonFile f) => Path' Abs (File f) -> IO (Either String PackageJson)
parsePackageJsonFile packageJsonFile = do
  byteString <- IOUtil.readFileBytes packageJsonFile
  return $ maybeToEither "Error parsing the package.json file" $ decode byteString
