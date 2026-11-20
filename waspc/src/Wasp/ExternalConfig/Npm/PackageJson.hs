module Wasp.ExternalConfig.Npm.PackageJson
  ( PackageJson (..),
    DependenciesMap,
    PackageName,
    PackageVersion,
    PackageJsonFile,
    parsePackageJsonFile,
  )
where

import Data.Aeson (FromJSON, (.!=), (.:), (.:?))
import qualified Data.Aeson as Aeson
import Data.Either.Extra (maybeToEither)
import Data.Map (Map)
import qualified Data.Map as M
import StrongPath (Abs, File, Path')
import qualified Wasp.Util.IO as IOUtil

data PackageJson = PackageJson
  { name :: !String,
    dependencies :: !DependenciesMap,
    devDependencies :: !DependenciesMap,
    workspaces :: !(Maybe [String])
  }
  deriving (Show)

instance FromJSON PackageJson where
  parseJSON = Aeson.withObject "PackageJson" $ \v ->
    PackageJson
      <$> v .: "name"
      -- `dependencies` and `devDependencies` are both optional, so a missing
      -- field is read as an empty map rather than a parse error. This saves us
      -- from dealing with two "empty" states (Nothing + an empty map).
      <*> v .:? "dependencies" .!= M.empty
      <*> v .:? "devDependencies" .!= M.empty
      <*> v .:? "workspaces"

type DependenciesMap = Map PackageName PackageVersion

type PackageName = String

type PackageVersion = String

class PackageJsonFile f

parsePackageJsonFile :: (PackageJsonFile f) => Path' Abs (File f) -> IO (Either String PackageJson)
parsePackageJsonFile packageJsonFile = do
  byteString <- IOUtil.readFileBytes packageJsonFile
  return $ maybeToEither "Error parsing the package.json file" $ Aeson.decode byteString
