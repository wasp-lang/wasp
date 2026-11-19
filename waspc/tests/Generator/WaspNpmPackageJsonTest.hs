module Generator.WaspNpmPackageJsonTest where

import Data.Aeson ((.!=), (.:), (.:?))
import qualified Data.Aeson as Aeson
import Data.List (isPrefixOf, sort)
import qualified Data.Map as M
import Test.Hspec
import qualified Wasp.ExternalConfig.Npm.Dependency as Npm.Dependency
import Wasp.Generator.OptionalNpmDeps (allOptionalNpmDeps)

spec_waspNpmPackageJson :: Spec
spec_waspNpmPackageJson = do
  describe "packages/wasp/package.json" $ do
    -- A third-party package must be a peer dependency. npm can nest a regular
    -- dependency under node_modules/@wasp/wasp, where the generated code and
    -- the user's code don't find it.
    it "has only @wasp packages as regular dependencies" $ do
      packageJson <- readWaspNpmPackageJson
      M.keys (dependencies packageJson) `shouldSatisfy` all ("@wasp/" `isPrefixOf`)

    -- npm checks the versions of the packages that a feature needs, and
    -- waspc tells the user to install them at these versions.
    it "lists the packages of every feature as optional peer dependencies" $ do
      packageJson <- readWaspNpmPackageJson
      let optionalPeerDeps = M.restrictKeys (peerDependencies packageJson) (M.keysSet $ M.filter optional $ peerDependenciesMeta packageJson)
      sort (M.toList optionalPeerDeps) `shouldBe` sort (Npm.Dependency.toPair <$> allOptionalNpmDeps)
  where
    readWaspNpmPackageJson :: IO WaspNpmPackageJson
    readWaspNpmPackageJson =
      either (error . ("Couldn't parse packages/wasp/package.json: " ++)) return
        =<< Aeson.eitherDecodeFileStrict' waspNpmPackageJsonPath

    -- Tests run with waspc/ as the working directory.
    waspNpmPackageJsonPath = "../packages/wasp/package.json"

data WaspNpmPackageJson = WaspNpmPackageJson
  { dependencies :: M.Map String String,
    peerDependencies :: M.Map String String,
    peerDependenciesMeta :: M.Map String PeerDependencyMeta
  }

instance Aeson.FromJSON WaspNpmPackageJson where
  parseJSON = Aeson.withObject "WaspNpmPackageJson" $ \v ->
    WaspNpmPackageJson
      <$> v .: "dependencies"
      <*> v .: "peerDependencies"
      <*> v .:? "peerDependenciesMeta" .!= M.empty

newtype PeerDependencyMeta = PeerDependencyMeta {optional :: Bool}

instance Aeson.FromJSON PeerDependencyMeta where
  parseJSON = Aeson.withObject "PeerDependencyMeta" $ \v ->
    PeerDependencyMeta <$> v .:? "optional" .!= False
