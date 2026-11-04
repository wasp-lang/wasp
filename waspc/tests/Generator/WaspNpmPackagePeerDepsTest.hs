module Generator.WaspNpmPackagePeerDepsTest where

import Data.Aeson ((.:))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.Map as M
import Test.Hspec
import qualified Wasp.ExternalConfig.Npm.Dependency as D
import qualified Wasp.ExternalConfig.Npm.PackageJson as P
import Wasp.Generator.WaspNpmPackagePeerDeps (waspNpmPackagePeerDeps)

-- Ensures that the peer dependencies in packages/wasp/package.json and
-- `waspNpmPackagePeerDeps` are the same.
spec_waspNpmPackagePeerDeps :: Spec
spec_waspNpmPackagePeerDeps = do
  describe "waspNpmPackagePeerDeps" $ do
    it "are the peer dependencies in packages/wasp/package.json" $ do
      packageJson <- readWaspNpmPackageJson
      let peerDepsInPackageJson =
            either (error . ("Couldn't read peerDependencies: " ++)) id $
              Aeson.parseEither (Aeson.withObject "package.json" (.: "peerDependencies")) packageJson
      peerDepsInPackageJson `shouldBe` M.fromList [(D.name dep, D.version dep) | dep <- waspNpmPackagePeerDeps]

    it "leaves only the @wasp packages in the dependencies of packages/wasp/package.json" $ do
      packageJson <- readWaspNpmPackageJson
      M.keys (P.dependencies packageJson) `shouldBe` waspOwnPackages
  where
    readWaspNpmPackageJson :: (Aeson.FromJSON a) => IO a
    readWaspNpmPackageJson =
      either (error . ("Couldn't parse packages/wasp/package.json: " ++)) return
        =<< Aeson.eitherDecodeFileStrict' waspNpmPackageJsonPath

    -- Tests run with waspc/ as the working directory.
    waspNpmPackageJsonPath = "../packages/wasp/package.json"

    -- Regular dependencies. `pnpm pack` pins them to the Wasp version.
    waspOwnPackages = ["@wasp/cli", "@wasp/deploy", "@wasp/spec", "@wasp/studio"]
