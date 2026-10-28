module Generator.WaspNpmPackageDepsTest where

import qualified Data.Aeson as Aeson
import qualified Data.Map as M
import Test.Hspec
import qualified Wasp.ExternalConfig.Npm.Dependency as D
import qualified Wasp.ExternalConfig.Npm.PackageJson as P
import Wasp.Generator.WaspNpmPackageDeps (waspNpmPackageDeps)

-- The dependencies @wasp/wasp declares live in two places: its package.json
-- (what npm installs) and 'waspNpmPackageDeps' (what the generator and the
-- package.json validator assume is installed). This keeps them from drifting.
spec_waspNpmPackageDeps :: Spec
spec_waspNpmPackageDeps = do
  describe "waspNpmPackageDeps" $ do
    it "matches the dependencies in packages/wasp/package.json" $ do
      packageJson <-
        either (error . ("Couldn't parse packages/wasp/package.json: " ++)) return
          =<< Aeson.eitherDecodeFileStrict' waspNpmPackageJsonPath
      let depsInPackageJson = M.filterWithKey (\name _ -> name `notElem` waspOwnPackages) (P.dependencies packageJson)
          depsInHaskell = M.fromList [(D.name dep, D.version dep) | dep <- waspNpmPackageDeps]
      depsInPackageJson `shouldBe` depsInHaskell
  where
    -- Tests run with waspc/ as the working directory.
    waspNpmPackageJsonPath = "../packages/wasp/package.json"

    -- Pinned to the Wasp version by `pnpm pack`, not part of the core deps.
    waspOwnPackages = ["@wasp/cli", "@wasp/spec"]
