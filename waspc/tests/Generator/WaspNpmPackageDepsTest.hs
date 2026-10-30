module Generator.WaspNpmPackageDepsTest where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Map as M
import qualified Data.Text as T
import Test.Hspec
import qualified Wasp.ExternalConfig.Npm.Dependency as D
import qualified Wasp.ExternalConfig.Npm.PackageJson as P
import Wasp.Generator.Valid.PackageJson.Common (requiredUserDevelopmentDeps, requiredUserRuntimeDeps)
import Wasp.Generator.WaspNpmPackageDeps (waspNpmPackageDeps)

-- The packages @wasp/wasp declares live in two places: its package.json (what
-- npm installs) and the Haskell lists (what the generator and the package.json
-- validator assume is installed). This keeps them from drifting.
spec_waspNpmPackageDeps :: Spec
spec_waspNpmPackageDeps = do
  describe "waspNpmPackageDeps" $ do
    -- Everything a project gets through @wasp/wasp is a peer dependency: the
    -- core deps the generated code imports and the packages the project owns.
    -- npm installs peers at the project root and refuses conflicting versions,
    -- which is what makes bare imports in the generated code resolve to them.
    it "and the user's required deps are the peer dependencies in packages/wasp/package.json" $ do
      packageJson <-
        either (error . ("Couldn't parse packages/wasp/package.json: " ++)) return
          =<< Aeson.eitherDecodeFileStrict' waspNpmPackageJsonPath
      let peerDepsInPackageJson = getStringMap "peerDependencies" packageJson
          coreDeps = M.fromList [(D.name dep, D.version dep) | dep <- waspNpmPackageDeps]
          requiredUserDeps = M.fromList $ requiredUserRuntimeDeps <> requiredUserDevelopmentDeps
      peerDepsInPackageJson `shouldBe` M.union coreDeps requiredUserDeps

    it "is not duplicated in the dependencies of packages/wasp/package.json" $ do
      packageJson <-
        either (error . ("Couldn't parse packages/wasp/package.json: " ++)) return
          =<< Aeson.eitherDecodeFileStrict' waspNpmPackageJsonPath
      M.keys (P.dependencies packageJson) `shouldBe` waspOwnPackages
  where
    getStringMap :: String -> Aeson.Value -> M.Map String String
    getStringMap field (Aeson.Object obj) =
      case KM.lookup (Key.fromString field) obj of
        Just (Aeson.Object deps) -> M.fromList [(Key.toString k, T.unpack v) | (k, Aeson.String v) <- KM.toList deps]
        _ -> M.empty
    getStringMap _ _ = M.empty

    -- Tests run with waspc/ as the working directory.
    waspNpmPackageJsonPath = "../packages/wasp/package.json"

    -- Pinned to the Wasp version by `pnpm pack`, not part of the core deps.
    waspOwnPackages = ["@wasp/cli", "@wasp/deploy", "@wasp/spec", "@wasp/studio"]
