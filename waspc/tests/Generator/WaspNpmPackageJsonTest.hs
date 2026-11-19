module Generator.WaspNpmPackageJsonTest where

import qualified Data.Aeson as Aeson
import Data.List (isPrefixOf)
import qualified Data.Map as M
import Test.Hspec
import qualified Wasp.ExternalConfig.Npm.PackageJson as P

spec_waspNpmPackageJson :: Spec
spec_waspNpmPackageJson = do
  describe "packages/wasp/package.json" $ do
    -- A third-party package must be a peer dependency. npm can nest a regular
    -- dependency under node_modules/@wasp/wasp, where the generated code and
    -- the user's code don't find it.
    it "has only @wasp packages as regular dependencies" $ do
      packageJson <- readWaspNpmPackageJson
      M.keys (P.dependencies packageJson) `shouldSatisfy` all ("@wasp/" `isPrefixOf`)
  where
    readWaspNpmPackageJson :: IO P.PackageJson
    readWaspNpmPackageJson =
      either (error . ("Couldn't parse packages/wasp/package.json: " ++)) return
        =<< Aeson.eitherDecodeFileStrict' waspNpmPackageJsonPath

    -- Tests run with waspc/ as the working directory.
    waspNpmPackageJsonPath = "../packages/wasp/package.json"
