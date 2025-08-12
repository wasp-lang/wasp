module ExternalConfig.Npm.TarballTest where

import Data.Maybe (fromJust)
import StrongPath (parseRelFile)
import Test.Tasty.Hspec
import qualified Wasp.ExternalConfig.Npm.Tarball as Tarball

spec_createTarballFilenamesCorrectly :: Spec
spec_createTarballFilenamesCorrectly = do
  describe "sanitizeForTarballFilename" $ do
    -- Test cases based on the allowed npm package name regex and expected
    -- values generated using `npm pack` command.
    itShouldSanitizePackageNameCorrectly "@scope/package" "scope-package"
    itShouldSanitizePackageNameCorrectly "@my-org/my-package" "my-org-my-package"
    itShouldSanitizePackageNameCorrectly "@scope/package-name.with.dots" "scope-package-name.with.dots"
    itShouldSanitizePackageNameCorrectly "@test-scope/test_package" "test-scope-test_package"
    itShouldSanitizePackageNameCorrectly "@*scope/package" "*scope-package"
    itShouldSanitizePackageNameCorrectly "@~scope/package" "~scope-package"
    itShouldSanitizePackageNameCorrectly "@scope*/package" "scope*-package"
    itShouldSanitizePackageNameCorrectly "lodash" "lodash"
    itShouldSanitizePackageNameCorrectly "my-package" "my-package"
    itShouldSanitizePackageNameCorrectly "package_name" "package_name"
    itShouldSanitizePackageNameCorrectly "package123" "package123"
    itShouldSanitizePackageNameCorrectly "some.package.name" "some.package.name"
    itShouldSanitizePackageNameCorrectly "~package" "~package"

  describe "makeTarballFilePath" $ do
    itShouldCreateValidTarballPath ("@wasp.sh/libs-auth", "1.0.0") "wasp.sh-libs-auth-1.0.0.tgz"
  where
    itShouldSanitizePackageNameCorrectly :: String -> String -> Spec
    itShouldSanitizePackageNameCorrectly input expected = do
      it ("sanitizes " ++ input) $ do
        Tarball.sanitizeForTarballFilename input `shouldBe` Tarball.SanitizedTarballName expected

    itShouldCreateValidTarballPath :: (String, String) -> String -> Spec
    itShouldCreateValidTarballPath (packageName, packageVersion) expected = do
      it ("pacakge " ++ packageName ++ " with version " ++ packageName) $ do
        Tarball.makeTarballFilePath (Tarball.sanitizeForTarballFilename packageName) packageVersion `shouldBe` fromJust (parseRelFile expected)
