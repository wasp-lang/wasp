module ExternalConfig.Npm.TarballTest where

import Test.Hspec
import Wasp.ExternalConfig.Npm.Tarball (TarballFilename (TarballFilename))
import qualified Wasp.ExternalConfig.Npm.Tarball as Tarball

spec_createTarballFilenamesCorrectly :: Spec
spec_createTarballFilenamesCorrectly = do
  describe "packageNameToTarballPrefix" $ do
    -- Test cases based on the allowed npm package name regex and expected
    -- values generated using `npm pack` command.
    "@scope/package" ~> "scope-package"
    "@my-org/my-package" ~> "my-org-my-package"
    "@scope/package-name.with.dots" ~> "scope-package-name.with.dots"
    "@test-scope/test_package" ~> "test-scope-test_package"
    "@*scope/package" ~> "*scope-package"
    "@~scope/package" ~> "~scope-package"
    "@scope*/package" ~> "scope*-package"
    "lodash" ~> "lodash"
    "my-package" ~> "my-package"
    "package_name" ~> "package_name"
    "package123" ~> "package123"
    "some.package.name" ~> "some.package.name"
    "~package" ~> "~package"

  describe "makeTarballFilename" $ do
    itShouldCreateValidTarballPath ("@wasp.sh/lib-auth", "1.0.0") "wasp.sh-lib-auth-1.0.0.tgz"
  where
    (~>) :: String -> String -> Spec
    (~>) input expected = do
      it ("sanitizes " ++ input) $ do
        Tarball.packageNameToTarballPrefix input `shouldBe` expected

    itShouldCreateValidTarballPath :: (String, String) -> String -> Spec
    itShouldCreateValidTarballPath (packageName, packageVersion) expected = do
      it ("package " ++ packageName ++ " with version " ++ packageName) $ do
        Tarball.makeTarballFilename packageName packageVersion `shouldBe` TarballFilename expected
