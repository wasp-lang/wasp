module Generator.Valid.PackageJsonTest (spec_PackageJson) where

import qualified Data.Map as M
import Test.Hspec
import qualified Wasp.ExternalConfig.Npm.PackageJson as P
import Wasp.Generator.Valid.PackageJson
  ( DependencyType (..),
    inDependency,
    makeOptionalDepValidator,
    makeRequiredDepValidator,
  )
import qualified Wasp.Generator.Valid.Validator as V

spec_PackageJson :: Spec
spec_PackageJson = do
  describe "makeOptionalDepValidator" $ do
    it "succeeds when optional dependency is not present" $ do
      let pkgJson =
            makePackageJson
              []
              []

      let validator = makeOptionalDepValidator Runtime ("optional-pkg", "1.0.0")
      V.execValidator validator pkgJson `shouldBe` []

    it "succeeds when optional dependency has correct version" $ do
      let pkgJson =
            makePackageJson
              [("optional-pkg", "1.0.0")]
              []

      let validator = makeOptionalDepValidator Runtime ("optional-pkg", "1.0.0")
      V.execValidator validator pkgJson `shouldBe` []

    it "fails when optional dependency has wrong version" $ do
      let pkgJson =
            makePackageJson
              [("optional-pkg", "2.0.0")]
              []

      let validator = makeOptionalDepValidator Runtime ("optional-pkg", "1.0.0")
      let errors = V.message <$> V.execValidator validator pkgJson
      errors `shouldBe` ["Wasp requires package \"optional-pkg\" to be version \"1.0.0\" if present."]

    it "works with development dependencies" $ do
      let pkgJson =
            makePackageJson
              []
              [("dev-pkg", "3.0.0")]

      let validator = makeOptionalDepValidator Development ("dev-pkg", "3.0.0")
      V.execValidator validator pkgJson `shouldBe` []

    it "sets correct field path for runtime dependencies" $ do
      let pkgJson =
            makePackageJson
              [("optional-pkg", "2.0.0")]
              []

      let validator = makeOptionalDepValidator Runtime ("optional-pkg", "1.0.0")
      let errors = V.execValidator validator pkgJson
      V.fieldPath (head errors) `shouldBe` ["dependencies", "optional-pkg"]

    it "sets correct field path for development dependencies" $ do
      let pkgJson =
            makePackageJson
              []
              [("dev-pkg", "2.0.0")]

      let validator = makeOptionalDepValidator Development ("dev-pkg", "1.0.0")
      let errors = V.execValidator validator pkgJson
      V.fieldPath (head errors) `shouldBe` ["devDependencies", "dev-pkg"]

  describe "makeRequiredDepValidator" $ do
    it "succeeds when required dependency is present with correct version" $ do
      let pkgJson =
            makePackageJson
              [("required-pkg", "1.0.0")]
              []

      let validator = makeRequiredDepValidator Runtime ("required-pkg", "1.0.0")
      V.execValidator validator pkgJson `shouldBe` []

    it "fails when required dependency is missing" $ do
      let pkgJson =
            makePackageJson
              []
              []

      let validator = makeRequiredDepValidator Runtime ("required-pkg", "1.0.0")
      let errors = V.message <$> V.execValidator validator pkgJson
      errors `shouldBe` ["Wasp requires package \"required-pkg\" with version \"1.0.0\"."]

    it "fails when required dependency has wrong version" $ do
      let pkgJson =
            makePackageJson
              [("required-pkg", "2.0.0")]
              []

      let validator = makeRequiredDepValidator Runtime ("required-pkg", "1.0.0")
      let errors = V.message <$> V.execValidator validator pkgJson
      errors `shouldBe` ["Wasp requires package \"required-pkg\" to be version \"1.0.0\"."]

    it "fails when required runtime dependency is in devDependencies" $ do
      let pkgJson =
            makePackageJson
              []
              [("required-pkg", "1.0.0")]

      let validator = makeRequiredDepValidator Runtime ("required-pkg", "1.0.0")
      let errors = V.message <$> V.execValidator validator pkgJson
      errors `shouldBe` ["Wasp requires package \"required-pkg\" to be in \"dependencies\"."]

    it "fails when required dev dependency is in dependencies" $ do
      let pkgJson =
            makePackageJson
              [("dev-tool", "1.0.0")]
              []

      let validator = makeRequiredDepValidator Development ("dev-tool", "1.0.0")
      let errors = V.message <$> V.execValidator validator pkgJson
      errors `shouldBe` ["Wasp requires package \"dev-tool\" to be in \"devDependencies\"."]

    it "works with development dependencies" $ do
      let pkgJson =
            makePackageJson
              []
              [("dev-tool", "2.0.0")]

      let validator = makeRequiredDepValidator Development ("dev-tool", "2.0.0")
      V.execValidator validator pkgJson `shouldBe` []

    it "sets correct field path for runtime dependencies" $ do
      let pkgJson =
            makePackageJson
              [("required-pkg", "2.0.0")]
              []

      let validator = makeRequiredDepValidator Runtime ("required-pkg", "1.0.0")
      let errors = V.execValidator validator pkgJson
      V.fieldPath (head errors) `shouldBe` ["dependencies", "required-pkg"]

    it "sets correct field path for development dependencies" $ do
      let pkgJson =
            makePackageJson
              []
              [("dev-tool", "2.0.0")]

      let validator = makeRequiredDepValidator Development ("dev-tool", "1.0.0")
      let errors = V.execValidator validator pkgJson
      V.fieldPath (head errors) `shouldBe` ["devDependencies", "dev-tool"]

  describe "inDependency" $ do
    it "runs validator on runtime dependency value" $ do
      let pkgJson =
            makePackageJson
              [("test-pkg", "1.0.0")]
              []

      let validator = inDependency Runtime ("test-pkg", "1.0.0") $ V.eqJust "1.0.0"
      V.execValidator validator pkgJson `shouldBe` []

    it "runs validator on development dependency value" $ do
      let pkgJson =
            makePackageJson
              []
              [("test-pkg", "2.0.0")]

      let validator = inDependency Development ("test-pkg", "2.0.0") $ V.eqJust "2.0.0"
      V.execValidator validator pkgJson `shouldBe` []

    it "passes Nothing when dependency is missing" $ do
      let pkgJson =
            makePackageJson
              []
              []

      let validator = inDependency Runtime ("missing-pkg", "1.0.0") $ \case
            Nothing -> V.success
            Just _ -> V.failure "Should be Nothing"
      V.execValidator validator pkgJson `shouldBe` []

    it "sets correct field path with nested fields" $ do
      let pkgJson =
            makePackageJson
              [("test-pkg", "1.0.0")]
              []

      let validator = inDependency Runtime ("test-pkg", "1.0.0") $ \_ -> V.failure "test error"
      let errors = V.execValidator validator pkgJson
      V.fieldPath (head errors) `shouldBe` ["dependencies", "test-pkg"]

    it "distinguishes between runtime and development dependencies" $ do
      let pkgJson =
            makePackageJson
              [("pkg-a", "1.0.0")]
              [("pkg-b", "2.0.0")]

      let runtimeValidator = inDependency Runtime ("pkg-a", "1.0.0") $ V.eqJust "1.0.0"
      let devValidator = inDependency Development ("pkg-b", "2.0.0") $ V.eqJust "2.0.0"
      V.execValidator runtimeValidator pkgJson `shouldBe` []
      V.execValidator devValidator pkgJson `shouldBe` []

    it "looks in correct dependency type only" $ do
      let pkgJson =
            makePackageJson
              [("pkg", "1.0.0")]
              []

      -- Looking for pkg in devDependencies (where it doesn't exist)
      let validator = inDependency Development ("pkg", "1.0.0") $ \case
            Nothing -> V.success
            Just _ -> V.failure "Should not find in devDependencies"
      V.execValidator validator pkgJson `shouldBe` []

makePackageJson :: [(String, String)] -> [(String, String)] -> P.PackageJson
makePackageJson deps devDeps =
  P.PackageJson
    { P.name = "test-package",
      P.dependencies = M.fromList deps,
      P.devDependencies = M.fromList devDeps,
      P.workspaces = Nothing
    }
