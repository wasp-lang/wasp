module Generator.Valid.PackageJson.DependenciesTest where

import qualified Data.Map as M
import Test.Hspec
import qualified Wasp.ExternalConfig.Npm.Dependency as Npm.Dependency
import qualified Wasp.ExternalConfig.Npm.PackageJson as P
import Wasp.Generator.OptionalNpmDeps (FeatureNpmDeps (..))
import Wasp.Generator.Valid.PackageJson.Dependencies (featureNpmDepsValidator)
import qualified Wasp.Validator as V

spec_featureNpmDepsValidator :: Spec
spec_featureNpmDepsValidator = do
  describe "featureNpmDepsValidator" $ do
    it "succeeds when the project lists every package of the feature" $ do
      validate (packageJsonWith [("mailer", "^1.0.0")] [("@types/mailer", "^1.0.0")]) `shouldBe` []

    it "accepts a package in either dependency field" $ do
      validate (packageJsonWith [("@types/mailer", "^1.0.0")] [("mailer", "^1.0.0")]) `shouldBe` []

    it "doesn't check the versions, since npm does" $ do
      validate (packageJsonWith [("mailer", "0.0.1")] [("@types/mailer", "0.0.1")]) `shouldBe` []

    it "names the missing packages and how to install them" $ do
      validate (packageJsonWith [] [])
        `shouldBe` [ "Your app uses email, so it needs mailer, @types/mailer."
                       ++ " Run `npm install \"mailer@^1.0.0\"` and `npm install --save-dev \"@types/mailer@^1.0.0\"`."
                   ]

    it "only installs what is missing" $ do
      validate (packageJsonWith [("mailer", "^1.0.0")] [])
        `shouldBe` ["Your app uses email, so it needs @types/mailer. Run `npm install --save-dev \"@types/mailer@^1.0.0\"`."]
  where
    validate = map V.message . V.execValidator (featureNpmDepsValidator emailNpmDeps)

    emailNpmDeps =
      FeatureNpmDeps
        { featureName = "email",
          isFeatureUsed = const True,
          dependencies = Npm.Dependency.fromList [("mailer", "^1.0.0")],
          devDependencies = Npm.Dependency.fromList [("@types/mailer", "^1.0.0")]
        }

    packageJsonWith deps devDeps =
      P.PackageJson
        { P.name = "test-app",
          P.dependencies = M.fromList deps,
          P.devDependencies = M.fromList devDeps,
          P.workspaces = Nothing
        }
