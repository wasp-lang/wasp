module Generator.Valid.PackageJson.WaspConfigTest where

import qualified Data.Map as M
import Fixtures.AppSpec (basicAppSpec)
import Test.Hspec
import qualified Wasp.AppSpec as AS
import qualified Wasp.AppSpec.App as AS.App
import qualified Wasp.AppSpec.Core.Decl as AS.Decl
import qualified Wasp.ExternalConfig.Npm.PackageJson as P
import Wasp.Generator.Valid.PackageJson.Common (getGeneratedServerDeps)
import Wasp.Generator.Valid.PackageJson.WaspConfig (waspConfigValidator)
import qualified Wasp.Validator as V

spec_waspConfigValidator :: Spec
spec_waspConfigValidator = do
  describe "wasp.overriddenDeps" $ do
    it "accepts a generated server dependency at the version Wasp requires" $ do
      validateOverriddenDeps [(serverDepName, serverDepVersion)] `shouldBe` []

    it "rejects a generated server dependency at another version" $ do
      validateOverriddenDeps [(serverDepName, "0.0.0-not-the-required-version")]
        `shouldBe` [ ( ["wasp", "overriddenDeps", serverDepName],
                       "Wasp requires version " ++ serverDepVersion ++ ", but found an override for version 0.0.0-not-the-required-version."
                     )
                   ]

    it "rejects a package that comes with @wasp/wasp, and points to npm's overrides" $ do
      validateOverriddenDeps [("react", "^19.2.1")]
        `shouldBe` [(["wasp", "overriddenDeps", "react"], notOverridableDepMessage)]

    it "rejects a package Wasp doesn't use" $ do
      validateOverriddenDeps [("not-a-wasp-dependency", "1.0.0")]
        `shouldBe` [(["wasp", "overriddenDeps", "not-a-wasp-dependency"], notOverridableDepMessage)]
  where
    validateOverriddenDeps :: [(P.PackageName, P.PackageVersion)] -> [([String], String)]
    validateOverriddenDeps overriddenDeps =
      [ (V.fieldPath validationError, V.message validationError)
      | validationError <- V.execValidator (waspConfigValidator appSpec) (packageJsonWithOverriddenDeps overriddenDeps)
      ]

    packageJsonWithOverriddenDeps overriddenDeps =
      (AS.packageJson appSpec)
        { P.wasp = Just $ P.WaspConfig {P.overriddenDeps = Just $ M.fromList overriddenDeps}
        }

    (serverDepName, serverDepVersion) = M.findMin $ getGeneratedServerDeps appSpec

    appSpec = basicAppSpec {AS.decls = [AS.Decl.makeDecl app]}

    app =
      AS.App.App
        { AS.App.name = "TestApp",
          AS.App.title = "Test App",
          AS.App.deployment = Nothing,
          AS.App.db = Nothing,
          AS.App.server = Nothing,
          AS.App.client = Nothing,
          AS.App.auth = Nothing,
          AS.App.head = Nothing,
          AS.App.emailSender = Nothing,
          AS.App.webSocket = Nothing
        }

    notOverridableDepMessage =
      "`wasp.overriddenDeps` only takes the dependencies of the generated server, which `wasp deps` lists. For a package that comes with @wasp/wasp, use npm's `overrides` field in package.json instead."
