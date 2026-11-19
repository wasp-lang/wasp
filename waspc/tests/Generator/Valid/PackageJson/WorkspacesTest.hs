module Generator.Valid.PackageJson.WorkspacesTest where

import qualified Data.Map as M
import Test.Hspec
import qualified Wasp.ExternalConfig.Npm.PackageJson as P
import Wasp.Generator.Valid.PackageJson.Workspaces (workspacesValidator)
import qualified Wasp.Validator as V

spec_workspacesValidator :: Spec
spec_workspacesValidator = do
  describe "workspacesValidator" $ do
    it "succeeds without workspaces" $ do
      validate Nothing `shouldBe` []

    it "succeeds with the user's own workspaces" $ do
      validate (Just ["packages/*", "tools/wasp-helper"]) `shouldBe` []

    it "fails when the generated code is still a workspace" $ do
      validate (Just [".wasp/out/*", "packages/*", ".wasp/out/sdk/wasp"])
        `shouldBe` ["Wasp's generated code is no longer an npm workspace. Remove \".wasp/out/*\", \".wasp/out/sdk/wasp\" from \"workspaces\"."]

    it "fails for any path inside .wasp" $ do
      validate (Just ["./.wasp/build/*"])
        `shouldBe` ["Wasp's generated code is no longer an npm workspace. Remove \"./.wasp/build/*\" from \"workspaces\"."]
  where
    validate workspaces = V.message <$> V.execValidator workspacesValidator (packageJsonWith workspaces)

    packageJsonWith workspaces =
      P.PackageJson
        { P.name = "test-app",
          P.dependencies = M.empty,
          P.devDependencies = M.empty,
          P.workspaces = workspaces
        }
