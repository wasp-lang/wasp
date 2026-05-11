module Project.ExternalConfig.PackageJsonTest (spec_PackageJson) where

import qualified Data.Map as M
import Test.Hspec
import qualified Wasp.ExternalConfig.Npm.PackageJson as P
import Wasp.Project.Common (TsConfigPaths, tsConfigPathsInWaspLangProjects, tsConfigPathsInWaspTsProjects)
import Wasp.Project.ExternalConfig.PackageJson (packageJsonValidator)
import qualified Wasp.Validator as V

spec_PackageJson :: Spec
spec_PackageJson = do
  describe "packageJsonValidator" $ do
    it "runs Node types validation for Wasp TS projects" $
      validate tsConfigPathsInWaspTsProjects validPackageJson
        `shouldSatisfy` hasErrorInField ["devDependencies", "@types/node"]

    it "skips Node types validation for Wasp-lang projects" $
      validate tsConfigPathsInWaspLangProjects validPackageJson
        `shouldBe` []

validate :: TsConfigPaths -> P.PackageJson -> [V.ValidationError]
validate tsConfigPaths = V.execValidator (packageJsonValidator tsConfigPaths)

hasErrorInField :: [String] -> [V.ValidationError] -> Bool
hasErrorInField fieldPath = any ((== fieldPath) . V.fieldPath)

validPackageJson :: P.PackageJson
validPackageJson =
  P.PackageJson
    { P.name = "test-app",
      P.dependencies = M.empty,
      P.devDependencies = M.empty,
      P.workspaces = Nothing,
      P.wasp = Nothing
    }
