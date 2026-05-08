module Project.ExternalConfig.PackageJsonTest (spec_PackageJson) where

import Data.List (isInfixOf)
import qualified Data.Map as M
import Test.Hspec
import qualified Wasp.ExternalConfig.Npm.PackageJson as P
import qualified Wasp.Node.Version as NodeVersion
import Wasp.Project.Common (TsConfigPaths, tsConfigPathsInWaspLangProjects, tsConfigPathsInWaspTsProjects)
import Wasp.Project.ExternalConfig.PackageJson (packageJsonValidator)
import qualified Wasp.Validator as V

spec_PackageJson :: Spec
spec_PackageJson = do
  describe "packageJsonValidator" $ do
    it "returns no errors for a valid Wasp TS project package.json" $
      validate tsConfigPathsInWaspTsProjects (validPackageJson `withDevDependency` requiredNodeTypesDependency)
        `shouldBe` []

    it "returns an error when a Wasp TS project is missing @types/node" $
      validate tsConfigPathsInWaspTsProjects validPackageJson
        `shouldSatisfy` any (mentionsAll ["devDependencies", "@types/node"])

    it "returns an error when a Wasp TS project has a wrong @types/node version" $
      validate tsConfigPathsInWaspTsProjects (validPackageJson `withDevDependency` ("@types/node", "^23.0.0"))
        `shouldSatisfy` any (mentionsAll ["devDependencies", "@types/node", "^24.0.0", "^23.0.0"])

    it "returns an error when a Wasp TS project has @types/node in dependencies" $
      validate tsConfigPathsInWaspTsProjects (validPackageJson `withDependency` requiredNodeTypesDependency)
        `shouldSatisfy` any (mentionsAll ["dependencies", "devDependencies", "@types/node"])

    it "does not require @types/node for a Wasp-lang project" $
      validate tsConfigPathsInWaspLangProjects validPackageJson
        `shouldBe` []

validate :: TsConfigPaths -> P.PackageJson -> [String]
validate tsConfigPaths = map show . V.execValidator (packageJsonValidator tsConfigPaths)

validPackageJson :: P.PackageJson
validPackageJson =
  P.PackageJson
    { P.name = "test-app",
      P.dependencies = M.empty,
      P.devDependencies = M.empty,
      P.workspaces = Nothing,
      P.wasp = Nothing
    }

requiredNodeTypesDependency :: (P.PackageName, P.PackageVersion)
requiredNodeTypesDependency =
  ("@types/node", NodeVersion.nodeTypesPackageVersionRange NodeVersion.oldestWaspSupportedNodeVersion)

withDevDependency :: P.PackageJson -> (P.PackageName, P.PackageVersion) -> P.PackageJson
withDevDependency packageJson (name, version) =
  packageJson {P.devDependencies = M.insert name version (P.devDependencies packageJson)}

withDependency :: P.PackageJson -> (P.PackageName, P.PackageVersion) -> P.PackageJson
withDependency packageJson (name, version) =
  packageJson {P.dependencies = M.insert name version (P.dependencies packageJson)}

mentionsAll :: [String] -> String -> Bool
mentionsAll needles message = all (`isInfixOf` message) needles
