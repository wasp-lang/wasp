module Project.ExternalConfig.PackageJsonTest (spec_PackageJson) where

import Data.List (isInfixOf)
import qualified Data.Map as M
import Test.Hspec
import qualified Wasp.ExternalConfig.Npm.PackageJson as P
import qualified Wasp.Node.Version as NodeVersion
import Wasp.Project.Common (TsConfigPaths (..), tsConfigPaths)
import Wasp.Project.ExternalConfig.PackageJson (validatePackageJsonForModule, validatePackageJsonForProject)

spec_PackageJson :: Spec
spec_PackageJson = do
  describe "validatePackageJsonForProject" $ do
    it "returns no errors for a valid Wasp TS project package.json" $
      validate tsConfigPaths (validPackageJson `withDevDependency` requiredNodeTypesDependency)
        `shouldBe` []

    it "returns an error when a Wasp TS project is missing @types/node" $
      assertReturnsValidationErrorMentioningField "@types/node" validPackageJson

  describe "validatePackageJsonForModule" $ do
    it "returns no errors when wasp.module is an object" $
      validatePackageJsonForModule validModulePackageJson `shouldBe` []

    it "returns an error when wasp is missing" $
      validatePackageJsonForModule validPackageJson `shouldSatisfy` any ("wasp" `isInfixOf`)

    it "returns an error when wasp.module is missing" $
      validatePackageJsonForModule
        validPackageJson {P.wasp = Just emptyWaspConfig}
        `shouldSatisfy` any ("module" `isInfixOf`)

    it "returns an error when name is empty" $
      validatePackageJsonForModule
        validModulePackageJson {P.name = ""}
        `shouldSatisfy` any ("name" `isInfixOf`)

validate :: TsConfigPaths -> P.PackageJson -> [String]
validate = validatePackageJsonForProject

assertReturnsValidationErrorMentioningField :: String -> P.PackageJson -> Expectation
assertReturnsValidationErrorMentioningField fieldName packageJson =
  validate tsConfigPaths packageJson `shouldSatisfy` any (fieldName `isInfixOf`)

validPackageJson :: P.PackageJson
validPackageJson =
  P.PackageJson
    { P.name = "test-app",
      P.version = Nothing,
      P.dependencies = M.empty,
      P.devDependencies = M.empty,
      P.workspaces = Nothing,
      P.wasp = Nothing
    }

validModulePackageJson :: P.PackageJson
validModulePackageJson =
  validPackageJson
    { P.wasp =
        Just
          emptyWaspConfig
            { P.module_ = Just mempty
            }
    }

emptyWaspConfig :: P.WaspConfig
emptyWaspConfig =
  P.WaspConfig
    { P.overriddenDeps = Nothing,
      P.module_ = Nothing
    }

requiredNodeTypesDependency :: (P.PackageName, P.PackageVersion)
requiredNodeTypesDependency =
  ("@types/node", show $ NodeVersion.nodeTypesVersionRangeMatchingNodeMajor NodeVersion.oldestWaspSupportedNodeVersion)

withDevDependency :: P.PackageJson -> (P.PackageName, P.PackageVersion) -> P.PackageJson
withDevDependency packageJson (name, version) =
  packageJson {P.devDependencies = M.insert name version (P.devDependencies packageJson)}
