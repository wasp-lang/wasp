module Project.ExternalConfig.PackageJsonTest (spec_PackageJson) where

import Data.List (isInfixOf)
import qualified Data.Map as M
import Test.Hspec
import qualified Wasp.ExternalConfig.Npm.PackageJson as P
import qualified Wasp.Node.Version as NodeVersion
import Wasp.Project.Common (TsConfigPaths (..), tsConfigPaths)
import Wasp.Project.ExternalConfig.PackageJson (isValidNpmPackageName, validatePackageJsonForModule, validatePackageJsonForProject)

spec_PackageJson :: Spec
spec_PackageJson = do
  describe "validatePackageJsonForProject" $ do
    it "returns no errors for a valid Wasp TS project package.json" $
      validate tsConfigPaths (validPackageJson `withDevDependency` requiredNodeTypesDependency)
        `shouldBe` []

    it "returns an error when a Wasp TS project is missing @types/node" $
      assertReturnsValidationErrorMentioningField "@types/node" validPackageJson

  describe "validatePackageJsonForModule" $ do
    it "returns no errors for valid module metadata" $
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

  describe "isValidNpmPackageName" $ do
    it "accepts scoped and unscoped npm package names" $ do
      isValidNpmPackageName "my-module" `shouldBe` True
      isValidNpmPackageName "@wasp.sh/my-module" `shouldBe` True

    it "rejects names npm cannot publish" $
      map isValidNpmPackageName ["", "MyModule", ".hidden", "@scope", "@scope/bad/name", "has space"]
        `shouldBe` replicate 6 False

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
      P.packageType = Nothing,
      P.files = Nothing,
      P.dependencies = M.empty,
      P.devDependencies = M.empty,
      P.peerDependencies = M.empty,
      P.workspaces = Nothing,
      P.wasp = Nothing
    }

validModulePackageJson :: P.PackageJson
validModulePackageJson =
  validPackageJson
    { P.packageType = Just "module",
      P.files = Just ["dist"],
      P.peerDependencies = M.fromList [("@wasp.sh/spec", "^0.25.0"), ("react", "^19.2.1"), ("wasp", "*")],
      P.wasp =
        Just
          emptyWaspConfig
            { P.module_ = Just $ P.WaspModuleConfig ["./src/**/*.{ts,tsx}"]
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
