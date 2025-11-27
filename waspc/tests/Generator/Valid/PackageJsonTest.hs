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
    it "succeeds when optional dependency is not present" $
      forEachCase $ \depType pkgJson -> do
        testSuccess
          (makeOptionalDepValidator depType ("optional-pkg", "1.0.0"))
          pkgJson

    it "succeeds when optional dependency has correct version" $
      forEachCase $ \depType pkgJson -> do
        testSuccess
          (makeOptionalDepValidator depType ("optional-pkg", "1.0.0"))
          (pkgJson `withDep` (depType, ("optional-pkg", "1.0.0")))

    it "fails when optional dependency has wrong version" $
      forEachCase $ \depType pkgJson -> do
        testFailure
          (makeOptionalDepValidator depType ("optional-pkg", "1.0.0"))
          (pkgJson `withDep` (depType, ("optional-pkg", "2.0.0")))
          ["Wasp requires package \"optional-pkg\" to be version \"1.0.0\" if present."]

    it "sets correct field path for dependencies" $
      forEachCase $ \depType pkgJson -> do
        let fieldName = depTypeToFieldName depType
        testErrorsHaveCorrectFieldPath
          (makeOptionalDepValidator depType ("optional-pkg", "1.0.0"))
          (pkgJson `withDep` (depType, ("optional-pkg", "2.0.0")))
          [fieldName, "optional-pkg"]

  describe "makeRequiredDepValidator" $ do
    it "succeeds when required dependency is present with correct version" $
      forEachCase $ \depType pkgJson -> do
        testSuccess
          (makeRequiredDepValidator depType ("required-pkg", "1.0.0"))
          (pkgJson `withDep` (depType, ("required-pkg", "1.0.0")))

    it "fails when required dependency is missing" $
      forEachCase $ \depType pkgJson -> do
        testFailure
          (makeRequiredDepValidator depType ("required-pkg", "1.0.0"))
          pkgJson
          ["Wasp requires package \"required-pkg\" with version \"1.0.0\"."]

    it "fails when required dependency has wrong version" $
      forEachCase $ \depType pkgJson -> do
        testFailure
          (makeRequiredDepValidator depType ("required-pkg", "1.0.0"))
          (pkgJson `withDep` (depType, ("required-pkg", "2.0.0")))
          ["Wasp requires package \"required-pkg\" to be version \"1.0.0\"."]

    it "fails when required dependency is in the opposite list" $
      forEachCase $ \depType pkgJson -> do
        let oppositeDepType = depTypeToOpposite depType
        let fieldName = depTypeToFieldName depType
        testFailure
          (makeRequiredDepValidator depType ("required-pkg", "1.0.0"))
          (pkgJson `withDep` (oppositeDepType, ("required-pkg", "1.0.0")))
          ["Wasp requires package \"required-pkg\" to be in \"" <> fieldName <> "\"."]

    it "sets correct field path" $
      forEachCase $ \depType pkgJson -> do
        let fieldName = depTypeToFieldName depType
        testErrorsHaveCorrectFieldPath
          (makeRequiredDepValidator depType ("required-pkg", "1.0.0"))
          (pkgJson `withDep` (depType, ("required-pkg", "2.0.0")))
          [fieldName, "required-pkg"]

  describe "inDependency" $ do
    it "runs validator on dependency value" $
      forEachCase $ \depType pkgJson -> do
        let isExpectedValidator = V.eqJust "1.0.0"
        testSuccess
          (inDependency depType "test-pkg" isExpectedValidator)
          (pkgJson `withDep` (depType, ("test-pkg", "1.0.0")))

    it "passes Nothing when dependency is missing" $
      forEachCase $ \depType pkgJson -> do
        let isNothingValidator Nothing = V.success
            isNothingValidator _ = V.failure "Should be Nothing"
        testSuccess
          (inDependency depType "missing-pkg" isNothingValidator)
          pkgJson

    it "sets correct field path with nested fields" $
      forEachCase $ \depType pkgJson -> do
        let fieldName = depTypeToFieldName depType

        let alwaysFailValidator = const $ V.failure "test error"
        let validator =
              inDependency depType "test-pkg" alwaysFailValidator

        testErrorsHaveCorrectFieldPath
          validator
          pkgJson
          [fieldName, "test-pkg"]

    it "looks in correct dependency type only" $ do
      let pkgJson =
            mockPackageJsonEmpty
              `withDep` (Runtime, ("pkg-a", "1.0.0"))
              `withDep` (Development, ("pkg-b", "2.0.0"))

      testSuccess
        (inDependency Runtime "pkg-a" $ V.eqJust "1.0.0")
        pkgJson

      testSuccess
        (inDependency Development "pkg-b" $ V.eqJust "2.0.0")
        pkgJson

-- | Runs a test for each combination of DependencyType and empty/mock PackageJson
forEachCase :: (DependencyType -> P.PackageJson -> Expectation) -> Expectation
forEachCase fn =
  sequence_
    [ fn depType pkgJson
      | depType <- [Runtime, Development],
        pkgJson <- [mockPackageJson, mockPackageJsonEmpty]
    ]

testSuccess :: V.Validator P.PackageJson -> P.PackageJson -> IO ()
testSuccess validator pkgJson =
  V.execValidator validator pkgJson `shouldBe` []

testFailure :: V.Validator P.PackageJson -> P.PackageJson -> [String] -> IO ()
testFailure validator pkgJson expectedErrors = do
  let errors = V.execValidator validator pkgJson
  (V.message <$> errors) `shouldBe` expectedErrors

testErrorsHaveCorrectFieldPath :: V.Validator P.PackageJson -> P.PackageJson -> [String] -> IO ()
testErrorsHaveCorrectFieldPath validator pkgJson expectedPath = do
  let errors = V.execValidator validator pkgJson
  mapM_ (`shouldBe` expectedPath) (V.fieldPath <$> errors)

mockPackageJson :: P.PackageJson
mockPackageJson =
  mockPackageJsonEmpty
    `withDep` (Runtime, ("existing-pkg", "1.0.0"))
    `withDep` (Development, ("existing-dev-pkg", "1.0.0"))

mockPackageJsonEmpty :: P.PackageJson
mockPackageJsonEmpty =
  P.PackageJson
    { P.name = "mock-package",
      P.dependencies = M.empty,
      P.devDependencies = M.empty,
      P.workspaces = Nothing
    }

withDep :: P.PackageJson -> (DependencyType, (String, String)) -> P.PackageJson
withDep pkgJson (Runtime, (name, version)) =
  pkgJson {P.dependencies = M.insert name version (P.dependencies pkgJson)}
withDep pkgJson (Development, (name, version)) =
  pkgJson {P.devDependencies = M.insert name version (P.devDependencies pkgJson)}

depTypeToFieldName :: DependencyType -> String
depTypeToFieldName Runtime = "dependencies"
depTypeToFieldName Development = "devDependencies"

depTypeToOpposite :: DependencyType -> DependencyType
depTypeToOpposite Runtime = Development
depTypeToOpposite Development = Runtime
