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
import Prelude hiding (fail)

spec_PackageJson :: Spec
spec_PackageJson = do
  describe "makeOptionalDepValidator" $ do
    it "succeeds when optional dependency is not present" $
      forEachCase $ \depType pkgJson -> do
        makeOptionalDepValidator depType ("optional-pkg", "1.0.0")
          <-- pkgJson
          ~> []

    it "succeeds when optional dependency has correct version" $
      forEachCase $ \depType pkgJson -> do
        makeOptionalDepValidator depType ("optional-pkg", "1.0.0")
          <-- (pkgJson `withDep` (depType, ("optional-pkg", "1.0.0")))
          ~> []

    it "fails when optional dependency has wrong version" $
      forEachCase $ \depType pkgJson -> do
        makeOptionalDepValidator depType ("optional-pkg", "1.0.0")
          <-- (pkgJson `withDep` (depType, ("optional-pkg", "2.0.0")))
          ~> ["Wasp requires package \"optional-pkg\" to be version \"1.0.0\" if present."]

    it "sets the correct field path in errors" $
      forEachCase $ \depType pkgJson -> do
        let fieldName = depTypeToFieldName depType
        makeOptionalDepValidator depType ("optional-pkg", "1.0.0")
          <-- (pkgJson `withDep` (depType, ("optional-pkg", "2.0.0")))
          ~~> (([fieldName, "optional-pkg"] ==) . V.fieldPath)

  describe "makeRequiredDepValidator" $ do
    it "succeeds when required dependency is present with correct version" $
      forEachCase $ \depType pkgJson -> do
        makeRequiredDepValidator depType ("required-pkg", "1.0.0")
          <-- (pkgJson `withDep` (depType, ("required-pkg", "1.0.0")))
          ~> []

    it "fails when required dependency is missing" $
      forEachCase $ \depType pkgJson -> do
        makeRequiredDepValidator depType ("required-pkg", "1.0.0")
          <-- pkgJson
          ~> ["Wasp requires package \"required-pkg\" with version \"1.0.0\"."]

    it "fails when required dependency has wrong version" $
      forEachCase $ \depType pkgJson -> do
        makeRequiredDepValidator depType ("required-pkg", "1.0.0")
          <-- (pkgJson `withDep` (depType, ("required-pkg", "2.0.0")))
          ~> ["Wasp requires package \"required-pkg\" to be version \"1.0.0\"."]

    it "fails when required dependency is in the opposite list" $
      forEachCase $ \depType pkgJson -> do
        let oppositeDepType = depTypeToOpposite depType
            fieldName = depTypeToFieldName depType
        makeRequiredDepValidator depType ("required-pkg", "1.0.0")
          <-- (pkgJson `withDep` (oppositeDepType, ("required-pkg", "1.0.0")))
          ~> ["Wasp requires package \"required-pkg\" to be in \"" <> fieldName <> "\"."]

    it "sets the correct field path in errors" $
      forEachCase $ \depType pkgJson -> do
        let fieldName = depTypeToFieldName depType
        makeRequiredDepValidator depType ("required-pkg", "1.0.0")
          <-- (pkgJson `withDep` (depType, ("required-pkg", "2.0.0")))
          ~~> (([fieldName, "required-pkg"] ==) . V.fieldPath)

  describe "inDependency" $ do
    it "runs validator on dependency version value" $
      forEachCase $ \depType pkgJson -> do
        inDependency depType "test-pkg" (V.eqJust "1.0.0")
          <-- (pkgJson `withDep` (depType, ("test-pkg", "1.0.0")))
          ~> []

    it "passes Nothing when dependency is missing" $
      forEachCase $ \depType pkgJson -> do
        inDependency depType "missing-pkg" (maybe V.success $ const $ V.failure "Should be Nothing")
          <-- pkgJson
          ~> []

    it "sets the correct field path in errors" $
      forEachCase $ \depType pkgJson -> do
        let fieldName = depTypeToFieldName depType
            alwaysFailValidator = const $ V.failure "test error"
        inDependency depType "test-pkg" alwaysFailValidator
          <-- pkgJson
          ~~> (([fieldName, "test-pkg"] ==) . V.fieldPath)

    it "looks in correct dependency list only" $ do
      let pkgJson =
            mockPackageJsonEmpty
              `withDep` (Runtime, ("pkg-a", "1.0.0"))
              `withDep` (Development, ("pkg-b", "2.0.0"))
      inDependency Runtime "pkg-a" (V.eqJust "1.0.0")
        <-- pkgJson
        ~> []
      inDependency Development "pkg-b" (V.eqJust "2.0.0")
        <-- pkgJson
        ~> []

-- | Runs a test for each combination of DependencyType and empty/mock PackageJson
forEachCase :: (DependencyType -> P.PackageJson -> Expectation) -> Expectation
forEachCase fn =
  sequence_
    [ fn depType pkgJson
      | depType <- [Runtime, Development],
        pkgJson <- [mockPackageJson, mockPackageJsonEmpty]
    ]

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

-- | Checks that the validator produces exactly the expected error messages
(~>) :: V.Validator () -> [String] -> Expectation
validator ~> expectedErrorMessages =
  V.message <$> V.execValidator validator () `shouldBe` expectedErrorMessages

-- | Checks that all the validator errors satisfy a given predicate
(~~>) :: V.Validator () -> (V.ValidationError -> Bool) -> Expectation
validator ~~> f =
  mapM_ (`shouldSatisfy` f) (V.execValidator validator ())

-- | Forces a specific input for the Validator
(<--) :: V.Validator a -> a -> V.Validator ()
validator <-- value = const $ validator value
