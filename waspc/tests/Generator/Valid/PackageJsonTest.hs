module Generator.Valid.PackageJsonTest (spec_PackageJson) where

import qualified Data.Map as M
import Test.Hspec
import Text.Printf (printf)
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
    itEach "succeeds when optional dependency is not present" $ \depType pkgJson -> do
      makeOptionalDepValidator depType ("optional-pkg", "1.0.0")
        <-- pkgJson
        ~> []

    itEach "succeeds when optional dependency has correct version" $ \depType pkgJson -> do
      makeOptionalDepValidator depType ("optional-pkg", "1.0.0")
        <-- (pkgJson `withDep` (depType, ("optional-pkg", "1.0.0")))
        ~> []

    itEach "fails when optional dependency has wrong version" $ \depType pkgJson -> do
      makeOptionalDepValidator depType ("optional-pkg", "1.0.0")
        <-- (pkgJson `withDep` (depType, ("optional-pkg", "2.0.0")))
        ~> ["Wasp requires package \"optional-pkg\" to be version \"1.0.0\" if present."]

    itEach "sets the correct field path in errors" $ \depType pkgJson -> do
      let fieldName = depTypeToFieldName depType
      makeOptionalDepValidator depType ("optional-pkg", "1.0.0")
        <-- (pkgJson `withDep` (depType, ("optional-pkg", "2.0.0")))
        ~~> (([fieldName, "optional-pkg"] ==) . V.fieldPath)

  describe "makeRequiredDepValidator" $ do
    itEach "succeeds when required dependency is present with correct version" $ \depType pkgJson -> do
      makeRequiredDepValidator depType ("required-pkg", "1.0.0")
        <-- (pkgJson `withDep` (depType, ("required-pkg", "1.0.0")))
        ~> []

    itEach "fails when required dependency is missing" $ \depType pkgJson -> do
      makeRequiredDepValidator depType ("required-pkg", "1.0.0")
        <-- pkgJson
        ~> ["Wasp requires package \"required-pkg\" with version \"1.0.0\"."]

    itEach "fails when required dependency has wrong version" $ \depType pkgJson -> do
      makeRequiredDepValidator depType ("required-pkg", "1.0.0")
        <-- (pkgJson `withDep` (depType, ("required-pkg", "2.0.0")))
        ~> ["Wasp requires package \"required-pkg\" to be version \"1.0.0\"."]

    itEach "fails when required dependency is in the opposite list" $ \depType pkgJson -> do
      let oppositeDepType = depTypeToOpposite depType
          fieldName = depTypeToFieldName depType
      makeRequiredDepValidator depType ("required-pkg", "1.0.0")
        <-- (pkgJson `withDep` (oppositeDepType, ("required-pkg", "1.0.0")))
        ~> ["Wasp requires package \"required-pkg\" to be in \"" <> fieldName <> "\"."]

    itEach "sets the correct field path in errors" $ \depType pkgJson -> do
      let fieldName = depTypeToFieldName depType
      makeRequiredDepValidator depType ("required-pkg", "1.0.0")
        <-- (pkgJson `withDep` (depType, ("required-pkg", "2.0.0")))
        ~~> (([fieldName, "required-pkg"] ==) . V.fieldPath)

  describe "inDependency" $ do
    itEach "runs validator on dependency version value" $ \depType pkgJson -> do
      inDependency depType "test-pkg" (V.eqJust "1.0.0")
        <-- (pkgJson `withDep` (depType, ("test-pkg", "1.0.0")))
        ~> []

    itEach "passes Nothing when dependency is missing" $ \depType pkgJson -> do
      inDependency depType "missing-pkg" (maybe V.success $ const $ V.failure "Should be Nothing")
        <-- pkgJson
        ~> []

    itEach "sets the correct field path in errors" $ \depType pkgJson -> do
      let fieldName = depTypeToFieldName depType
          alwaysFailValidator = const $ V.failure "test error"
      inDependency depType "test-pkg" alwaysFailValidator
        <-- pkgJson
        ~~> (([fieldName, "test-pkg"] ==) . V.fieldPath)

    it "looks in correct dependency list only" $ do
      let pkgJson =
            emptyPackageJson
              `withDep` (Runtime, ("pkg-a", "1.0.0"))
              `withDep` (Development, ("pkg-b", "2.0.0"))
      inDependency Runtime "pkg-a" (V.eqJust "1.0.0")
        <-- pkgJson
        ~> []
      inDependency Development "pkg-b" (V.eqJust "2.0.0")
        <-- pkgJson
        ~> []

-- | Runs a test for each combination of DependencyType and empty/mock PackageJson
itEach :: String -> (DependencyType -> P.PackageJson -> Expectation) -> SpecWith (Arg (IO ()))
itEach testName fn =
  sequence_
    [ it (printf "%s (%s dep, %s)" testName (show depType) (pkgJsonName :: String)) $
        fn depType pkgJson
      | depType <- [Runtime, Development],
        (pkgJson, pkgJsonName) <-
          [ (mockPackageJson, "mocked package.json"),
            (emptyPackageJson, "empty package.json")
          ]
    ]

mockPackageJson :: P.PackageJson
mockPackageJson =
  emptyPackageJson
    `withDep` (Runtime, ("existing-pkg", "1.0.0"))
    `withDep` (Development, ("existing-dev-pkg", "1.0.0"))

emptyPackageJson :: P.PackageJson
emptyPackageJson =
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

-- | Passes the given input to the `Validator`, and turns it into a `Validator ()`.
-- This way we can use our shorthand operators, which only supply `()`.
(<--) :: V.Validator a -> a -> V.Validator ()
validator <-- value = const $ validator value
