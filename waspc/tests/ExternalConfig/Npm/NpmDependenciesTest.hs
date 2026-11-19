module ExternalConfig.Npm.NpmDependenciesTest where

import Data.List (sortOn)
import Test.Hspec
import qualified Wasp.ExternalConfig.Npm.Dependency as D
import Wasp.Generator.NpmDependencies

spec_NpmDepsForPackageEquality :: Spec
spec_NpmDepsForPackageEquality = do
  let waspDeps =
        D.fromList
          [ ("a", "1"),
            ("b", "2")
          ]

  it
    "NpmDependencies are equal even if dependencies have different order"
    $ do
      NpmDepsForPackage
        { dependencies = waspDeps,
          devDependencies = []
        }
      `shouldBe` NpmDepsForPackage
        { dependencies = reverse waspDeps,
          devDependencies = []
        }

  it
    "NpmDependencies are equal even if dependencies have different order, with dev dependencies"
    $ do
      NpmDepsForPackage
        { dependencies = waspDeps,
          devDependencies = reverse waspDeps
        }
      `shouldBe` NpmDepsForPackage
        { dependencies = reverse waspDeps,
          devDependencies = waspDeps
        }

  it
    "NpmDependencies can be unequal"
    $ do
      NpmDepsForPackage
        { dependencies = waspDeps,
          devDependencies = []
        }
      `shouldNotBe` NpmDepsForPackage
        { dependencies = [],
          devDependencies = []
        }

spec_mergeWaspAndUserDeps :: Spec
spec_mergeWaspAndUserDeps = do
  it "uses user version for overridden dependency" $ do
    let waspDeps = makeWaspDeps [("react", "18.0.0")] []
        userDeps = makeUserDeps [("react", "17.0.0")] []
        result = mergeWaspAndUserDeps waspDeps userDeps
    sortedDeps (dependencies result) `shouldBe` [("react", "17.0.0")]

  it "only overrides specified packages" $ do
    let waspDeps = makeWaspDeps [("react", "18.0.0"), ("lodash", "4.0.0")] []
        userDeps = makeUserDeps [("react", "17.0.0")] []
        result = mergeWaspAndUserDeps waspDeps userDeps
    sortedDeps (dependencies result) `shouldBe` [("lodash", "4.0.0"), ("react", "17.0.0")]

  it "doesn't add user dependency when it's in override list but not in wasp deps" $ do
    let waspDeps = makeWaspDeps [("react", "18.0.0")] []
        userDeps = makeUserDeps [("axios", "1.0.0")] []
        result = mergeWaspAndUserDeps waspDeps userDeps
    sortedDeps (dependencies result) `shouldBe` [("react", "18.0.0")]

  it "handles mixed dependencies and devDependencies" $ do
    let waspDeps = makeWaspDeps [("typescript", "5.0.0")] [("jest", "29.0.0")]
        userDeps = makeUserDeps [("jest", "28.0.0")] [("typescript", "4.9.0")]
        result = mergeWaspAndUserDeps waspDeps userDeps
    sortedDeps (dependencies result) `shouldBe` [("typescript", "4.9.0")]
    sortedDeps (devDependencies result) `shouldBe` [("jest", "28.0.0")]

-- Helper functions

makeWaspDeps :: [(String, String)] -> [(String, String)] -> NpmDepsFromWasp
makeWaspDeps deps devDeps =
  NpmDepsFromWasp $
    NpmDepsForPackage
      { dependencies = D.fromList deps,
        devDependencies = D.fromList devDeps
      }

makeUserDeps :: [(String, String)] -> [(String, String)] -> NpmDepsFromUser
makeUserDeps deps devDeps =
  NpmDepsFromUser $
    NpmDepsForPackage
      { dependencies = D.fromList deps,
        devDependencies = D.fromList devDeps
      }

sortedDeps :: [D.Dependency] -> [(String, String)]
sortedDeps = sortOn fst . map D.toPair
