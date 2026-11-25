module ExternalConfig.Npm.NpmDependenciesTest where

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
