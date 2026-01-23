module Generator.NpmDependenciesTest where

import Test.Hspec
import qualified Wasp.ExternalConfig.Npm.Dependency as D
import Wasp.Generator.NpmDependencies

spec_getNpmDepsConflicts :: Spec
spec_getNpmDepsConflicts = do
  let waspDeps =
        D.fromList
          [ ("a", "1"),
            ("b", "2")
          ]

  let waspPeerDeps =
        D.fromList
          [ ("peer1", "1.0.0"),
            ("peer2", "2.0.0")
          ]

  it
    "NpmDependencies are equal even if dependencies have different order"
    $ do
      NpmDepsForPackage
        { dependencies = waspDeps,
          devDependencies = [],
          peerDependencies = []
        }
      `shouldBe` NpmDepsForPackage
        { dependencies = reverse waspDeps,
          devDependencies = [],
          peerDependencies = []
        }

  it
    "NpmDependencies are equal even if dependencies have different order, with dev dependencies"
    $ do
      NpmDepsForPackage
        { dependencies = waspDeps,
          devDependencies = reverse waspDeps,
          peerDependencies = []
        }
      `shouldBe` NpmDepsForPackage
        { dependencies = reverse waspDeps,
          devDependencies = waspDeps,
          peerDependencies = []
        }

  it
    "NpmDependencies are equal even if peer dependencies have different order"
    $ do
      NpmDepsForPackage
        { dependencies = [],
          devDependencies = [],
          peerDependencies = waspPeerDeps
        }
      `shouldBe` NpmDepsForPackage
        { dependencies = [],
          devDependencies = [],
          peerDependencies = reverse waspPeerDeps
        }

  it
    "NpmDependencies can be unequal"
    $ do
      NpmDepsForPackage
        { dependencies = waspDeps,
          devDependencies = [],
          peerDependencies = []
        }
      `shouldNotBe` NpmDepsForPackage
        { dependencies = [],
          devDependencies = [],
          peerDependencies = []
        }

spec_getPeerDependenciesPackageJsonEntry :: Spec
spec_getPeerDependenciesPackageJsonEntry = do
  it "generates correct package.json entry for peer dependencies" $ do
    let deps =
          NpmDepsForPackage
            { dependencies = [],
              devDependencies = [],
              peerDependencies = D.fromList [("react", "^18.0.0"), ("@tanstack/react-query", "^4.0.0")]
            }
    getPeerDependenciesPackageJsonEntry deps
      `shouldBe` "\"peerDependencies\": {\"react\": \"^18.0.0\",\n  \"@tanstack/react-query\": \"^4.0.0\"\n}"

  it "generates correct package.json entry for empty peer dependencies" $ do
    let deps =
          NpmDepsForPackage
            { dependencies = [],
              devDependencies = [],
              peerDependencies = []
            }
    getPeerDependenciesPackageJsonEntry deps
      `shouldBe` "\"peerDependencies\": {\n}"
