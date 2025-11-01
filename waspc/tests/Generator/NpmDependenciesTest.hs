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

  let waspDevDeps =
        D.fromList
          [ ("alpha", "10"),
            ("beta", "20")
          ]

  let waspPeerDeps =
        D.fromList
          [ ("peer1", "1.0.0"),
            ("peer2", "2.0.0")
          ]

  it "a conflicting version number is detected" $ do
    let npmDepsFromWasp =
          NpmDepsFromWasp $
            NpmDepsForPackage
              { dependencies = waspDeps,
                devDependencies = [],
                peerDependencies = []
              }
    let npmDepsFromUser =
          NpmDepsFromUser $
            NpmDepsForPackage
              { dependencies =
                  D.fromList
                    [ ("a", "1"),
                      ("b", "3")
                    ],
                devDependencies = [],
                peerDependencies = []
              }

    getNpmDepsConflicts npmDepsFromWasp npmDepsFromUser
      `shouldBe` [ DependencyConflictError
                     (D.make ("b", "2"))
                     (D.make ("b", "3"))
                 ]

  it "wasp deps completely overlap with user deps with same versions: no conflicts" $ do
    let npmDepsFromWasp =
          NpmDepsFromWasp $
            NpmDepsForPackage
              { dependencies = waspDeps,
                devDependencies = [],
                peerDependencies = []
              }
    let npmDepsFromUser =
          NpmDepsFromUser $
            NpmDepsForPackage
              { dependencies = waspDeps,
                devDependencies = [],
                peerDependencies = []
              }

    getNpmDepsConflicts npmDepsFromWasp npmDepsFromUser
      `shouldBe` []

  it "user dependencies have no overlap with wasp deps: no conflicts" $ do
    let npmDepsFromWasp =
          NpmDepsFromWasp $
            NpmDepsForPackage
              { dependencies = waspDeps,
                devDependencies = [],
                peerDependencies = []
              }
    let npmDepsFromUser =
          NpmDepsFromUser $
            NpmDepsForPackage
              { dependencies =
                  D.fromList
                    [ ("c", "3"),
                      ("d", "4")
                    ],
                devDependencies = [],
                peerDependencies = []
              }

    getNpmDepsConflicts npmDepsFromWasp npmDepsFromUser
      `shouldBe` []

  it "user dependencies partially overlap wasp dependencies with same versions: no conflicts" $ do
    let npmDepsFromWasp =
          NpmDepsFromWasp $
            NpmDepsForPackage
              { dependencies = waspDeps,
                devDependencies = [],
                peerDependencies = []
              }
    let npmDepsFromUser =
          NpmDepsFromUser $
            NpmDepsForPackage
              { dependencies =
                  D.fromList
                    [ ("a", "1"),
                      ("d", "4")
                    ],
                devDependencies = [],
                peerDependencies = []
              }

    getNpmDepsConflicts npmDepsFromWasp npmDepsFromUser
      `shouldBe` []

  it "report error if user dependency overlaps wasp dependency, different version" $ do
    let npmDepsFromWasp =
          NpmDepsFromWasp $
            NpmDepsForPackage
              { dependencies = waspDeps,
                devDependencies = [],
                peerDependencies = []
              }
    let npmDepsFromUser =
          NpmDepsFromUser $
            NpmDepsForPackage
              { dependencies =
                  D.fromList
                    [ ("a", "2"),
                      ("foo", "bar")
                    ],
                devDependencies = [],
                peerDependencies = []
              }

    getNpmDepsConflicts npmDepsFromWasp npmDepsFromUser
      `shouldBe` [ DependencyConflictError
                     (D.make ("a", "1"))
                     (D.make ("a", "2"))
                 ]

  it "a conflicting version number is detected with wasp devDependencies" $ do
    let npmDepsFromWasp =
          NpmDepsFromWasp $
            NpmDepsForPackage
              { dependencies = waspDeps,
                devDependencies = waspDevDeps,
                peerDependencies = []
              }
    let npmDepsFromUser =
          NpmDepsFromUser $
            NpmDepsForPackage
              { dependencies =
                  D.fromList
                    [ ("a", "1"),
                      ("alpha", "70")
                    ],
                devDependencies = [],
                peerDependencies = []
              }

    getNpmDepsConflicts npmDepsFromWasp npmDepsFromUser
      `shouldBe` [ DependencyConflictError
                     (D.make ("alpha", "10"))
                     (D.make ("alpha", "70"))
                 ]

  it "both dev deps and normal deps are same for user and wasp: no conflicts" $ do
    let npmDepsFromWasp =
          NpmDepsFromWasp $
            NpmDepsForPackage
              { dependencies = waspDeps,
                devDependencies = waspDevDeps,
                peerDependencies = []
              }

    let npmDepsFromUser =
          NpmDepsFromUser $
            NpmDepsForPackage
              { dependencies = waspDeps,
                devDependencies = waspDevDeps,
                peerDependencies = []
              }
    getNpmDepsConflicts npmDepsFromWasp npmDepsFromUser
      `shouldBe` []

  it "wasp dev dependency overlaps with user non-dev dependency with same version: no conflicts" $ do
    let npmDepsFromWasp =
          NpmDepsFromWasp $
            NpmDepsForPackage
              { dependencies = waspDeps,
                devDependencies = waspDevDeps,
                peerDependencies = []
              }

    let npmDepsFromUser =
          NpmDepsFromUser $
            NpmDepsForPackage
              { dependencies =
                  D.fromList
                    [ ("alpha", "10")
                    ],
                devDependencies = [],
                peerDependencies = []
              }

    getNpmDepsConflicts npmDepsFromWasp npmDepsFromUser
      `shouldBe` []

  it "no user dependencies: no conflicts" $ do
    let npmDepsFromWasp =
          NpmDepsFromWasp $
            NpmDepsForPackage
              { dependencies = waspDeps,
                devDependencies = waspDevDeps,
                peerDependencies = []
              }
    let npmDepsFromUser =
          NpmDepsFromUser $
            NpmDepsForPackage
              { dependencies = [],
                devDependencies = [],
                peerDependencies = []
              }

    getNpmDepsConflicts npmDepsFromWasp npmDepsFromUser
      `shouldBe` []

  it "conflictErrorToMessage" $ do
    conflictErrorToMessage
      ( DependencyConflictError
          { waspDependency = D.make ("a", "1"),
            userDependency = D.make ("a", "2")
          }
      )
      `shouldBe` "Error: Dependency conflict for user dependency (a, 2): "
        ++ "Version must be set to the exactly "
        ++ "the same version as the one wasp is using: 1"

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
