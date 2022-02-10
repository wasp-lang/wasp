module Generator.PackageJsonGeneratorTest where

import Test.Tasty.Hspec
import qualified Wasp.AppSpec.App.Dependency as D
import Wasp.Generator.PackageJsonGenerator

spec_combinePackageJsonDependencies :: Spec
spec_combinePackageJsonDependencies = do
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

  it "a conflicting version number is detected" $ do
    let waspPackageJsonDependencies =
          PackageJsonDependencies
            { dependencies = waspDeps,
              devDependencies = []
            }
    let userPackageJsonDependencies =
          PackageJsonDependencies
            { dependencies =
                D.fromList
                  [ ("a", "1"),
                    ("b", "3")
                  ],
              devDependencies = []
            }

    combinePackageJsonDependencies waspPackageJsonDependencies userPackageJsonDependencies
      `shouldBe` Left
        PackageJsonDependenciesError
          { dependenciesConflictErrors =
              [ DependencyConflictError
                  (D.make ("b", "2"))
                  (D.make ("b", "3"))
              ],
            devDependenciesConflictErrors = []
          }

  it "wasp deps completely overlap with user deps, no duplication" $ do
    let waspPackageJsonDependencies =
          PackageJsonDependencies
            { dependencies = waspDeps,
              devDependencies = []
            }
    let userPackageJsonDependencies =
          PackageJsonDependencies
            { dependencies =
                D.fromList
                  [ ("a", "1"),
                    ("b", "2")
                  ],
              devDependencies = []
            }

    combinePackageJsonDependencies waspPackageJsonDependencies userPackageJsonDependencies
      `shouldBe` Right
        PackageJsonDependencies
          { dependencies =
              D.fromList
                [ ("a", "1"),
                  ("b", "2")
                ],
            devDependencies = []
          }

  it "user dependencies supplement wasp dependencies" $ do
    let waspPackageJsonDependencies =
          PackageJsonDependencies
            { dependencies = waspDeps,
              devDependencies = []
            }
    let userPackageJsonDependencies =
          PackageJsonDependencies
            { dependencies =
                D.fromList
                  [ ("c", "3"),
                    ("d", "4")
                  ],
              devDependencies = []
            }

    combinePackageJsonDependencies waspPackageJsonDependencies userPackageJsonDependencies
      `shouldBe` Right
        PackageJsonDependencies
          { dependencies =
              D.fromList
                [ ("a", "1"),
                  ("b", "2"),
                  ("c", "3"),
                  ("d", "4")
                ],
            devDependencies = []
          }

  it "user dependencies partially overlap wasp dependencies, so only non-overlapping supplement" $ do
    let waspPackageJsonDependencies =
          PackageJsonDependencies
            { dependencies = waspDeps,
              devDependencies = []
            }
    let userPackageJsonDependencies =
          PackageJsonDependencies
            { dependencies =
                D.fromList
                  [ ("a", "1"),
                    ("d", "4")
                  ],
              devDependencies = []
            }

    combinePackageJsonDependencies waspPackageJsonDependencies userPackageJsonDependencies
      `shouldBe` Right
        PackageJsonDependencies
          { dependencies =
              D.fromList
                [ ("a", "1"),
                  ("b", "2"),
                  ("d", "4")
                ],
            devDependencies = []
          }

  it "report error if user dependency overlaps wasp dependency, different version" $ do
    let waspPackageJsonDependencies =
          PackageJsonDependencies
            { dependencies = waspDeps,
              devDependencies = []
            }
    let userPackageJsonDependencies =
          PackageJsonDependencies
            { dependencies =
                D.fromList
                  [ ("a", "2"),
                    ("foo", "bar")
                  ],
              devDependencies = []
            }

    combinePackageJsonDependencies waspPackageJsonDependencies userPackageJsonDependencies
      `shouldBe` Left
        PackageJsonDependenciesError
          { dependenciesConflictErrors =
              [ DependencyConflictError
                  (D.make ("a", "1"))
                  (D.make ("a", "2"))
              ],
            devDependenciesConflictErrors = []
          }

  it "a conflicting version number is detected with wasp devDependencies" $ do
    let waspPackageJsonDependencies =
          PackageJsonDependencies
            { dependencies = waspDeps,
              devDependencies = waspDevDeps
            }
    let userPackageJsonDependencies =
          PackageJsonDependencies
            { dependencies =
                D.fromList
                  [ ("a", "1"),
                    ("alpha", "70")
                  ],
              devDependencies = []
            }

    combinePackageJsonDependencies waspPackageJsonDependencies userPackageJsonDependencies
      `shouldBe` Left
        PackageJsonDependenciesError
          { dependenciesConflictErrors =
              [ DependencyConflictError
                  (D.make ("alpha", "10"))
                  (D.make ("alpha", "70"))
              ],
            devDependenciesConflictErrors = []
          }

  it "dev dependencies are also combined" $ do
    let waspPackageJsonDependencies =
          PackageJsonDependencies
            { dependencies = waspDeps,
              devDependencies = waspDevDeps
            }

    let userPackageJsonDependencies =
          PackageJsonDependencies
            { dependencies =
                D.fromList
                  [ ("a", "1"),
                    ("d", "4")
                  ],
              devDependencies =
                D.fromList
                  [ ("alpha", "10"),
                    ("gamma", "30")
                  ]
            }
    combinePackageJsonDependencies waspPackageJsonDependencies userPackageJsonDependencies
      `shouldBe` Right
        PackageJsonDependencies
          { dependencies =
              D.fromList
                [ ("a", "1"),
                  ("b", "2"),
                  ("d", "4")
                ],
            devDependencies =
              D.fromList
                [ ("alpha", "10"),
                  ("beta", "20"),
                  ("gamma", "30")
                ]
          }

  it "wasp dev dependency overlaps with user dependency, should remain devDependency" $ do
    let waspPackageJsonDependencies =
          PackageJsonDependencies
            { dependencies = waspDeps,
              devDependencies = waspDevDeps
            }

    let userPackageJsonDependencies =
          PackageJsonDependencies
            { dependencies =
                D.fromList
                  [ ("alpha", "10")
                  ],
              devDependencies = []
            }
    combinePackageJsonDependencies waspPackageJsonDependencies userPackageJsonDependencies
      `shouldBe` Right
        PackageJsonDependencies
          { dependencies =
              D.fromList
                [ ("a", "1"),
                  ("b", "2")
                ],
            devDependencies =
              D.fromList
                [ ("alpha", "10"),
                  ("beta", "20")
                ]
          }

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
    "PackageJsonDependencies are equal even if dependencies have different order"
    $ do
      PackageJsonDependencies
        { dependencies = waspDeps,
          devDependencies = []
        }
      `shouldBe` PackageJsonDependencies
        { dependencies = reverse waspDeps,
          devDependencies = []
        }

  it
    "PackageJsonDependencies are equal even if dependencies have different order, with dev dependencies"
    $ do
      PackageJsonDependencies
        { dependencies = waspDeps,
          devDependencies = reverse waspDeps
        }
      `shouldBe` PackageJsonDependencies
        { dependencies = reverse waspDeps,
          devDependencies = waspDeps
        }

  it
    "PackageJsonDependencies can be unequal"
    $ do
      PackageJsonDependencies
        { dependencies = waspDeps,
          devDependencies = []
        }
      `shouldNotBe` PackageJsonDependencies
        { dependencies = [],
          devDependencies = []
        }
