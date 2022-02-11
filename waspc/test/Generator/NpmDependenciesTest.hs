module Generator.NpmDependenciesTest where

import Test.Tasty.Hspec
import qualified Wasp.AppSpec.App.Dependency as D
import Wasp.Generator.NpmDependencies

spec_combineNpmDependencies :: Spec
spec_combineNpmDependencies = do
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
    let waspNpmDependencies =
          NpmDependencies
            { dependencies = waspDeps,
              devDependencies = []
            }
    let userNpmDependencies =
          NpmDependencies
            { dependencies =
                D.fromList
                  [ ("a", "1"),
                    ("b", "3")
                  ],
              devDependencies = []
            }

    combineNpmDependencies waspNpmDependencies userNpmDependencies
      `shouldBe` Left
        NpmDependenciesError
          { dependenciesConflictErrors =
              [ DependencyConflictError
                  (D.make ("b", "2"))
                  (D.make ("b", "3"))
              ],
            devDependenciesConflictErrors = []
          }

  it "wasp deps completely overlap with user deps, no duplication" $ do
    let waspNpmDependencies =
          NpmDependencies
            { dependencies = waspDeps,
              devDependencies = []
            }
    let userNpmDependencies =
          NpmDependencies
            { dependencies =
                D.fromList
                  [ ("a", "1"),
                    ("b", "2")
                  ],
              devDependencies = []
            }

    combineNpmDependencies waspNpmDependencies userNpmDependencies
      `shouldBe` Right
        NpmDependencies
          { dependencies =
              D.fromList
                [ ("a", "1"),
                  ("b", "2")
                ],
            devDependencies = []
          }

  it "user dependencies supplement wasp dependencies" $ do
    let waspNpmDependencies =
          NpmDependencies
            { dependencies = waspDeps,
              devDependencies = []
            }
    let userNpmDependencies =
          NpmDependencies
            { dependencies =
                D.fromList
                  [ ("c", "3"),
                    ("d", "4")
                  ],
              devDependencies = []
            }

    combineNpmDependencies waspNpmDependencies userNpmDependencies
      `shouldBe` Right
        NpmDependencies
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
    let waspNpmDependencies =
          NpmDependencies
            { dependencies = waspDeps,
              devDependencies = []
            }
    let userNpmDependencies =
          NpmDependencies
            { dependencies =
                D.fromList
                  [ ("a", "1"),
                    ("d", "4")
                  ],
              devDependencies = []
            }

    combineNpmDependencies waspNpmDependencies userNpmDependencies
      `shouldBe` Right
        NpmDependencies
          { dependencies =
              D.fromList
                [ ("a", "1"),
                  ("b", "2"),
                  ("d", "4")
                ],
            devDependencies = []
          }

  it "report error if user dependency overlaps wasp dependency, different version" $ do
    let waspNpmDependencies =
          NpmDependencies
            { dependencies = waspDeps,
              devDependencies = []
            }
    let userNpmDependencies =
          NpmDependencies
            { dependencies =
                D.fromList
                  [ ("a", "2"),
                    ("foo", "bar")
                  ],
              devDependencies = []
            }

    combineNpmDependencies waspNpmDependencies userNpmDependencies
      `shouldBe` Left
        NpmDependenciesError
          { dependenciesConflictErrors =
              [ DependencyConflictError
                  (D.make ("a", "1"))
                  (D.make ("a", "2"))
              ],
            devDependenciesConflictErrors = []
          }

  it "a conflicting version number is detected with wasp devDependencies" $ do
    let waspNpmDependencies =
          NpmDependencies
            { dependencies = waspDeps,
              devDependencies = waspDevDeps
            }
    let userNpmDependencies =
          NpmDependencies
            { dependencies =
                D.fromList
                  [ ("a", "1"),
                    ("alpha", "70")
                  ],
              devDependencies = []
            }

    combineNpmDependencies waspNpmDependencies userNpmDependencies
      `shouldBe` Left
        NpmDependenciesError
          { dependenciesConflictErrors =
              [ DependencyConflictError
                  (D.make ("alpha", "10"))
                  (D.make ("alpha", "70"))
              ],
            devDependenciesConflictErrors = []
          }

  it "dev dependencies are also combined" $ do
    let waspNpmDependencies =
          NpmDependencies
            { dependencies = waspDeps,
              devDependencies = waspDevDeps
            }

    let userNpmDependencies =
          NpmDependencies
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
    combineNpmDependencies waspNpmDependencies userNpmDependencies
      `shouldBe` Right
        NpmDependencies
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
    let waspNpmDependencies =
          NpmDependencies
            { dependencies = waspDeps,
              devDependencies = waspDevDeps
            }

    let userNpmDependencies =
          NpmDependencies
            { dependencies =
                D.fromList
                  [ ("alpha", "10")
                  ],
              devDependencies = []
            }
    combineNpmDependencies waspNpmDependencies userNpmDependencies
      `shouldBe` Right
        NpmDependencies
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
    "NpmDependencies are equal even if dependencies have different order"
    $ do
      NpmDependencies
        { dependencies = waspDeps,
          devDependencies = []
        }
      `shouldBe` NpmDependencies
        { dependencies = reverse waspDeps,
          devDependencies = []
        }

  it
    "NpmDependencies are equal even if dependencies have different order, with dev dependencies"
    $ do
      NpmDependencies
        { dependencies = waspDeps,
          devDependencies = reverse waspDeps
        }
      `shouldBe` NpmDependencies
        { dependencies = reverse waspDeps,
          devDependencies = waspDeps
        }

  it
    "NpmDependencies can be unequal"
    $ do
      NpmDependencies
        { dependencies = waspDeps,
          devDependencies = []
        }
      `shouldNotBe` NpmDependencies
        { dependencies = [],
          devDependencies = []
        }
