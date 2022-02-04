module Generator.PackageJsonGeneratorTest where

import Test.Tasty.Hspec
import qualified Wasp.AppSpec.App.Dependency as D
import Wasp.Generator.PackageJsonGenerator
  ( DependencyConflictError (DependencyConflictError),
    resolveDependencies,
    resolveNpmDeps,
  )

spec_resolveNpmDeps :: Spec
spec_resolveNpmDeps = do
  let waspDeps =
        D.fromList
          [ ("a", "1"),
            ("b", "2")
          ]

  it "a conflicting version number is detected" $ do
    let userDeps =
          D.fromList
            [ ("a", "1"),
              ("b", "3")
            ]
    resolveDependencies waspDeps userDeps
      `shouldBe` Left
        [ DependencyConflictError
            (D.create ("b", "2"))
            (D.create ("b", "3"))
        ]
  it "same versions so no conflict" $ do
    let userDeps =
          D.fromList
            [ ("a", "1"),
              ("b", "2")
            ]
    resolveDependencies waspDeps userDeps
      `shouldBe` Right (waspDeps, [])

  it "no version overlap so no conflict" $ do
    let userDeps =
          D.fromList
            [ ("c", "1"),
              ("d", "2")
            ]
    resolveDependencies waspDeps userDeps
      `shouldBe` Right (waspDeps, userDeps)

  it "partial overlap without conflict" $ do
    let userDeps =
          D.fromList
            [ ("a", "1"),
              ("d", "2")
            ]
    resolveDependencies waspDeps userDeps
      `shouldBe` Right (waspDeps, [D.create ("d", "2")])

  it "Reports error if user dep version does not match wasp dep version" $ do
    let userDeps =
          D.fromList
            [ ("a", "2"),
              ("foo", "bar")
            ]
    let Left conflicts = resolveNpmDeps waspDeps userDeps
    conflicts
      `shouldBe` [ ( D.create ("a", "2"),
                     "Error: Dependency conflict for user dependency (a, 2): "
                       ++ "Version must be set to the exactly "
                       ++ "the same version as the one wasp is using: 1"
                   )
                 ]