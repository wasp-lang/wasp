module Generator.PackageJsonGeneratorTest where

import Test.Tasty.Hspec
import qualified Wasp.AppSpec.App.Dependency as D
import Wasp.Generator.PackageJsonGenerator (resolveNpmDeps)

spec_resolveNpmDeps :: Spec
spec_resolveNpmDeps = do
  let waspDeps =
        [ ("axios", "^0.20.0"),
          ("lodash", "^4.17.15")
        ]

  it "Concatenates two distincts lists of deps." $ do
    let userDeps =
          [ ("foo", "bar"),
            ("foo2", "bar2")
          ]
    resolveNpmDeps (D.fromList waspDeps) (D.fromList userDeps)
      `shouldBe` Right (D.fromList waspDeps, D.fromList userDeps)

  it "Does not repeat dep if it is both user and wasp dep." $ do
    let userDeps =
          [ ("axios", "^0.20.0"),
            ("foo", "bar")
          ]
    resolveNpmDeps (D.fromList waspDeps) (D.fromList userDeps)
      `shouldBe` Right (D.fromList waspDeps, D.fromList [("foo", "bar")])

  it "Reports error if user dep version does not match wasp dep version." $ do
    let userDeps =
          [ ("axios", "^1.20.0"),
            ("foo", "bar")
          ]
    let Left conflicts = resolveNpmDeps (D.fromList waspDeps) (D.fromList userDeps)
    map fst conflicts `shouldBe` D.fromList [("axios", "^1.20.0")]
