module Generator.PackageJsonGeneratorTest where

import Test.Tasty.Hspec
import Wasp.Generator.PackageJsonGenerator (resolveNpmDeps)
import qualified Wasp.NpmDependency as ND

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
    resolveNpmDeps (ND.fromList waspDeps) (ND.fromList userDeps)
      `shouldBe` Right (ND.fromList waspDeps, ND.fromList userDeps)

  it "Does not repeat dep if it is both user and wasp dep." $ do
    let userDeps =
          [ ("axios", "^0.20.0"),
            ("foo", "bar")
          ]
    resolveNpmDeps (ND.fromList waspDeps) (ND.fromList userDeps)
      `shouldBe` Right (ND.fromList waspDeps, ND.fromList [("foo", "bar")])

  it "Reports error if user dep version does not match wasp dep version." $ do
    let userDeps =
          [ ("axios", "^1.20.0"),
            ("foo", "bar")
          ]
    let Left conflicts = resolveNpmDeps (ND.fromList waspDeps) (ND.fromList userDeps)
    map fst conflicts `shouldBe` ND.fromList [("axios", "^1.20.0")]
