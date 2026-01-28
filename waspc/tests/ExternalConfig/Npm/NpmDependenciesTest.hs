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

spec_mergeWithUserOverrides :: Spec
spec_mergeWithUserOverrides = do
  it "uses user version for overridden dependency" $ do
    let waspDeps = makeWaspDeps [("react", "18.0.0")] []
        userDeps = makeUserDeps [("react", "17.0.0")] []
        result = mergeWithUserOverrides waspDeps userDeps
    sortedDeps (dependencies result) `shouldBe` [("react", "17.0.0")]

  it "only overrides specified packages" $ do
    let waspDeps = makeWaspDeps [("react", "18.0.0"), ("lodash", "4.0.0")] []
        userDeps = makeUserDeps [("react", "17.0.0")] []
        result = mergeWithUserOverrides waspDeps userDeps
    sortedDeps (dependencies result) `shouldBe` [("lodash", "4.0.0"), ("react", "17.0.0")]

  it "doesn't add user dependency when it's in override list but not in wasp deps" $ do
    let waspDeps = makeWaspDeps [("react", "18.0.0")] []
        userDeps = makeUserDeps [("axios", "1.0.0")] []
        result = mergeWithUserOverrides waspDeps userDeps
    sortedDeps (dependencies result) `shouldBe` [("react", "18.0.0")]

  it "handles mixed dependencies and devDependencies" $ do
    let waspDeps = makeWaspDeps [("typescript", "5.0.0")] [("jest", "29.0.0")]
        userDeps = makeUserDeps [("jest", "28.0.0")] [("typescript", "4.9.0")]
        result = mergeWithUserOverrides waspDeps userDeps
    sortedDeps (dependencies result) `shouldBe` [("typescript", "4.9.0")]
    sortedDeps (devDependencies result) `shouldBe` [("jest", "28.0.0")]

-- Helper functions

makeWaspDeps :: [(String, String)] -> [(String, String)] -> NpmDepsFromWasp
makeWaspDeps deps devDeps =
  NpmDepsFromWasp $
    NpmDepsForPackage
      { dependencies = D.fromList deps,
        devDependencies = D.fromList devDeps,
        peerDependencies = []
      }

makeUserDeps :: [(String, String)] -> [(String, String)] -> NpmDepsFromUser
makeUserDeps deps devDeps =
  NpmDepsFromUser $
    NpmDepsForPackage
      { dependencies = D.fromList deps,
        devDependencies = D.fromList devDeps,
        peerDependencies = []
      }

sortedDeps :: [D.Dependency] -> [(String, String)]
sortedDeps = sortOn fst . map D.toPair
