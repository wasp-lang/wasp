module Parser.NpmDependenciesTest where

import Data.Either (isLeft)
import Test.Tasty.Hspec
import qualified Wasp.NpmDependency as ND
import Wasp.Parser.Common (runWaspParser)
import Wasp.Parser.NpmDependencies (npmDependencies)
import Wasp.Wasp.NpmDependencies

spec_parseNpmDependencies :: Spec
spec_parseNpmDependencies = do
  describe "Parsing npm dependencies" $ do
    it "When given a valid declaration with valid json, parses it correctly" $ do
      runWaspParser npmDependencies "dependencies {=json \"foo\": \"test1\", \"bar\": \"test2\" json=}"
        `shouldBe` Right
          NpmDependencies
            { _dependencies =
                [ ND.NpmDependency {ND._name = "foo", ND._version = "test1"},
                  ND.NpmDependency {ND._name = "bar", ND._version = "test2"}
                ]
            }
    it "When given invalid json, reports error" $ do
      isLeft (runWaspParser npmDependencies "dependencies {=json foo: 42 json=}")
        `shouldBe` True
