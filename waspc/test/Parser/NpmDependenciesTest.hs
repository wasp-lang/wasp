module Parser.NpmDependenciesTest where

import           Test.Tasty.Hspec

import           Data.Aeson             ((.=))
import           Data.Either            (isLeft)
import           Data.HashMap.Strict    (fromList)

import qualified NpmDependency          as ND
import           Parser.Common          (runWaspParser)
import           Parser.NpmDependencies (npmDependencies)
import           Wasp.NpmDependencies


spec_parseNpmDependencies :: Spec
spec_parseNpmDependencies = do
    describe "Parsing npm dependencies" $ do
        it "When given a valid declaration with valid json, parses it correctly" $ do
            runWaspParser npmDependencies "dependencies {=json \"foo\": \"test1\", \"bar\": \"test2\" json=}"
                `shouldBe` Right NpmDependencies
                               { _dependencies =
                                       [ ND.NpmDependency { ND._name = "foo", ND._version = "test1" }
                                       , ND.NpmDependency { ND._name = "bar", ND._version = "test2" }
                                       ]
                               }
        it "When given invalid json, reports error" $ do
            isLeft (runWaspParser npmDependencies "dependencies {=json foo: 42 json=}")
                `shouldBe` True
