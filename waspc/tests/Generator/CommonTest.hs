module Generator.CommonTest where

import Test.Tasty.Hspec
import Wasp.Generator.Common (makeJsArrayFromHaskellList)

spec_GeneratorCommonTest :: Spec
spec_GeneratorCommonTest = do
  describe "makeJsArrayFromHaskellList" $ do
    it "creates an empty JS array from empty Haskell list" $ do
      makeJsArrayFromHaskellList []
        `shouldBe` "[]"

    it "creates an array of strings from Haskell list of strings" $ do
      makeJsArrayFromHaskellList ["one", "two", "three"]
        `shouldBe` "['one', 'two', 'three']"
