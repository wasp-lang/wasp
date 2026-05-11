module Util.StrongPathTest where

import Control.Exception (evaluate)
import StrongPath (reldir)
import Test.Hspec (Spec, anyErrorCall, describe, it, shouldBe, shouldThrow)
import Wasp.Util.StrongPath (invertRelDir)

spec_invertRelDir :: Spec
spec_invertRelDir = do
  describe "invertRelDir" $ do
    it "throws on dotdot at start of path" $ do
      evaluate (invertRelDir [reldir|../|]) `shouldThrow` anyErrorCall
      evaluate (invertRelDir [reldir|../../a|]) `shouldThrow` anyErrorCall

    it "returns current dir for current dir input" $ do
      invertRelDir [reldir|.|] `shouldBe` [reldir|./|]
      invertRelDir [reldir|././././././././././|] `shouldBe` [reldir|./|]

    it "converts path segments into dotdots" $ do
      invertRelDir [reldir|types|] `shouldBe` [reldir|../|]
      invertRelDir [reldir|.wasp/out|] `shouldBe` [reldir|../../|]
      invertRelDir [reldir|a/b/c|] `shouldBe` [reldir|../../../|]

    it "ignores current dir in path" $ do
      invertRelDir [reldir|./././middle/./././|] `shouldBe` [reldir|../|]
      invertRelDir [reldir|./.wasp/./out/./|] `shouldBe` [reldir|../../|]
