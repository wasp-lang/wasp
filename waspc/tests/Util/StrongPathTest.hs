module Util.StrongPathTest where

import Control.Exception (evaluate)
import StrongPath (fromRelDir, reldir)
import Test.Hspec (Spec, anyErrorCall, describe, it, shouldBe, shouldThrow)
import Wasp.Util.StrongPath (invertRelDir)

spec_invertRelDir :: Spec
spec_invertRelDir = do
  describe "invertRelDir" $ do
    it "throws on dotdot at start of path" $ do
      evaluate (invertRelDir [reldir|../|]) `shouldThrow` anyErrorCall
      evaluate (invertRelDir [reldir|../../a|]) `shouldThrow` anyErrorCall

    it "returns current dir for current dir input" $ do
      fromRelDir (invertRelDir [reldir|.|]) `shouldBe` fromRelDir [reldir|./|]
      fromRelDir (invertRelDir [reldir|././././././././././|]) `shouldBe` fromRelDir [reldir|./|]

    it "converts path segments into dotdots" $ do
      fromRelDir (invertRelDir [reldir|types|]) `shouldBe` fromRelDir [reldir|../|]
      fromRelDir (invertRelDir [reldir|.wasp/out|]) `shouldBe` fromRelDir [reldir|../../|]
      fromRelDir (invertRelDir [reldir|a/b/c|]) `shouldBe` fromRelDir [reldir|../../../|]

    it "ignores current dir in path" $ do
      fromRelDir (invertRelDir [reldir|./././middle/./././|]) `shouldBe` fromRelDir [reldir|../|]
      fromRelDir (invertRelDir [reldir|./.wasp/./out/./|]) `shouldBe` fromRelDir [reldir|../../|]
