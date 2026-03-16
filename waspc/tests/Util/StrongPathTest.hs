module Util.StrongPathTest where

import Control.Exception (evaluate)
import StrongPath (fromRelDir, reldir)
import Test.Hspec (Spec, anyErrorCall, describe, it, shouldBe, shouldThrow)
import Wasp.Util.StrongPath (invertRelDir)

spec_invertRelDir :: Spec
spec_invertRelDir = do
  describe "invertRelDir" $ do
    it "throws on single dotdot segment" $ do
      evaluate (invertRelDir [reldir|../|]) `shouldThrow` anyErrorCall

    it "throws on two dotdot segments" $ do
      evaluate (invertRelDir [reldir|../../|]) `shouldThrow` anyErrorCall

    it "throws on three dotdot segments" $ do
      evaluate (invertRelDir [reldir|../../../|]) `shouldThrow` anyErrorCall

    it "returns current dir for current dir input" $ do
      fromRelDir (invertRelDir [reldir|.|]) `shouldBe` "./"

    it "returns one parent for single-segment path" $ do
      fromRelDir (invertRelDir [reldir|types|]) `shouldBe` "../"

    it "returns two parents for two-segment path" $ do
      fromRelDir (invertRelDir [reldir|.wasp/out|]) `shouldBe` "../../"

    it "returns three parents for three-segment path" $ do
      fromRelDir (invertRelDir [reldir|a/b/c|]) `shouldBe` "../../../"
