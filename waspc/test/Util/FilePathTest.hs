module Util.FilePathTest where

import Test.Tasty.Hspec (Spec, describe, it, shouldBe)
import Wasp.Util.FilePath (removePathPrefix)

spec_FilePathTest :: Spec
spec_FilePathTest = do
  describe "removePathPrefix" $ do
    it "removes path prefix from a path that includes that prefix" $ do
      removePathPrefix "src/" "src/Component.js" `shouldBe` Just "Component.js"

    it "removes path prefix even if the prefix doesn't have trailing slash" $ do
      removePathPrefix "src" "src/Component.js" `shouldBe` Just "Component.js"

    it "returns Nothing if path doesn't contain the prefix" $ do
      removePathPrefix "src/" "Component.js" `shouldBe` Nothing

    it "returns Nothing if prefix is only partially matched" $ do
      removePathPrefix "src" "srce/Component.js" `shouldBe` Nothing
