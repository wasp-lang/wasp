module Util.FilePathTest where

import qualified System.FilePath as FP
import Test.Tasty.Hspec (Spec, describe, it, shouldBe)
import Wasp.Util.FilePath (removePathPrefix)

spec_FilePathTest :: Spec
spec_FilePathTest = do
  describe "removePathPrefix" $ do
    it "removes path prefix from a path that includes that prefix" $ do
      removePathPrefix prefixWithTrailingSlash ("src" FP.</> "Component.js") `shouldBe` Just "Component.js"

    it "removes path prefix even if the prefix doesn't have trailing slash" $ do
      removePathPrefix prefixWithoutTrailingSlash ("src" FP.</> "Component.js") `shouldBe` Just "Component.js"

    it "returns Nothing if path doesn't contain the prefix" $ do
      removePathPrefix prefixWithTrailingSlash "Component.js" `shouldBe` Nothing

    it "returns Nothing if prefix is only partially matched" $ do
      removePathPrefix prefixWithoutTrailingSlash ("srce" FP.</> "Component.js") `shouldBe` Nothing
  where
    prefixWithTrailingSlash = "src" ++ [FP.pathSeparator]
    prefixWithoutTrailingSlash = "src"
