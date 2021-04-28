module WaspignoreFileTest where

import Test.Tasty.Hspec
import Test.Tasty.QuickCheck (property)
import WaspignoreFile (ignores, parseWaspignoreFile)

spec_IgnoreFile :: Spec
spec_IgnoreFile = do
  describe "IgnoreFile" $ do
    it "When given a single pattern, should match it and '.waspignore'" $ do
      let ignoreFile = parseWaspignoreFile "*.tmp"
      (ignoreFile `ignores` "a.tmp") `shouldBe` True
      (ignoreFile `ignores` "a.src") `shouldBe` False
      (ignoreFile `ignores` ".waspignore") `shouldBe` True

    it "When given a blank input, should match only '.waspignore'" $ do
      let ignoreFile = parseWaspignoreFile ""
      property $ \fp ->
        if fp == ".waspignore"
          then ignoreFile `ignores` fp
          else not $ ignoreFile `ignores` fp

    it "When given a comment as the only line, should match only '.waspignore'" $ do
      let ignoreFile = parseWaspignoreFile "# test comment"
      property $ \fp ->
        if fp == ".waspignore"
          then ignoreFile `ignores` fp
          else not $ ignoreFile `ignores` fp

    it "When the only difference between two files is a comment, the files should match the same strings" $ do
      let comment = "\n# test comment"
      property $ \pat fp ->
        (parseWaspignoreFile pat `ignores` fp)
          == (parseWaspignoreFile (pat ++ comment) `ignores` fp)

    it "When given 2 patterns, should match the path if either of the patterns match" $ do
      let pat1 = parseWaspignoreFile "a"
      let pat2 = parseWaspignoreFile "b"
      let patBoth = parseWaspignoreFile "a\nb"
      property $ \fp -> patBoth `ignores` fp == (pat1 `ignores` fp || pat2 `ignores` fp)
