module Util.UrlPathTest where

import Test.Hspec
import Wasp.Util.UrlPath

spec_UrlPath :: Spec
spec_UrlPath = do
  describe "stripTrailingSlashes" $ do
    it "keeps the root path" $ stripTrailingSlashes "/" `shouldBe` "/"
    it "removes trailing slashes" $ stripTrailingSlashes "/api//" `shouldBe` "/api"
    it "keeps paths without trailing slashes" $ stripTrailingSlashes "/api/v1" `shouldBe` "/api/v1"

  describe "isPathSegmentPrefixOf" $ do
    it "is true for the same path" $ ("/api" `isPathSegmentPrefixOf` "/api") `shouldBe` True
    it "is true for a descendant path" $ ("/api" `isPathSegmentPrefixOf` "/api/tasks/1") `shouldBe` True
    it "is false for a path that only shares its first letters" $ ("/api" `isPathSegmentPrefixOf` "/apis") `shouldBe` False
    it "is true for the root and any path" $ ("/" `isPathSegmentPrefixOf` "/api") `shouldBe` True
    it "ignores trailing slashes" $ ("/api/" `isPathSegmentPrefixOf` "/api/tasks") `shouldBe` True

  describe "getStaticPathPrefix" $ do
    it "returns a path without patterns as is" $ getStaticPathPrefix "/api/tasks" `shouldBe` "/api/tasks"
    it "stops at a named parameter" $ getStaticPathPrefix "/files/:id/raw" `shouldBe` "/files"
    it "stops at a wildcard" $ getStaticPathPrefix "/files/*path" `shouldBe` "/files"
    it "stops at a segment with an optional part" $ getStaticPathPrefix "/files/raw{.:ext}" `shouldBe` "/files"
    it "stops at a segment with an escaped character" $ getStaticPathPrefix "/files/u\\p" `shouldBe` "/files"
    it "returns the root when the path starts with a pattern" $ getStaticPathPrefix "/:id" `shouldBe` "/"
