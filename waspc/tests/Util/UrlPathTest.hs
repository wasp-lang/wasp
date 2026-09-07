module Util.UrlPathTest where

import Test.Hspec
import Wasp.Util.UrlPath

spec_UrlPath :: Spec
spec_UrlPath = do
  describe "stripTrailingSlashes" $ do
    it "keeps the root path" $ stripTrailingSlashes "/" `shouldBe` "/"
    it "removes trailing slashes" $ stripTrailingSlashes "/api//" `shouldBe` "/api"
    it "keeps paths without trailing slashes" $ stripTrailingSlashes "/api/v1" `shouldBe` "/api/v1"

  describe "toUrlPathPrefix" $ do
    it "turns the root path into an empty string" $ toUrlPathPrefix "/" `shouldBe` ""
    it "removes trailing slashes" $ toUrlPathPrefix "/api/" `shouldBe` "/api"
    it "keeps paths without trailing slashes" $ toUrlPathPrefix "/api/v1" `shouldBe` "/api/v1"

  describe "isPathSegmentPrefixOf" $ do
    it "matches equal paths" $ ("/api" `isPathSegmentPrefixOf` "/api") `shouldBe` True
    it "matches ancestor paths" $ ("/api" `isPathSegmentPrefixOf` "/api/auth/me") `shouldBe` True
    it "does not match partial segments" $ ("/api" `isPathSegmentPrefixOf` "/apis") `shouldBe` False
    it "treats the root as a prefix of everything" $ ("/" `isPathSegmentPrefixOf` "/anything") `shouldBe` True
    it "does not treat a child as a prefix of its parent" $ ("/api/auth" `isPathSegmentPrefixOf` "/api") `shouldBe` False
