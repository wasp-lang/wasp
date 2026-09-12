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
    it "matches equal paths" $ ("/api" `isPathSegmentPrefixOf` "/api") `shouldBe` True
    it "matches ancestor paths" $ ("/api" `isPathSegmentPrefixOf` "/api/auth/me") `shouldBe` True
    it "does not match partial segments" $ ("/api" `isPathSegmentPrefixOf` "/apis") `shouldBe` False
    it "treats the root as a prefix of everything" $ ("/" `isPathSegmentPrefixOf` "/anything") `shouldBe` True
    it "does not treat a child as a prefix of its parent" $ ("/api/auth" `isPathSegmentPrefixOf` "/api") `shouldBe` False

  describe "getStaticPathPrefix" $ do
    it "keeps fully static paths" $ getStaticPathPrefix "/foo/bar" `shouldBe` "/foo/bar"
    it "cuts at a param segment" $ getStaticPathPrefix "/foo/:id/edit" `shouldBe` "/foo"
    it "cuts at a splat segment" $ getStaticPathPrefix "/files/*" `shouldBe` "/files"
    it "cuts at an optional segment" $ getStaticPathPrefix "/foo/bar?/baz" `shouldBe` "/foo"
    it "cuts at a regex group segment" $ getStaticPathPrefix "/foo/(a|b)" `shouldBe` "/foo"
    it "returns the root for a leading param" $ getStaticPathPrefix "/:id" `shouldBe` "/"
    it "returns the root for the root" $ getStaticPathPrefix "/" `shouldBe` "/"
