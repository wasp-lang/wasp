module Util.UrlPathTest where

import Test.Hspec
import Wasp.Util.UrlPath

spec_UrlPath :: Spec
spec_UrlPath = do
  describe "stripTrailingSlashes" $ do
    it "keeps the root path" $ stripTrailingSlashes "/" `shouldBe` "/"
    it "removes trailing slashes" $ stripTrailingSlashes "/api//" `shouldBe` "/api"
    it "keeps paths without trailing slashes" $ stripTrailingSlashes "/api/v1" `shouldBe` "/api/v1"
