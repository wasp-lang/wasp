module Util.WebRouterPathTest where

import Test.Hspec (Spec, describe, it, shouldBe)
import Wasp.Util.WebRouterPath (doesConcretePathMatchRoutePattern)

spec_WebRouterPathTest :: Spec
spec_WebRouterPathTest = do
  describe "doesConcretePathMatchRoutePattern" $ do
    let matches routePattern path = doesConcretePathMatchRoutePattern routePattern path `shouldBe` True
    let doesNotMatch routePattern path = doesConcretePathMatchRoutePattern routePattern path `shouldBe` False

    it "matches identical static paths" $ do
      matches "/about" "/about"
      matches "/" "/"
      matches "/foo/bar" "/foo/bar"

    it "does not match different static paths" $ do
      doesNotMatch "/about" "/contact"
      doesNotMatch "/foo/bar" "/foo/baz"

    it "does not match when the number of segments differs" $ do
      doesNotMatch "/foo" "/foo/bar"
      doesNotMatch "/foo/bar" "/foo"

    it "matches a dynamic param against any single non-empty segment" $ do
      matches "/blog/:slug" "/blog/intro"
      matches "/users/:id/posts/:postId" "/users/42/posts/7"

    it "does not match a dynamic param against the wrong static prefix" $ do
      doesNotMatch "/blog/:slug" "/not-blog/intro"

    it "does not match a dynamic param against extra segments" $ do
      doesNotMatch "/blog/:slug" "/blog/intro/extra"

    it "matches a trailing splat against any number of remaining segments" $ do
      matches "/files/*" "/files/readme.md"
      matches "/files/*" "/files/docs/readme.md"
      matches "/files/*" "/files"

    it "matches optional segments whether present or absent" $ do
      matches "/photo/:id/edit?" "/photo/42/edit"
      matches "/photo/:id/edit?" "/photo/42"
      matches "/users/:id?" "/users/42"
      matches "/users/:id?" "/users"
