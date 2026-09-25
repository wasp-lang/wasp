module ServerRoutes.ServerRouteTest where

import Test.Hspec
import qualified Wasp.AppSpec.Api as AS.Api
import Wasp.ServerRoutes.ServerRoute
  ( ServerRoute (..),
    ServerRouteHttpMethods (..),
    ServerRouteOwner (..),
    ServerRoutePath (..),
    doRoutesOverlap,
  )

spec_ServerRoute :: Spec
spec_ServerRoute = do
  describe "doRoutesOverlap" $ do
    let exactRoute httpMethods = ServerRoute (UserApiRoute "a") httpMethods . ExactPath
    let subtreeRoute httpMethods = ServerRoute (UserApiRoute "b") httpMethods . SubtreePath
    let onlyPost = OnlyHttpMethods [AS.Api.POST]

    it "is true for the same path and method" $
      doRoutesOverlap (exactRoute onlyPost "/foo") (exactRoute onlyPost "/foo") `shouldBe` True
    it "ignores casing and trailing slashes, as Express does" $
      doRoutesOverlap (exactRoute onlyPost "/foo") (exactRoute onlyPost "/FOO/") `shouldBe` True
    it "is false when the routes share no method" $
      doRoutesOverlap (exactRoute onlyPost "/foo") (exactRoute getAndHead "/foo") `shouldBe` False
    it "is true when one of the routes answers on any method" $
      doRoutesOverlap (exactRoute AnyHttpMethod "/foo") (exactRoute getAndHead "/foo") `shouldBe` True
    it "is true for a path under a subtree" $
      doRoutesOverlap (exactRoute onlyPost "/foo/bar") (subtreeRoute onlyPost "/foo") `shouldBe` True
    it "is false for a path that only starts with the same letters as a subtree" $
      doRoutesOverlap (exactRoute onlyPost "/foobar") (subtreeRoute onlyPost "/foo") `shouldBe` False
    it "is true for a subtree inside another subtree" $
      doRoutesOverlap (subtreeRoute onlyPost "/foo/bar") (subtreeRoute onlyPost "/foo") `shouldBe` True
  where
    getAndHead = OnlyHttpMethods [AS.Api.GET, AS.Api.HEAD]
