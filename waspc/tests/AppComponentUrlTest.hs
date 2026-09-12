module AppComponentUrlTest where

import StrongPath (absdirP)
import Test.Hspec
import Wasp.AppComponentUrl (AppComponentUrl (..))
import qualified Wasp.AppComponentUrl as AppComponentUrl

spec_AppComponentUrl :: Spec
spec_AppComponentUrl = do
  describe "origin" $ do
    it "leaves out the path" $ do
      AppComponentUrl.origin (Local {port = 3000, path = Just [absdirP|/app/|]})
        `shouldBe` "http://localhost:3000"

  describe "url" $ do
    it "appends the path when there is one" $ do
      AppComponentUrl.url (Local {port = 3000, path = Just [absdirP|/app/|]})
        `shouldBe` "http://localhost:3000/app/"

    it "is the origin when there is no path" $ do
      AppComponentUrl.url (Local {port = 3001, path = Nothing})
        `shouldBe` "http://localhost:3001"
