module AppComponentUrlTest where

import Network.URI (parseURI)
import StrongPath (absdirP)
import Test.Hspec
import Wasp.AppComponentUrl (AppComponentUrl (..), makeAppComponentUrl)

spec_makeAppComponentUrl :: Spec
spec_makeAppComponentUrl = do
  describe "makeAppComponentUrl" $ do
    it "builds a localhost URL from the port and the optional path" $ do
      makeAppComponentUrl 3001 Nothing Nothing
        `shouldBe` AppComponentUrl {port = 3001, url = "http://localhost:3001", localUrl = "http://localhost:3001"}
      makeAppComponentUrl 3000 (Just [absdirP|/app/|]) Nothing
        `shouldBe` AppComponentUrl {port = 3000, url = "http://localhost:3000/app/", localUrl = "http://localhost:3000/app/"}

    it "uses the custom URL as given, and keeps the localhost URL next to it" $ do
      makeAppComponentUrl 3000 (Just [absdirP|/app/|]) (parseURI "https://my-app.loca.lt")
        `shouldBe` AppComponentUrl {port = 3000, url = "https://my-app.loca.lt", localUrl = "http://localhost:3000/app/"}
