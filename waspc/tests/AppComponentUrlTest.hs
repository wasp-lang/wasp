module AppComponentUrlTest where

import Network.URI (parseURI)
import StrongPath (absdirP)
import Test.Hspec
import Wasp.AppComponentUrl (AppComponentUrl (..), localUrl, url)

spec_url :: Spec
spec_url = do
  describe "url" $ do
    it "builds a localhost URL from the port and the optional path" $ do
      url AppComponentUrl {port = 3001, path = Nothing, customUrl = Nothing} `shouldBe` "http://localhost:3001"
      url localClientUrl `shouldBe` "http://localhost:3000/app/"

    it "returns the custom URL as given, regardless of the port and the path" $ do
      url customClientUrl `shouldBe` "https://my-app.loca.lt"

  describe "localUrl" $ do
    it "builds a localhost URL from the port and the optional path, even for custom URLs" $ do
      localUrl localClientUrl `shouldBe` "http://localhost:3000/app/"
      localUrl customClientUrl `shouldBe` "http://localhost:3000/app/"
  where
    localClientUrl = AppComponentUrl {port = 3000, path = Just [absdirP|/app/|], customUrl = Nothing}
    customClientUrl = localClientUrl {customUrl = parseURI "https://my-app.loca.lt"}
