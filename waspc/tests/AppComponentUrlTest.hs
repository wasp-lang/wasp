module AppComponentUrlTest where

import Data.Maybe (fromJust)
import Network.URI (parseURI)
import StrongPath (absdirP)
import Test.Hspec
import Wasp.AppComponentUrl (AppComponentUrl (..), isCustom, url)

spec_url :: Spec
spec_url = do
  describe "url" $ do
    it "builds a localhost URL from the port and the optional path" $ do
      url Local {port = 3001, path = Nothing} `shouldBe` "http://localhost:3001"
      url Local {port = 3000, path = Just [absdirP|/app/|]} `shouldBe` "http://localhost:3000/app/"

    it "returns the custom URL as given, regardless of the port" $ do
      url Custom {port = 3001, publicUrl = fromJust $ parseURI "https://my-app.loca.lt"} `shouldBe` "https://my-app.loca.lt"

  describe "isCustom" $ do
    it "tells custom URLs apart from local ones" $ do
      isCustom Custom {port = 3001, publicUrl = fromJust $ parseURI "https://my-app.loca.lt"} `shouldBe` True
      isCustom Local {port = 3001, path = Nothing} `shouldBe` False
