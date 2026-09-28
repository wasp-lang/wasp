module Wasp.Cli.Util.UrlArgumentTest where

import Data.Either (isLeft)
import Data.Maybe (fromJust)
import Network.URI (parseURI)
import Test.Hspec
import Wasp.Cli.Util.UrlArgument (parseUrl)

spec_parseUrl :: Spec
spec_parseUrl = do
  describe "parseUrl" $ do
    it "accepts http and https URLs with a host" $ do
      parseUrl "http://192.168.1.39.nip.io:3000" `shouldBe` Right (fromJust $ parseURI "http://192.168.1.39.nip.io:3000")
      parseUrl "https://my-app.loca.lt" `shouldBe` Right (fromJust $ parseURI "https://my-app.loca.lt")

    it "accepts URLs with a path" $ do
      parseUrl "http://192.168.1.39.nip.io:3000/app/" `shouldBe` Right (fromJust $ parseURI "http://192.168.1.39.nip.io:3000/app/")

    it "rejects relative URLs" $ do
      parseUrl "localhost:3000" `shouldSatisfy` isLeft
      parseUrl "/app" `shouldSatisfy` isLeft

    it "rejects schemes other than http and https" $ do
      parseUrl "ftp://example.com" `shouldSatisfy` isLeft

    it "rejects URLs without a host" $ do
      parseUrl "http://" `shouldSatisfy` isLeft
      parseUrl "http:///path" `shouldSatisfy` isLeft

    it "rejects URLs with a query or a fragment" $ do
      parseUrl "https://example.com?x=1" `shouldSatisfy` isLeft
      parseUrl "https://example.com#top" `shouldSatisfy` isLeft
