module Wasp.Cli.Util.HttpUrlArgumentTest where

import Data.Either (isLeft)
import Data.Maybe (fromJust)
import Network.URI (parseURI)
import Test.Hspec
import Wasp.Cli.Util.HttpUrlArgument (parseHttpUrl)

spec_parseUrl :: Spec
spec_parseUrl = do
  describe "parseHttpUrl" $ do
    it "accepts http and https URLs with a host" $ do
      parseHttpUrl "http://192.168.1.39.nip.io:3000" `shouldBe` Right (fromJust $ parseURI "http://192.168.1.39.nip.io:3000")
      parseHttpUrl "https://my-app.loca.lt" `shouldBe` Right (fromJust $ parseURI "https://my-app.loca.lt")

    it "accepts URLs with a path" $ do
      parseHttpUrl "http://192.168.1.39.nip.io:3000/app/" `shouldBe` Right (fromJust $ parseURI "http://192.168.1.39.nip.io:3000/app/")

    it "rejects relative URLs" $ do
      parseHttpUrl "localhost:3000" `shouldSatisfy` isLeft
      parseHttpUrl "/app" `shouldSatisfy` isLeft

    it "rejects schemes other than http and https" $ do
      parseHttpUrl "ftp://example.com" `shouldSatisfy` isLeft

    it "rejects URLs without a host" $ do
      parseHttpUrl "http://" `shouldSatisfy` isLeft
      parseHttpUrl "http:///path" `shouldSatisfy` isLeft

    it "rejects URLs with a query or a fragment" $ do
      parseHttpUrl "https://example.com?x=1" `shouldSatisfy` isLeft
      parseHttpUrl "https://example.com#top" `shouldSatisfy` isLeft
