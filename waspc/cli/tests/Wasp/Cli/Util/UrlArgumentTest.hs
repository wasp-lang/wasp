module Wasp.Cli.Util.UrlArgumentTest where

import Data.Either (isLeft)
import Test.Hspec
import Wasp.Cli.Util.UrlArgument (parseAppComponentUrl)

spec_parseAppComponentUrl :: Spec
spec_parseAppComponentUrl = do
  describe "parseAppComponentUrl" $ do
    it "accepts http and https URLs with a host" $ do
      parseAppComponentUrl "http://192.168.1.39.nip.io:3000" `shouldBe` Right "http://192.168.1.39.nip.io:3000"
      parseAppComponentUrl "https://my-app.loca.lt" `shouldBe` Right "https://my-app.loca.lt"

    it "accepts URLs with a path" $ do
      parseAppComponentUrl "http://192.168.1.39.nip.io:3000/app/" `shouldBe` Right "http://192.168.1.39.nip.io:3000/app/"

    it "rejects relative URLs" $ do
      parseAppComponentUrl "localhost:3000" `shouldSatisfy` isLeft
      parseAppComponentUrl "/app" `shouldSatisfy` isLeft

    it "rejects schemes other than http and https" $ do
      parseAppComponentUrl "ftp://example.com" `shouldSatisfy` isLeft

    it "rejects URLs without a host" $ do
      parseAppComponentUrl "http://" `shouldSatisfy` isLeft
      parseAppComponentUrl "http:///path" `shouldSatisfy` isLeft

    it "rejects URLs with a query or a fragment" $ do
      parseAppComponentUrl "https://example.com?x=1" `shouldSatisfy` isLeft
      parseAppComponentUrl "https://example.com#top" `shouldSatisfy` isLeft
