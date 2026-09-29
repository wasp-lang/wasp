module Wasp.Cli.Util.UrlArgumentTest where

import Data.Either (isLeft)
import Data.Maybe (fromJust)
import Network.URI (parseURI)
import Test.Hspec
import Wasp.Cli.Util.UrlArgument (UrlArgument (..), parseUrl)

spec_parseUrl :: Spec
spec_parseUrl = do
  describe "parseUrl" $ do
    it "accepts http and https URLs with a host" $ do
      parseUrl "http://192.168.1.39.nip.io:3000" `shouldBe` Right (urlArgument "http://192.168.1.39.nip.io:3000" (Just 3000))
      parseUrl "https://my-app.loca.lt" `shouldBe` Right (urlArgument "https://my-app.loca.lt" Nothing)

    it "accepts URLs with a path" $ do
      parseUrl "http://192.168.1.39.nip.io:3000/app/" `shouldBe` Right (urlArgument "http://192.168.1.39.nip.io:3000/app/" (Just 3000))

    it "remembers only a port written in the URL" $ do
      explicitPort <$> parseUrl "https://example.com:8443" `shouldBe` Right (Just 8443)
      explicitPort <$> parseUrl "https://example.com" `shouldBe` Right Nothing
      explicitPort <$> parseUrl "https://example.com:" `shouldBe` Right Nothing

    it "rejects ports outside of 1-65535" $ do
      parseUrl "http://example.com:0" `shouldSatisfy` isLeft
      parseUrl "http://example.com:65536" `shouldSatisfy` isLeft

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
  where
    urlArgument url port = UrlArgument {uri = fromJust $ parseURI url, explicitPort = port}
