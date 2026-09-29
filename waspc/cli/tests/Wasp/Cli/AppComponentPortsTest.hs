module Wasp.Cli.AppComponentPortsTest where

import Data.Either (isLeft)
import Test.Hspec
import Wasp.Cli.AppComponentPorts (resolveRequestedPort)
import Wasp.Cli.Util.UrlArgument (parseUrl)

spec_resolveRequestedPort :: Spec
spec_resolveRequestedPort = do
  describe "resolveRequestedPort" $ do
    it "requests nothing when neither the option nor the URL has a port" $ do
      resolveRequestedPort "client" Nothing Nothing `shouldBe` Right Nothing
      resolveRequestedPort "client" Nothing (Just $ url "https://my-app.loca.lt") `shouldBe` Right Nothing

    it "requests the port from the option" $ do
      resolveRequestedPort "client" (Just 4000) Nothing `shouldBe` Right (Just 4000)
      resolveRequestedPort "client" (Just 4000) (Just $ url "https://my-app.loca.lt") `shouldBe` Right (Just 4000)

    it "requests the port written in the URL" $ do
      resolveRequestedPort "client" Nothing (Just $ url "http://192.168.1.39.nip.io:4000") `shouldBe` Right (Just 4000)

    it "accepts the option and the URL when their ports agree" $ do
      resolveRequestedPort "client" (Just 4000) (Just $ url "http://192.168.1.39.nip.io:4000") `shouldBe` Right (Just 4000)

    it "rejects the option and the URL when their ports differ" $ do
      resolveRequestedPort "client" (Just 4000) (Just $ url "http://192.168.1.39.nip.io:3000") `shouldSatisfy` isLeft
  where
    url = either error id . parseUrl
