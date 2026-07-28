module Project.Db.Dev.PostgresTest where

import Test.Hspec
import Wasp.Project.Db.Dev.Postgres (parseDockerPortOutput)

spec_parseDockerPortOutput :: Spec
spec_parseDockerPortOutput = do
  describe "parseDockerPortOutput" $ do
    it "parses single-line output" $ do
      parseDockerPortOutput "0.0.0.0:5432\n" `shouldBe` Just 5432

    it "parses multi-line output by using the first line" $ do
      parseDockerPortOutput "0.0.0.0:5433\n[::]:5434\n" `shouldBe` Just 5433

    it "parses an IPv6 first line" $ do
      parseDockerPortOutput "[::]:5433\n0.0.0.0:5433\n" `shouldBe` Just 5433

    it "returns Nothing on empty output" $ do
      parseDockerPortOutput "" `shouldBe` Nothing

    it "returns Nothing on output without a port" $ do
      parseDockerPortOutput "no port mapping found\n" `shouldBe` Nothing
