module Wasp.Cli.Util.EnvVarArgumentTest where

import Test.Tasty.Hspec
import Wasp.Cli.Util.EnvVarArgument (envVarFromString)

spec_envVarFromString :: Spec
spec_envVarFromString = do
  describe "envVarFromString" $ do
    describe "valid environment variable formats" $ do
      it "parses simple name=value format" $ do
        envVarFromString "NAME=value" `shouldBe` Right ("NAME", "value")

      it "preserves uppercase and lowercase" $ do
        envVarFromString "MyVar=teSt" `shouldBe` Right ("MyVar", "teSt")

      it "parses variable with empty value" $ do
        envVarFromString "EMPTY=" `shouldBe` Right ("EMPTY", "")

      it "parses variable with value containing spaces" $ do
        envVarFromString "MESSAGE=hello world" `shouldBe` Right ("MESSAGE", "hello world")

      it "parses variable with value containing special characters" $ do
        envVarFromString "SPECIAL=!@#$%^&*()" `shouldBe` Right ("SPECIAL", "!@#$%^&*()")

      it "parses variable with value containing equals signs" $ do
        envVarFromString "EQUATION=x=y+z" `shouldBe` Right ("EQUATION", "x=y+z")

      it "parses variable with value containing multiple equals signs" $ do
        envVarFromString "COMPLEX=a=b=c=d" `shouldBe` Right ("COMPLEX", "a=b=c=d")

      it "parses variable with JSON value" $ do
        envVarFromString "CONFIG={\"key\":\"value\",\"number\":42}"
          `shouldBe` Right ("CONFIG", "{\"key\":\"value\",\"number\":42}")

      it "parses variable with URL value" $ do
        envVarFromString "API_URL=https://api.example.com/v1"
          `shouldBe` Right ("API_URL", "https://api.example.com/v1")

      it "parses variable with numeric name (though unusual)" $ do
        envVarFromString "123=numeric_name" `shouldBe` Right ("123", "numeric_name")

      it "parses variable with underscores and numbers in name" $ do
        envVarFromString "VAR_123_TEST=value" `shouldBe` Right ("VAR_123_TEST", "value")

      it "allows variable with leading whitespace in name" $ do
        envVarFromString " NAME=value" `shouldBe` Right (" NAME", "value")

    describe "invalid environment variable formats" $ do
      it "rejects empty string" $ do
        envVarFromString "" `shouldBe` Left "Environment variable must be in the format NAME=VALUE: "

      it "rejects string with no equals sign" $ do
        envVarFromString "INVALID"
          `shouldBe` Left "Environment variable must be in the format NAME=VALUE: INVALID"

      it "rejects string starting with equals sign" $ do
        envVarFromString "=value"
          `shouldBe` Left "Environment variable must be in the format NAME=VALUE: =value"

      it "rejects string with only equals sign" $ do
        envVarFromString "="
          `shouldBe` Left "Environment variable must be in the format NAME=VALUE: ="

      it "rejects whitespace-only string" $ do
        envVarFromString "   "
          `shouldBe` Left "Environment variable must be in the format NAME=VALUE:    "

    describe "edge cases" $ do
      it "parses single character name and value" $ do
        envVarFromString "A=B" `shouldBe` Right ("A", "B")

      it "parses variable with newline in value" $ do
        envVarFromString "MULTILINE=line1\nline2" `shouldBe` Right ("MULTILINE", "line1\nline2")

      it "parses variable with tab character in value" $ do
        envVarFromString "TABBED=value\twith\ttabs" `shouldBe` Right ("TABBED", "value\twith\ttabs")

      it "parses variable with quotes in value" $ do
        envVarFromString "QUOTED=This is a \"quoted\" value"
          `shouldBe` Right ("QUOTED", "This is a \"quoted\" value")

      it "parses variable with backslashes in value" $ do
        envVarFromString "PATH=C:\\Windows\\System32"
          `shouldBe` Right ("PATH", "C:\\Windows\\System32")
