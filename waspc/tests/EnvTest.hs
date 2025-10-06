module EnvTest where

import qualified Data.Text as T
import NeatInterpolation (trimming)
import Test.Tasty.Hspec
import Wasp.Env
  ( envVarsToDotEnvContent,
    formatEnvVarValue,
  )

spec_envVarHelpers :: Spec
spec_envVarHelpers = do
  describe "formatEnvVarValue" $ do
    it "should handle simple string values correctly" $ do
      envVarValueShouldBeFormattedCorrectly "simple_value" "simple_value"

    it "should handle JSON values correctly" $ do
      envVarValueShouldBeFormattedCorrectly "{\"teamConcurrency\":3}" "{\"teamConcurrency\":3}"

    it "should handle nested JSON objects correctly" $ do
      envVarValueShouldBeFormattedCorrectly
        "{\"database\":{\"host\":\"localhost\",\"port\":5432},\"options\":[\"option1\",\"option2\"]}"
        "{\"database\":{\"host\":\"localhost\",\"port\":5432},\"options\":[\"option1\",\"option2\"]}"

    it "should handle boolean and number values in JSON correctly" $ do
      envVarValueShouldBeFormattedCorrectly
        "{\"enabled\":true,\"maxRetries\":5,\"timeout\":30.5}"
        "{\"enabled\":true,\"maxRetries\":5,\"timeout\":30.5}"

    it "should handle values that need quoting in shell environments" $ do
      envVarValueShouldBeFormattedCorrectly "hello world with spaces" "\"hello world with spaces\""
      envVarValueShouldBeFormattedCorrectly "{\"teamConcurrency\":3, \"retryLimit\":2}" "\"{\"teamConcurrency\":3, \"retryLimit\":2}\""

    it "should handle special characters correctly" $ do
      envVarValueShouldBeFormattedCorrectly "value$with&special=chars" "value$with&special=chars"

  describe "envVarsToDotEnvContent" $ do
    it "should handle multiple environment variables correctly" $ do
      let envVars =
            [ ("DATABASE_URL", "postgresql://localhost:5432/mydb"),
              ("PG_BOSS_NEW_OPTIONS", "{\"teamConcurrency\":3,\"retryLimit\":2}"),
              ("SIMPLE_VAR", "value")
            ]
      let expected =
            [trimming|
              DATABASE_URL=postgresql://localhost:5432/mydb
              PG_BOSS_NEW_OPTIONS={"teamConcurrency":3,"retryLimit":2}
              SIMPLE_VAR=value
            |]

      envVarsToDotEnvContent envVars `shouldBe` expected

    it "should handle empty values correctly" $ do
      envVarsToDotEnvContent [("EMPTY_VAR", "")] `shouldBe` T.pack "EMPTY_VAR="
  where
    envVarValueShouldBeFormattedCorrectly :: String -> String -> Expectation
    envVarValueShouldBeFormattedCorrectly rawValue expectedFormattedValue = do
      let formattedValue = formatEnvVarValue rawValue
      formattedValue `shouldBe` expectedFormattedValue
