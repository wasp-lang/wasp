module EnvTest where

import qualified Data.Text as T
import Test.Tasty.Hspec
import Wasp.Env (envVarsToDotEnvContent)

spec_envVarsToDotEnvContent :: Spec
spec_envVarsToDotEnvContent = do
  describe "envVarsToDotEnvContent" $ do
    it "should handle simple string values correctly" $ do
      let envVars = [("SIMPLE_VAR", "simple_value")]
      let result = envVarsToDotEnvContent envVars
      -- Simple strings should be output without extra quotes
      result `shouldBe` T.pack "SIMPLE_VAR=simple_value"

    it "should handle JSON values correctly (this currently fails due to the bug)" $ do
      let envVars = [("PG_BOSS_NEW_OPTIONS", "{\"teamConcurrency\":3}")]
      let result = envVarsToDotEnvContent envVars
      -- JSON should be output as-is without escaping or extra quotes
      -- This is what we WANT to happen, but currently fails due to the bug
      result `shouldBe` T.pack "PG_BOSS_NEW_OPTIONS={\"teamConcurrency\":3}"

    it "should handle multiple environment variables correctly" $ do
      let envVars =
            [ ("DATABASE_URL", "postgresql://localhost:5432/mydb"),
              ("PG_BOSS_NEW_OPTIONS", "{\"teamConcurrency\":3,\"retryLimit\":2}"),
              ("SIMPLE_VAR", "value")
            ]
      let result = envVarsToDotEnvContent envVars
      let expected =
            T.pack $
              unlines
                [ "DATABASE_URL=postgresql://localhost:5432/mydb",
                  "PG_BOSS_NEW_OPTIONS={\"teamConcurrency\":3,\"retryLimit\":2}",
                  "SIMPLE_VAR=value"
                ]
      -- Remove the trailing newline from expected since intercalate doesn't add one at the end
      result `shouldBe` T.take (T.length expected - 1) expected

    it "should handle nested JSON objects correctly" $ do
      let envVars = [("COMPLEX_JSON", "{\"database\":{\"host\":\"localhost\",\"port\":5432},\"options\":[\"option1\",\"option2\"]}")]
      let result = envVarsToDotEnvContent envVars
      -- Complex JSON should remain readable and parseable
      result `shouldBe` T.pack "COMPLEX_JSON={\"database\":{\"host\":\"localhost\",\"port\":5432},\"options\":[\"option1\",\"option2\"]}"

    it "should handle boolean and number values in JSON correctly" $ do
      let envVars = [("CONFIG_JSON", "{\"enabled\":true,\"maxRetries\":5,\"timeout\":30.5}")]
      let result = envVarsToDotEnvContent envVars
      -- Boolean and number values should not be escaped
      result `shouldBe` T.pack "CONFIG_JSON={\"enabled\":true,\"maxRetries\":5,\"timeout\":30.5}"

    it "should handle values that need quoting in shell environments" $ do
      let envVars = [("VALUE_WITH_SPACES", "hello world with spaces")]
      let result = envVarsToDotEnvContent envVars
      -- Values with spaces should be quoted for shell compatibility
      result `shouldBe` T.pack "VALUE_WITH_SPACES=\"hello world with spaces\""

    it "should handle empty values correctly" $ do
      let envVars = [("EMPTY_VAR", "")]
      let result = envVarsToDotEnvContent envVars
      -- Empty values should be handled without issues
      result `shouldBe` T.pack "EMPTY_VAR="

    it "should handle special characters correctly" $ do
      let envVars = [("SPECIAL_CHARS", "value$with&special=chars")]
      let result = envVarsToDotEnvContent envVars
      -- Special characters should be preserved as-is
      result `shouldBe` T.pack "SPECIAL_CHARS=value$with&special=chars"
