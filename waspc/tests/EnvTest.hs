module EnvTest where

import qualified Data.Set as Set
import qualified Data.Text as T
import NeatInterpolation (trimming)
import Test.Hspec
import Wasp.Env

spec_envVarHelpers :: Spec
spec_envVarHelpers = do
  describe "formatEnvVarValue" $ do
    it "should handle simple string values correctly" $ envVarValueShouldBeFormattedCorrectly "simple_value" "simple_value"

    it "should handle JSON values correctly" $ envVarValueShouldBeFormattedCorrectly "{\"teamConcurrency\":3}" "{\"teamConcurrency\":3}"

    it "should handle nested JSON objects correctly" $
      envVarValueShouldBeFormattedCorrectly
        "{\"database\":{\"host\":\"localhost\",\"port\":5432},\"options\":[\"option1\",\"option2\"]}"
        "{\"database\":{\"host\":\"localhost\",\"port\":5432},\"options\":[\"option1\",\"option2\"]}"

    it "should handle boolean and number values in JSON correctly" $
      envVarValueShouldBeFormattedCorrectly
        "{\"enabled\":true,\"maxRetries\":5,\"timeout\":30.5}"
        "{\"enabled\":true,\"maxRetries\":5,\"timeout\":30.5}"

    it "should handle values that need quoting in shell environments" $ do
      envVarValueShouldBeFormattedCorrectly "hello world with spaces" "\"hello world with spaces\""
      envVarValueShouldBeFormattedCorrectly "{\"teamConcurrency\":3, \"retryLimit\":2}" "\"{\"teamConcurrency\":3, \"retryLimit\":2}\""

    it "should handle special characters correctly" $ envVarValueShouldBeFormattedCorrectly "value$with&special=chars" "value$with&special=chars"

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

    it "should handle empty values correctly" $ envVarsToDotEnvContent [("EMPTY_VAR", "")] `shouldBe` T.pack "EMPTY_VAR="
  where
    envVarValueShouldBeFormattedCorrectly :: String -> String -> Expectation
    envVarValueShouldBeFormattedCorrectly rawValue expectedFormattedValue = do
      let formattedValue = formatEnvVarValue rawValue
      formattedValue `shouldBe` expectedFormattedValue

spec_envVarCombining :: Spec
spec_envVarCombining = do
  describe "nubEnvVars" $ do
    it "should keep the first occurence of each env var name" $
      nubEnvVars [("A", "1"), ("B", "2"), ("A", "3")]
        `shouldBe` [("A", "1"), ("B", "2")]

    it "should leave a list without duplicates unchanged" $
      nubEnvVars [("A", "1"), ("B", "2")]
        `shouldBe` [("A", "1"), ("B", "2")]

  describe "findDuplicateEnvVars" $ do
    it "should return names present in both lists" $
      findDuplicateEnvVars [("A", "1"), ("B", "2")] [("B", "3"), ("C", "4"), ("A", "5")]
        `shouldBe` Set.fromList ["A", "B"]

    it "should return an empty list when there is no overlap" $
      findDuplicateEnvVars [("A", "1")] [("B", "2")]
        `shouldBe` Set.fromList []

  describe "addEnvVarsOverride" $
    it "should let incoming env vars override the existing ones" $
      addEnvVarsOverride (EnvVarsHolder [("A", "1"), ("B", "2")]) [("B", "3"), ("C", "4")]
        `shouldBe` EnvVarsHolder [("B", "3"), ("C", "4"), ("A", "1")]

  describe "addEnvVarsUnique" $ do
    it "should add the incoming env vars when there are no duplicates" $
      addEnvVarsUnique (EnvVarsHolder [("A", "1")]) [("B", "2")]
        `shouldBe` Right (EnvVarsHolder [("B", "2"), ("A", "1")])

    it "should return the duplicate env var names when there are duplicates" $
      addEnvVarsUnique (EnvVarsHolder [("A", "1"), ("B", "2")]) [("B", "3"), ("A", "4")]
        `shouldBe` Left (Set.fromList ["A", "B"])

newtype EnvVarsHolder = EnvVarsHolder [EnvVar] deriving (Show, Eq, Ord)

instance HasEnvVars EnvVarsHolder where
  getEnvVars (EnvVarsHolder envVars) = envVars
  setEnvVars _ = EnvVarsHolder
