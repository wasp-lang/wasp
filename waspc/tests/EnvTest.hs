module EnvTest where

import Data.Maybe (fromJust)
import qualified Data.Set as Set
import qualified Data.Text as T
import NeatInterpolation (trimming)
import qualified StrongPath as SP
import System.IO.Temp (withSystemTempDirectory)
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

    it "should quote values containing # or newlines" $ do
      envVarValueShouldBeFormattedCorrectly "foo#bar" "\"foo#bar\""
      envVarValueShouldBeFormattedCorrectly "line1\nline2\rline3" "\"line1\\nline2\\rline3\""
      envVarValueShouldBeFormattedCorrectly "'quoted'" "\"'quoted'\""

    it "should preserve literal escapes when U+2028 or U+2029 appear with interior quotes" $ do
      envVarValueShouldBeFormattedCorrectly "a' `\\nb" "'a' `\\nb'"
      envVarValueShouldBeFormattedCorrectly "a' `\x2028\\nb" "'a' `\x2028\\nb'"
      envVarValueShouldBeFormattedCorrectly "a' `\x2029\\nb" "'a' `\x2029\\nb'"
      envVarValueShouldBeFormattedCorrectly "a'\x2028`\\nb" "`a'\x2028`\\nb`"
      envVarValueShouldBeFormattedCorrectly "a'\x2029`\\nb" "`a'\x2029`\\nb`"
      envVarValueShouldBeFormattedCorrectly "'a'\x2028`\"b" "''a'\x2028`\"b'"
      envVarValueShouldBeFormattedCorrectly "'a'\x2029`\"b" "''a'\x2029`\"b'"

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

  describe "addEnvVarsOverride"
    $ it "should let incoming env vars override the existing ones"
    $ addEnvVarsOverride
      [("B", "3"), ("C", "4")]
      [("A", "1"), ("B", "2")]
      `shouldBe` [("B", "3"), ("C", "4"), ("A", "1")]

  describe "addEnvVarsUnique" $ do
    it "should add the incoming env vars when there are no duplicates" $
      addEnvVarsUnique
        [("B", "2")]
        [("A", "1")]
        `shouldBe` Right [("B", "2"), ("A", "1")]

    it "should return the duplicate env var names when there are duplicates" $
      addEnvVarsUnique
        [("B", "3"), ("A", "4")]
        [("A", "1"), ("B", "2")]
        `shouldBe` Left (Set.fromList ["A", "B"])
spec_parseDotEnvContent :: Spec
spec_parseDotEnvContent = do
  describe "parseDotEnvFile" $ do
    it "lets the last definition win when the same variable is defined twice" $ do
      parseDotEnv "SOME_VAR=1\nSOME_VAR=2"
        `shouldReturn` [("SOME_VAR", "2")]

    it "keeps ${VAR} literally without expanding from earlier lines" $ do
      parseDotEnv "A=hello\nB=${A}_world"
        `shouldReturn` [("A", "hello"), ("B", "${A}_world")]

    it "keeps $(command) literally without executing it" $ do
      parseDotEnv "CMD=$(echo hi)"
        `shouldReturn` [("CMD", "$(echo hi)")]

    it "parses unquoted values containing spaces" $ do
      parseDotEnv "KEY=some spaced value"
        `shouldReturn` [("KEY", "some spaced value")]

    it "parses backtick-quoted values" $ do
      parseDotEnv "KEY=`value`"
        `shouldReturn` [("KEY", "value")]

    it "skips lines that are not definitions instead of failing the file" $ do
      parseDotEnv "not a definition\nVALID=1\nalso invalid"
        `shouldReturn` [("VALID", "1")]

    it "keeps escaped quotes as \\\" inside quotes" $ do
      parseDotEnv "KEY=\"a\\\"b\""
        `shouldReturn` [("KEY", "a\\\"b")]
      parseDotEnv "KEY='a\\\"b'"
        `shouldReturn` [("KEY", "a\\\"b")]

    it "expands \\n and \\r inside double quotes but not single quotes" $ do
      parseDotEnv "DOUBLE=\"line1\\nline2\"\nSINGLE='line1\\nline2'"
        `shouldReturn` [("DOUBLE", "line1\nline2"), ("SINGLE", "line1\\nline2")]

    it "round-trips formatted env vars through envVarsToDotEnvContent and parseDotEnvFile" $ do
      let envVars =
            [ ("SIMPLE", "value"),
              ("SPACED", "some spaced value"),
              ("HASH", "foo#bar"),
              ("NEWLINES", "line1\nline2\rline3"),
              ("SINGLE_QUOTED_INNER", "'quoted'"),
              ("LINE_SEP_SINGLE_QUOTE_SAFE", "a' `\x2028\\nb"),
              ("PARA_SEP_SINGLE_QUOTE_SAFE", "a' `\x2029\\nb"),
              ("LINE_SEP_APOSTROPHE", "a'\x2028`\\nb"),
              ("PARA_SEP_APOSTROPHE", "a'\x2029`\\nb"),
              ("LINE_SEP_SINGLE_LINE_CONFLICT", "'a'\x2028`\"b"),
              ("PARA_SEP_SINGLE_LINE_CONFLICT", "'a'\x2029`\"b")
            ]
      parseDotEnv (T.unpack $ envVarsToDotEnvContent envVars)
        `shouldReturn` envVars
  where
    parseDotEnv :: String -> IO [EnvVar]
    parseDotEnv content =
      withSystemTempDirectory "wasp-env-test" $ \tmpDir -> do
        let envFile = fromJust (SP.parseAbsDir tmpDir) SP.</> [SP.relfile|.env|]
        writeFile (SP.fromAbsFile envFile) content
        parseDotEnvFile envFile
