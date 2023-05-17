module Wasp.Cli.ParserTest where

-- NOTE: This is temporary location for this test file.

import qualified Options.Applicative as O
import Test.Tasty.Hspec
import Wasp.Cli.Command.Call
import Wasp.Cli.Parser (parserRunnerSettings)

parseMaybe :: String -> Maybe Call
parseMaybe args = O.getParseResult $ uncurry O.execParserPure parserRunnerSettings $ words args

-- NOTE: This is pure parser tests. It doesn't take account of actual runtime failure or thrown exceptions.

-- We treat call of help command as Nothing in here, so no need to test them.
spec_versionCommandTests :: Spec
spec_versionCommandTests =
  describe "version commands" $ do
    it "version" $ parseMaybe "version" `shouldBe` Just Version
    it "version -z" $ parseMaybe "version -z" `shouldBe` Nothing
    it "version awsaws -z" $ parseMaybe "version awsaws -z" `shouldBe` Nothing

spec_testCommandTests :: Spec
spec_testCommandTests =
  describe "test commands" $ do
    it "test client" $
      parseMaybe "test client" `shouldBe` Just (Test $ TestClient [])
    it "test" $
      parseMaybe "test" `shouldBe` Nothing
    it "test client apple" $
      parseMaybe "test client apple"
        `shouldBe` Just (Test (TestClient ["apple"]))
    it "test client -ui" $ parseMaybe "test client -ui" `shouldBe` Just (Test (TestClient ["-ui"]))
    it "test client --ui banana apple zero33 49 -q" $
      parseMaybe "test client --ui banana apple zero33 49 -q"
        `shouldBe` Just (Test (TestClient ["--ui", "banana", "apple", "zero33", "49", "-q"]))
    it "test server --zero bin 493a a83" $
      parseMaybe "test server --zero bin 493a a83" `shouldBe` Just (Test (TestServer ["--zero", "bin", "493a", "a83"]))
    it "test server -a9 \"bzn\" 493a a83 442" $
      parseMaybe "test server -a9 \"bzn\" 493a a83 442" `shouldBe` Just (Test (TestServer ["-a9", "\"bzn\"", "493a", "a83", "442"]))
