module Wasp.Cli.Command.Start.ArgumentsParserTest where

import qualified Options.Applicative as Opt
import Test.Hspec
import Wasp.Cli.Command.Start.ArgumentsParser (StartArgs (..), startArgsParser)
import Wasp.Cli.Util.PortArgument (defaultAppPorts)
import Wasp.Project.Apps (Apps (Apps))

spec_startArgsParser :: Spec
spec_startArgsParser = do
  describe "startArgsParser" $ do
    describe "valid arguments" $ do
      it "uses the default ports when no arguments are given" $ do
        parse [] `shouldBe` Just (StartArgs defaultAppPorts)

      it "parses --client-port" $ do
        parse ["--client-port", "4000"] `shouldBe` Just (StartArgs $ Apps 4000 3001)

      it "parses --server-port" $ do
        parse ["--server-port", "4001"] `shouldBe` Just (StartArgs $ Apps 3000 4001)

      it "parses both ports" $ do
        parse ["--client-port", "4000", "--server-port", "4001"]
          `shouldBe` Just (StartArgs $ Apps 4000 4001)

      it "accepts the port number bounds" $ do
        parse ["--client-port", "1", "--server-port", "65535"]
          `shouldBe` Just (StartArgs $ Apps 1 65535)

    describe "invalid arguments" $ do
      it "rejects a non-numeric port" $ do
        parse ["--client-port", "abc"] `shouldBe` Nothing

      -- Port 0 means "let the OS pick a port", which we can't use because we have to
      -- tell each side where the other one is running.
      it "rejects port 0" $ do
        parse ["--client-port", "0"] `shouldBe` Nothing

      it "rejects a port above the maximum port number" $ do
        parse ["--server-port", "65536"] `shouldBe` Nothing

      it "rejects a port missing its value" $ do
        parse ["--client-port"] `shouldBe` Nothing

      it "rejects unknown options" $ do
        parse ["--db-port", "5432"] `shouldBe` Nothing

parse :: [String] -> Maybe StartArgs
parse args = case Opt.execParserPure Opt.defaultPrefs parserInfo args of
  Opt.Success parsedArgs -> Just parsedArgs
  _parsingFailed -> Nothing
  where
    parserInfo = Opt.info startArgsParser Opt.fullDesc
