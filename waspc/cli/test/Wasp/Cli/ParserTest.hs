module Wasp.Cli.ParserTest where

import qualified Options.Applicative as O
import Test.Tasty.Hspec (Spec, SpecWith, describe, it, shouldBe)
import Wasp.Cli.Command.Call
import Wasp.Cli.Command.ShellCompletion.CompletionArgs
import Wasp.Cli.Parser (parserRunnerSettings)

(~>) :: String -> CommandCall -> SpecWith ()
(~>) = testParsingCliArgs

testParsingCliArgs :: String -> CommandCall -> SpecWith ()
testParsingCliArgs cliArgs expectedCmdCall =
  it ("`" <> cliArgs <> "`") $ parseArgs cliArgs `shouldBe` Just expectedCmdCall

parseArgs :: String -> Maybe CommandCall
parseArgs args = O.getParseResult $ uncurry O.execParserPure parserRunnerSettings $ words args

spec_cliArgsParser :: Spec
spec_cliArgsParser =
  describe "CLI command calls should parse correctly, returning correct Command.Call" $ do
    describe "Top level commands that take no arguments" $ do
      "version" ~> Version
      "uninstall" ~> Uninstall
      "compile" ~> Compile
      "clean" ~> Clean
      "build" ~> Build
      "telemetry" ~> Telemetry
      "deps" ~> Deps
      "dockerfile" ~> Dockerfile
      "info" ~> Info

    describe "`new` command" $ do
      "new"
        ~> New (NewProjectArgs {newProjectName = Nothing, newTemplateName = Nothing})
      "new MyTodoApp --template todo-ts"
        ~> New (NewProjectArgs {newProjectName = Just "MyTodoApp", newTemplateName = Just "todo-ts"})

    describe "`waspls` command" $ do
      "waspls"
        ~> WaspLS (WaspLSArgs {wlsLogFile = Nothing, wlsUseStudio = False})
      "waspls --stdio --log myLogFile.txt"
        ~> WaspLS (WaspLSArgs {wlsLogFile = Just "myLogFile.txt", wlsUseStudio = True})

    describe "`completion` command" $ do
      "completion" ~> Completion PrintInstruction
      "completion generate bash" ~> Completion (PrintScript Bash)
      "completion generate zsh" ~> Completion (PrintScript Zsh)
      "completion generate fish" ~> Completion (PrintScript Fish)

    describe "`start` command" $ do
      "start" ~> Start StartApp
      "start db" ~> Start StartDb

    describe "`db` command" $ do
      "db start" ~> Db DbStart
      describe "`db migrate-dev` command" $ do
        "db migrate-dev"
          ~> Db (DbMigrateDev (DbMigrateDevArgs {dbMigrateName = Nothing, dbMigrateCreateOnly = False}))
        "db migrate-dev --create-only --name \"myMigrationName\""
          ~> Db (DbMigrateDev (DbMigrateDevArgs {dbMigrateName = Just "\"myMigrationName\"", dbMigrateCreateOnly = True}))
      "db reset" ~> Db DbReset
      "db seed" ~> Db (DbSeed Nothing)
      "db seed apple" ~> Db (DbSeed (Just "apple"))
      "db studio" ~> Db DbStudio

    describe "`deploy` command" $ do
      "deploy" ~> Deploy []
      "deploy fly launch my-wasp-app mia --server-secret GOOGLE_CLIENT_ID=\"EXAMPLE@ID\" --server-secret GOOGLE_CLIENT_SECRET=EXAMPLE_SECRET"
        ~> Deploy ["fly", "launch", "my-wasp-app", "mia", "--server-secret", "GOOGLE_CLIENT_ID=\"EXAMPLE@ID\"", "--server-secret", "GOOGLE_CLIENT_SECRET=EXAMPLE_SECRET"]

    describe "`test` command" $ do
      describe "`test client` command" $ do
        "test client" ~> Test (TestClient [])
        "test client --ui run someRandomArgs nextArgs moreRandomArgs restOfArgs"
          ~> Test (TestClient ["--ui", "run", "someRandomArgs", "nextArgs", "moreRandomArgs", "restOfArgs"])