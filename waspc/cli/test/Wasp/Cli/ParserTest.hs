module Wasp.Cli.ParserTest where

import qualified Options.Applicative as O
import Test.Tasty.Hspec (Spec, SpecWith, describe, it, shouldBe)
import Wasp.Cli.Command.Call
import Wasp.Cli.Command.ShellCompletion.Shell (Shell (..))
import Wasp.Cli.Parser (parserRunnerSettings)

parseArgs :: String -> Maybe CommandCall
parseArgs args = O.getParseResult $ uncurry O.execParserPure parserRunnerSettings $ words args

runTest :: String -> String -> CommandCall -> SpecWith ()
runTest cliArgs description expectedResult =
  it description $ parseArgs cliArgs `shouldBe` Just expectedResult

runTestWithoutCommandArgs :: String -> CommandCall -> SpecWith ()
runTestWithoutCommandArgs cliArgs expectedResult = runTest cliArgs "no args" expectedResult

spec_cliArgsParser :: Spec
spec_cliArgsParser =
  describe "top level commands that takes no arguments at all, which should pass" $ do
    (\(cmd, expectedResult) -> runTest cmd cmd expectedResult)
      `mapM_` [ ("version", Version),
                ("uninstall", Uninstall),
                ("compile", Compile),
                ("clean", Clean),
                ("build", Build),
                ("telemetry", Telemetry),
                ("deps", Deps),
                ("dockerfile", Dockerfile),
                ("info", Info)
              ]

    describe "new should correctly pass with" $ do
      runTestWithoutCommandArgs "new" $
        New (NewProjectArgs {newProjectName = Nothing, newTemplateName = Nothing})
      runTest "new Awn28WW2 --template todo-ts" "both projectName and --template arg" $
        New (NewProjectArgs {newProjectName = Just "Awn28WW2", newTemplateName = Just "todo-ts"})

    describe "waspls should correctly pass with" $ do
      runTestWithoutCommandArgs "waspls" $ WaspLS (WaspLSArgs {wlsLogFile = Nothing, wlsUseStudio = False})
      runTest "waspls --stdio --log zero2.txt" "both --stdio and --log arg" $
        WaspLS (WaspLSArgs {wlsLogFile = Just "zero2.txt", wlsUseStudio = True})

    describe "completion should correctly pass with" $ do
      runTestWithoutCommandArgs "completion" $ Completion PrintInstruction
      runTest "completion generate bash" "generate bash" $ Completion (PrintScript Bash)
      runTest "completion generate zsh" "generate zsh" $ Completion (PrintScript Zsh)
      runTest "completion generate fish" "generate fish" $ Completion (PrintScript Fish)

    describe "start should correctly pass with" $ do
      runTestWithoutCommandArgs "start" $ Start StartApp
      runTest "start db" "db arg" $ Start StartDb

    describe "db should correctly pass with" $ do
      runTest "db start" "start" $ Db DbStart
      runTest "db migrate-dev" "migrate-dev without args" $
        Db (DbMigrateDev (DbMigrateDevArgs {dbMigrateName = Nothing, dbMigrateCreateOnly = False}))
      runTest "db migrate-dev --create-only --name \"dev9-2\"" "migrate-dev with both --create-only and --name arg" $
        Db (DbMigrateDev (DbMigrateDevArgs {dbMigrateName = Just "\"dev9-2\"", dbMigrateCreateOnly = True}))
      runTest "db reset" "reset" $ Db DbReset
      runTest "db seed" "seed without arg" $ Db (DbSeed Nothing)
      runTest "db seed apple" "seed arg" $ Db (DbSeed (Just "apple"))
      runTest "db studio" "studio" $ Db DbStudio

    describe "deploy should correctly pass with" $ do
      runTestWithoutCommandArgs "deploy" $ Deploy []
      let cliArgs = "deploy fly launch my-wasp-app mia --server-secret GOOGLE_CLIENT_ID=\"EXAMPLE@ID\" --server-secret GOOGLE_CLIENT_SECRET=EXAMPLE_SECRET"
      runTest cliArgs "with args" $
        Deploy ["fly", "launch", "my-wasp-app", "mia", "--server-secret", "GOOGLE_CLIENT_ID=\"EXAMPLE@ID\"", "--server-secret", "GOOGLE_CLIENT_SECRET=EXAMPLE_SECRET"]

    describe "test should correctly pass with" $ do
      runTest "test client" "client without args" $ Test $ TestClient []
      runTest "test client --ui banana apple zero33 49 -q" "client with args" $
        Test (TestClient ["--ui", "banana", "apple", "zero33", "49", "-q"])
