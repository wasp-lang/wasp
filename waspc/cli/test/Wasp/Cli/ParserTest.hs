module Wasp.Cli.ParserTest where

import qualified Options.Applicative as O
import Test.Tasty.Hspec
import Text.Printf (printf)
import Wasp.Cli.Command.Call
import Wasp.Cli.Parser (parserRunnerSettings)

testParse :: String -> Maybe Call
testParse args = O.getParseResult $ uncurry O.execParserPure parserRunnerSettings $ words args

-- NOTE: This is pure parser tests. It doesn't take account of actual runtime failure or thrown exceptions.

-- We treat call of help command as Nothing in here, so no need to test them.

spec_newCommandTests :: Spec
spec_newCommandTests =
  describe "new commands" $ do
    it "`new` should pass" $ testParse "new" `shouldBe` Just (New (NewArgs {naProjectName = Nothing, naTemplateName = Nothing}))
    it "`new --template saas` should pass" $ testParse "new --template saas" `shouldBe` Just (New (NewArgs {naProjectName = Nothing, naTemplateName = Just "saas"}))
    it "`new meoM83` should pass" $ testParse "new meoM83" `shouldBe` Just (New (NewArgs {naProjectName = Just "meoM83", naTemplateName = Nothing}))
    it "`new Awn28WW2 --template todo-ts` should pass" $ testParse "new Awn28WW2 --template todo-ts" `shouldBe` Just (New (NewArgs {naProjectName = Just "Awn28WW2", naTemplateName = Just "todo-ts"}))

spec_versionCommandTests :: Spec
spec_versionCommandTests =
  describe "version commands" $ do
    it "`version` should pass" $ testParse "version" `shouldBe` Just Version
    it "`version -z` should fail" $ testParse "version -z" `shouldBe` Nothing
    it "`version awsaws -z` should fail" $ testParse "version awsaws -z" `shouldBe` Nothing

spec_wasplsCommandTests :: Spec
spec_wasplsCommandTests =
  describe "waspls commands" $ do
    it "`waspls` should pass" $ testParse "waspls" `shouldBe` Just (WaspLS (WaspLSArgs {wslLogFile = Nothing, wslUseStudio = False}))
    it "`waspls --stdio` should pass" $ testParse "waspls --stdio" `shouldBe` Just (WaspLS (WaspLSArgs {wslLogFile = Nothing, wslUseStudio = True}))
    it "`waspls --log ap2le.log` should pass" $ testParse "waspls --log ap2le.log" `shouldBe` Just (WaspLS (WaspLSArgs {wslLogFile = Just "ap2le.log", wslUseStudio = False}))
    it "`waspls --stdio --log zero2.txt` should pass" $ testParse "waspls --stdio --log zero2.txt" `shouldBe` Just (WaspLS (WaspLSArgs {wslLogFile = Just "zero2.txt", wslUseStudio = True}))
    it "`waspls --stdio --log` should fail" $ testParse "waspls --stdio --log" `shouldBe` Nothing

spec_completionCommandTests :: Spec
spec_completionCommandTests =
  describe "completion commands" $ do
    it "`completion` should pass" $ testParse "completion" `shouldBe` Just (Completion ShowInstruction)
    it "`completion generate` should fail" $ testParse "completion generate" `shouldBe` Nothing
    it "`completion generate bash` should pass" $ testParse "completion generate bash" `shouldBe` Just (Completion (Generate Bash))
    it "`completion generate zsh` should pass" $ testParse "completion generate zsh" `shouldBe` Just (Completion (Generate Zsh))
    it "`completion generate fish` should pass" $ testParse "completion generate fish" `shouldBe` Just (Completion (Generate Fish))

spec_uninstallCommandTests :: Spec
spec_uninstallCommandTests =
  describe "uninstall commands" $ do
    it "`uninstall` should pass" $ testParse "uninstall" `shouldBe` Just Uninstall

spec_startCommandTests :: Spec
spec_startCommandTests =
  describe "start commands" $ do
    it "`start` should pass" $ testParse "start" `shouldBe` Just (Start StartNormal)
    it "`start db` should pass" $ testParse "start db" `shouldBe` Just (Start StartDb)
    it "`start randomcommand` should fail" $ testParse "start randomcommand" `shouldBe` Nothing

spec_dbCommandTests :: Spec
spec_dbCommandTests =
  describe "db commands" $ do
    it "`db start` should pass" $ testParse "db start" `shouldBe` Just (Db DbStart)
    it "`db` should fail (print help)" $ testParse "db" `shouldBe` Nothing
    it "`db migrate-dev` should pass" $ testParse "db migrate-dev" `shouldBe` Just (Db (DbMigrateDev (DbMigrateDevArgs {dbmdaName = Nothing, dbmdaCreateOnly = False})))
    it "`db migrate-dev --name dev-db4` should pass" $ testParse "db migrate-dev --name dev-db4" `shouldBe` Just (Db (DbMigrateDev (DbMigrateDevArgs {dbmdaName = Just "dev-db4", dbmdaCreateOnly = False})))
    it "`db migrate-dev --create-only` should pass" $ testParse "db migrate-dev --create-only" `shouldBe` Just (Db (DbMigrateDev (DbMigrateDevArgs {dbmdaName = Nothing, dbmdaCreateOnly = True})))
    it "`db migrate-dev --create-only --name \"dev9-2\"` should pass" $ testParse "db migrate-dev --create-only --name \"dev9-2\"" `shouldBe` Just (Db (DbMigrateDev (DbMigrateDevArgs {dbmdaName = Just "\"dev9-2\"", dbmdaCreateOnly = True})))
    it "`db migrate-dev --name` should fail" $ testParse "db migrate-dev --name" `shouldBe` Nothing
    it "`db reset` should pass" $ testParse "db reset" `shouldBe` Just (Db DbReset)
    it "`db seed` should pass" $ testParse "db seed" `shouldBe` Just (Db (DbSeed Nothing))
    it "`db seed apple` should pass" $ testParse "db seed apple" `shouldBe` Just (Db (DbSeed (Just "apple")))
    it "`db studio` should pass" $ testParse "db studio" `shouldBe` Just (Db DbStudio)

spec_compileCommandTests :: Spec
spec_compileCommandTests =
  describe "compile commands" $ do
    it "`compile` should pass" $ testParse "compile" `shouldBe` Just Compile

spec_cleanCommandTests :: Spec
spec_cleanCommandTests =
  describe "clean commands" $ do
    it "`clean` should pass" $ testParse "clean" `shouldBe` Just Clean

spec_buildCommandTests :: Spec
spec_buildCommandTests =
  describe "build commands" $ do
    it "`build` should pass" $ testParse "build" `shouldBe` Just Build

spec_deployCommandTests :: Spec
spec_deployCommandTests =
  describe "deploy commands" $ do
    it "`deploy` should pass" $ testParse "deploy" `shouldBe` Just (Deploy [])
    it "`deploy --ui` should fail" $ testParse "deploy --ui" `shouldBe` Nothing
    it "`deploy fly --ui` should pass" $ testParse "deploy fly --ui" `shouldBe` Just (Deploy ["fly", "--ui"])
    let cmd = "deploy fly launch my-wasp-app mia --server-secret GOOGLE_CLIENT_ID=\"EXAMPLE@ID\" --server-secret GOOGLE_CLIENT_SECRET=EXAMPLE_SECRET"
    let cmdMsg = printf "`%s` should pass" cmd
    it cmdMsg $ testParse cmd `shouldBe` Just (Deploy ["fly", "launch", "my-wasp-app", "mia", "--server-secret", "GOOGLE_CLIENT_ID=\"EXAMPLE@ID\"", "--server-secret", "GOOGLE_CLIENT_SECRET=EXAMPLE_SECRET"])

spec_telemetryCommandTests :: Spec
spec_telemetryCommandTests =
  describe "telemetry commands" $ do
    it "`telemetry` should pass" $ testParse "telemetry" `shouldBe` Just Telemetry

spec_depsCommandTests :: Spec
spec_depsCommandTests =
  describe "deps commands" $ do
    it "`deps` should pass" $ testParse "deps" `shouldBe` Just Deps

spec_dockerfileCommandTests :: Spec
spec_dockerfileCommandTests =
  describe "dockerfile commands" $ do
    it "`dockerfile` should pass" $ testParse "dockerfile" `shouldBe` Just Dockerfile

spec_infoCommandTests :: Spec
spec_infoCommandTests =
  describe "info tests" $ do
    it "`info` should pass" $ testParse "info" `shouldBe` Just Info

spec_testCommandTests :: Spec
spec_testCommandTests =
  describe "test commands" $ do
    it "`test client` should pass" $
      testParse "test client" `shouldBe` Just (Test $ TestClient [])
    it "`test` should fail (print help)" $
      testParse "test" `shouldBe` Nothing
    it "`test client apple` should pass" $
      testParse "test client apple"
        `shouldBe` Just (Test (TestClient ["apple"]))
    it "`test client -ui` should pass" $ testParse "test client -ui" `shouldBe` Just (Test (TestClient ["-ui"]))
    it "`test client --ui banana apple zero33 49 -q` should pass" $
      testParse "test client --ui banana apple zero33 49 -q"
        `shouldBe` Just (Test (TestClient ["--ui", "banana", "apple", "zero33", "49", "-q"]))
    it "`test server --zero bin 493a a83` should pass" $
      testParse "test server --zero bin 493a a83" `shouldBe` Just (Test (TestServer ["--zero", "bin", "493a", "a83"]))
    it "`test server -a9 \"bzn\" 493a a83 442` should pass" $
      testParse "test server -a9 \"bzn\" 493a a83 442" `shouldBe` Just (Test (TestServer ["-a9", "\"bzn\"", "493a", "a83", "442"]))
