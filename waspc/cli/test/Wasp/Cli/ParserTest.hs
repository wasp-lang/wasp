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

spec_newCommandTests :: Spec
spec_newCommandTests =
  describe "new commands" $ do
    it "new" $ parseMaybe "new" `shouldBe` Just (New (NewArgs {naProjectName = Nothing, naTemplateName = Nothing}))
    it "new --template saas" $ parseMaybe "new --template saas" `shouldBe` Just (New (NewArgs {naProjectName = Nothing, naTemplateName = Just "saas"}))
    it "new meoM83" $ parseMaybe "new meoM83" `shouldBe` Just (New (NewArgs {naProjectName = Just "meoM83", naTemplateName = Nothing}))
    it "new Awn28WW2 --template todo-ts" $ parseMaybe "new Awn28WW2 --template todo-ts" `shouldBe` Just (New (NewArgs {naProjectName = Just "Awn28WW2", naTemplateName = Just "todo-ts"}))

spec_versionCommandTests :: Spec
spec_versionCommandTests =
  describe "version commands" $ do
    it "version" $ parseMaybe "version" `shouldBe` Just Version
    it "version -z" $ parseMaybe "version -z" `shouldBe` Nothing
    it "version awsaws -z" $ parseMaybe "version awsaws -z" `shouldBe` Nothing

spec_wasplsCommandTests :: Spec
spec_wasplsCommandTests =
  describe "waspls commands" $ do
    it "waspls" $ parseMaybe "waspls" `shouldBe` Just (WaspLS (WaspLSArgs {wslLogFile = Nothing, wslUseStudio = False}))
    it "waspls --stdio" $ parseMaybe "waspls --stdio" `shouldBe` Just (WaspLS (WaspLSArgs {wslLogFile = Nothing, wslUseStudio = True}))
    it "waspls --log ap2le.log" $ parseMaybe "waspls --log ap2le.log" `shouldBe` Just (WaspLS (WaspLSArgs {wslLogFile = Just "ap2le.log", wslUseStudio = False}))
    it "waspls --stdio --log zero2.txt" $ parseMaybe "waspls --stdio --log zero2.txt" `shouldBe` Just (WaspLS (WaspLSArgs {wslLogFile = Just "zero2.txt", wslUseStudio = True}))
    it "waspls --stdio --log" $ parseMaybe "waspls --stdio --log" `shouldBe` Nothing

spec_uninstallCommandTests :: Spec
spec_uninstallCommandTests =
  describe "uninstall commands" $ do
    it "uninstall" $ parseMaybe "uninstall" `shouldBe` Just Uninstall

spec_startCommandTests :: Spec
spec_startCommandTests =
  describe "start commands" $ do
    it "start" $ parseMaybe "start" `shouldBe` Just (Start StartNormal)
    it "start db" $ parseMaybe "start db" `shouldBe` Just (Start StartDb)
    it "start randomcommand" $ parseMaybe "start randomcommand" `shouldBe` Nothing

spec_dbCommandTests :: Spec
spec_dbCommandTests =
  describe "db commands" $ do
    it "db start" $ parseMaybe "db start" `shouldBe` Just (Db DbStart)
    it "db" $ parseMaybe "db" `shouldBe` Nothing
    it "db migrate-dev" $ parseMaybe "db migrate-dev" `shouldBe` Just (Db (DbMigrateDev (DbMigrateDevArgs {dbmdaName = Nothing, dbmdaCreateOnly = False})))
    it "db migrate-dev --name dev-db4" $ parseMaybe "db migrate-dev --name dev-db4" `shouldBe` Just (Db (DbMigrateDev (DbMigrateDevArgs {dbmdaName = Just "dev-db4", dbmdaCreateOnly = False})))
    it "db migrate-dev --create-only" $ parseMaybe "db migrate-dev --create-only" `shouldBe` Just (Db (DbMigrateDev (DbMigrateDevArgs {dbmdaName = Nothing, dbmdaCreateOnly = True})))
    it "db migrate-dev --create-only --name \"dev9-2\"" $ parseMaybe "db migrate-dev --create-only --name \"dev9-2\"" `shouldBe` Just (Db (DbMigrateDev (DbMigrateDevArgs {dbmdaName = Just "\"dev9-2\"", dbmdaCreateOnly = True})))
    it "db migrate-dev --name" $ parseMaybe "db migrate-dev --name" `shouldBe` Nothing
    it "db reset" $ parseMaybe "db reset" `shouldBe` Just (Db DbReset)
    it "db seed" $ parseMaybe "db seed" `shouldBe` Just (Db (DbSeed Nothing))
    it "db seed apple" $ parseMaybe "db seed apple" `shouldBe` Just (Db (DbSeed (Just "apple")))
    it "db studio" $ parseMaybe "db studio" `shouldBe` Just (Db DbStudio)

spec_compileCommandTests :: Spec
spec_compileCommandTests =
  describe "compile commands" $ do
    it "compile" $ parseMaybe "compile" `shouldBe` Just Compile

spec_cleanCommandTests :: Spec
spec_cleanCommandTests =
  describe "clean commands" $ do
    it "clean" $ parseMaybe "clean" `shouldBe` Just Clean

spec_buildCommandTests :: Spec
spec_buildCommandTests =
  describe "build commands" $ do
    it "build" $ parseMaybe "build" `shouldBe` Just Build

spec_deployCommandTests :: Spec
spec_deployCommandTests =
  describe "deploy commands" $ do
    it "deploy" $ parseMaybe "deploy" `shouldBe` Nothing
    it "deploy --ui" $ parseMaybe "deploy --ui" `shouldBe` Nothing
    it "deploy fly --ui" $ parseMaybe "deploy fly --ui" `shouldBe` Just (Deploy ["fly", "--ui"])
    let cmd = "deploy fly launch my-wasp-app mia --server-secret GOOGLE_CLIENT_ID=\"EXAMPLE@ID\" --server-secret GOOGLE_CLIENT_SECRET=EXAMPLE_SECRET"
    it cmd $ parseMaybe cmd `shouldBe` Just (Deploy ["fly", "launch", "my-wasp-app", "mia", "--server-secret", "GOOGLE_CLIENT_ID=\"EXAMPLE@ID\"", "--server-secret", "GOOGLE_CLIENT_SECRET=EXAMPLE_SECRET"])

spec_telemetryCommandTests :: Spec
spec_telemetryCommandTests =
  describe "telemetry commands" $ do
    it "telemetry" $ parseMaybe "telemetry" `shouldBe` Just Telemetry

spec_depsCommandTests :: Spec
spec_depsCommandTests =
  describe "deps commands" $ do
    it "deps" $ parseMaybe "deps" `shouldBe` Just Deps

spec_dockerfileCommandTests :: Spec
spec_dockerfileCommandTests =
  describe "dockerfile commands" $ do
    it "dockerfile" $ parseMaybe "dockerfile" `shouldBe` Just Dockerfile

spec_infoCommandTests :: Spec
spec_infoCommandTests =
  describe "info tests" $ do
    it "info" $ parseMaybe "info" `shouldBe` Just Info

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
