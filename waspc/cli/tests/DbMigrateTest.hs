module DbMigrateTest where

import qualified Options.Applicative as Opt
import Test.Hspec
import Wasp.Cli.Command.Db.Migrate (migrateArgsParser)
import Wasp.Generator.DbGenerator.Common (MigrateArgs (..), defaultMigrateArgs)

runParser :: [String] -> Either String MigrateArgs
runParser args = case Opt.execParserPure Opt.defaultPrefs parserInfo args of
  Opt.Success result -> Right result
  Opt.Failure failure -> Left $ show failure
  Opt.CompletionInvoked _ -> Left "completion invoked"
  where
    parserInfo = Opt.info migrateArgsParser Opt.fullDesc

spec_parseMigrateArgs :: Spec
spec_parseMigrateArgs =
  it "parses migrate-dev options" $ do
    runParser [] `shouldBe` Right defaultMigrateArgs
    runParser ["--create-only"]
      `shouldBe` Right (MigrateArgs {_migrationName = Nothing, _isCreateOnlyMigration = True})
    runParser ["--name", "something"]
      `shouldBe` Right (MigrateArgs {_migrationName = Just "something", _isCreateOnlyMigration = False})
    runParser ["--name", "something else longer"]
      `shouldBe` Right (MigrateArgs {_migrationName = Just "something else longer", _isCreateOnlyMigration = False})
    runParser ["--name", "something", "--create-only"]
      `shouldBe` Right (MigrateArgs {_migrationName = Just "something", _isCreateOnlyMigration = True})
    runParser ["--create-only", "--name", "something"]
      `shouldBe` Right (MigrateArgs {_migrationName = Just "something", _isCreateOnlyMigration = True})
    case runParser ["--create-only", "--wtf"] of
      Left _ -> pure ()
      Right _ -> expectationFailure "expected parse failure on unknown flag"
