module DbMigrateTest where

import Data.Either (isLeft)
import qualified Options.Applicative as Opt
import Test.Hspec
import Wasp.Cli.Command.Db.Migrate (migrateArgsParser)
import Wasp.Generator.DbGenerator.Common (MigrateArgs (..), defaultMigrateArgs)

spec_migrateArgsParser :: Spec
spec_migrateArgsParser =
  it "should parse input options strings correctly" $ do
    parseMigrationOptions [] `shouldBe` Right defaultMigrateArgs
    parseMigrationOptions ["--create-only"]
      `shouldBe` Right (MigrateArgs {_migrationName = Nothing, _isCreateOnlyMigration = True})
    parseMigrationOptions ["--name", "something"]
      `shouldBe` Right (MigrateArgs {_migrationName = Just "something", _isCreateOnlyMigration = False})
    parseMigrationOptions ["--name", "something else longer"]
      `shouldBe` Right (MigrateArgs {_migrationName = Just "something else longer", _isCreateOnlyMigration = False})
    parseMigrationOptions ["--name", "something", "--create-only"]
      `shouldBe` Right (MigrateArgs {_migrationName = Just "something", _isCreateOnlyMigration = True})
    parseMigrationOptions ["--create-only", "--name", "something"]
      `shouldBe` Right (MigrateArgs {_migrationName = Just "something", _isCreateOnlyMigration = True})
    isLeft (parseMigrationOptions ["--create-only", "--wtf"]) `shouldBe` True

parseMigrationOptions :: [String] -> Either String MigrateArgs
parseMigrationOptions = parse migrateArgsParser

parse :: Opt.Parser a -> [String] -> Either String a
parse parser args =
  case Opt.execParserPure Opt.defaultPrefs (Opt.info parser Opt.fullDesc) args of
    Opt.Success value -> Right value
    Opt.Failure failure -> Left $ fst $ Opt.renderFailure failure "wasp db migrate-dev"
    Opt.CompletionInvoked _ -> Left "Unexpected completion"
