module DbMigrateTest where

import Data.Either (isLeft)
import Test.Tasty.Hspec
import Wasp.Cli.Command.Db.Migrate (parseMigrateArgs)
import Wasp.Generator.DbGenerator.Common (MigrateArgs (..), defaultMigrateArgs)

spec_parseMigrateArgs :: Spec
spec_parseMigrateArgs =
  it "should parse input options strings correcly" $ do
    parseMigrateArgs [] `shouldBe` Right defaultMigrateArgs
    parseMigrateArgs ["--create-only"]
      `shouldBe` Right (MigrateArgs {_migrationName = Nothing, _isCreateOnlyMigration = True})
    parseMigrateArgs ["--name", "something"]
      `shouldBe` Right (MigrateArgs {_migrationName = Just "something", _isCreateOnlyMigration = False})
    parseMigrateArgs ["--name", "something else longer"]
      `shouldBe` Right (MigrateArgs {_migrationName = Just "something else longer", _isCreateOnlyMigration = False})
    parseMigrateArgs ["--name", "something", "--create-only"]
      `shouldBe` Right (MigrateArgs {_migrationName = Just "something", _isCreateOnlyMigration = True})
    parseMigrateArgs ["--create-only", "--name", "something"]
      `shouldBe` Right (MigrateArgs {_migrationName = Just "something", _isCreateOnlyMigration = True})
    isLeft (parseMigrateArgs ["--create-only", "--wtf"]) `shouldBe` True
