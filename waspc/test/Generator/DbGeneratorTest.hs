module Generator.DbGeneratorTest where

import Test.Tasty.Hspec (Spec, describe, it, shouldBe)
import Wasp.Generator.DbGenerator.Common
  ( MigrateArgs (..),
    asArgs,
    emptyMigrateArgs,
    parseMigrateArgs,
  )

spec_Common :: Spec
spec_Common =
  describe "parseMigrateArgs" $ do
    it "should parse input options strings correcly" $ do
      parseMigrateArgs Nothing `shouldBe` emptyMigrateArgs
      parseMigrateArgs (Just ["--create-only"])
        `shouldBe` (MigrateArgs {_migrationName = Nothing, _createOnlyMigration = True})
      parseMigrateArgs (Just ["--name", "something"])
        `shouldBe` (MigrateArgs {_migrationName = Just "something", _createOnlyMigration = False})
      parseMigrateArgs (Just ["--name", "something else longer"])
        `shouldBe` (MigrateArgs {_migrationName = Just "something else longer", _createOnlyMigration = False})
      parseMigrateArgs (Just ["--name", "something", "--create-only"])
        `shouldBe` (MigrateArgs {_migrationName = Just "something", _createOnlyMigration = True})
      parseMigrateArgs (Just ["--create-only", "--name", "something"])
        `shouldBe` (MigrateArgs {_migrationName = Just "something", _createOnlyMigration = True})
    it "should produce expected args" $ do
      asArgs emptyMigrateArgs `shouldBe` []
      asArgs (MigrateArgs {_migrationName = Nothing, _createOnlyMigration = True})
        `shouldBe` ["--create-only"]
      asArgs (MigrateArgs {_migrationName = Just "something", _createOnlyMigration = False})
        `shouldBe` ["--name", "something"]
      asArgs (MigrateArgs {_migrationName = Just "something else longer", _createOnlyMigration = False})
        `shouldBe` ["--name", "something else longer"]
      asArgs (MigrateArgs {_migrationName = Just "something", _createOnlyMigration = True})
        `shouldBe` ["--create-only", "--name", "something"]
