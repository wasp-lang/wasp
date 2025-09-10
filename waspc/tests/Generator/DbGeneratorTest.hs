module Generator.DbGeneratorTest where

import qualified Data.Text as T
import Test.Tasty.Hspec (Spec, describe, it, shouldBe)
import Wasp.Generator.DbGenerator.Common
  ( MigrateArgs (..),
    defaultMigrateArgs,
  )
import Wasp.Generator.DbGenerator.Jobs (asPrismaCliArgs)
import Wasp.Generator.DbGenerator.Operations (prismaErrorContainsDbNotCreatedError)

spec_Jobs :: Spec
spec_Jobs =
  it "should produce expected args" $ do
    asPrismaCliArgs defaultMigrateArgs `shouldBe` []
    asPrismaCliArgs (MigrateArgs {_migrationName = Nothing, _isCreateOnlyMigration = True})
      `shouldBe` ["--create-only"]
    asPrismaCliArgs (MigrateArgs {_migrationName = Just "something", _isCreateOnlyMigration = False})
      `shouldBe` ["--name", "something"]
    asPrismaCliArgs (MigrateArgs {_migrationName = Just "something else longer", _isCreateOnlyMigration = False})
      `shouldBe` ["--name", "something else longer"]
    asPrismaCliArgs (MigrateArgs {_migrationName = Just "something", _isCreateOnlyMigration = True})
      `shouldBe` ["--create-only", "--name", "something"]

spec_DbConnectionTest :: Spec
spec_DbConnectionTest =
  describe "prismaErrorContainsDbNotCreatedError" $ do
    it "should not match DB server not available error" $ do
      prismaErrorContainsDbNotCreatedError
        (T.pack "Error: P1001\n\nCan't reach database server at `localhost`:`5432`\n\nPlease make sure your database server is running at `localhost`:`5432`.")
        `shouldBe` False
    it "should not match similar error codes" $ do
      prismaErrorContainsDbNotCreatedError
        (T.pack "Error: P10033\n\nMade up error code")
        `shouldBe` False
    it "should match the DB not created error code" $ do
      prismaErrorContainsDbNotCreatedError
        (T.pack "Error: P1003\n\nDatabase `x` does not exist on the database server at `localhost:5432`.")
        `shouldBe` True
