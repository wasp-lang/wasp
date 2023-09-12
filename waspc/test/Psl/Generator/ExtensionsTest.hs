module Psl.Generator.ExtensionsTest where

import Test.Tasty.Hspec (Spec, describe, it, shouldBe)
import qualified Wasp.AppSpec.App.Db as AS.Db
import Wasp.Psl.Generator.Extensions (showDbExtensions)

spec_DbGeneratorPrisma :: Spec
spec_DbGeneratorPrisma = do
  describe "showDbExtensions" $ do
    it "returns empty string when no extensions are defined" $ do
      showDbExtensions [] `shouldBe` "[]"

    it "returns proper string for one extension with no options" $ do
      showDbExtensions
        [ AS.Db.PrismaDbExtension
            { AS.Db.name = "pg_vector",
              AS.Db.version = Nothing,
              AS.Db.map = Nothing,
              AS.Db.schema = Nothing
            }
        ]
        `shouldBe` "[pg_vector]"

    it "returns proper string for one extension with one option" $ do
      showDbExtensions
        [ AS.Db.PrismaDbExtension
            { AS.Db.name = "pg_vector",
              AS.Db.version = Nothing,
              AS.Db.map = Just "vector",
              AS.Db.schema = Nothing
            }
        ]
        `shouldBe` "[pg_vector(map: \"vector\")]"

    it "returns proper string for multiple extensions with more options" $ do
      showDbExtensions
        [ AS.Db.PrismaDbExtension
            { AS.Db.name = "pg_vector",
              AS.Db.version = Nothing,
              AS.Db.map = Just "vector",
              AS.Db.schema = Just "public"
            },
          AS.Db.PrismaDbExtension
            { AS.Db.name = "pg_trgm",
              AS.Db.version = Just "1.0.0",
              AS.Db.map = Nothing,
              AS.Db.schema = Nothing
            }
        ]
        `shouldBe` "[pg_vector(map: \"vector\", schema: \"public\"), pg_trgm(version: \"1.0.0\")]"
