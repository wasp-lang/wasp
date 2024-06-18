module Psl.Parser.ConfigBlockTest where

import qualified Data.Text as T
import NeatInterpolation (trimming)
import Test.Tasty.Hspec
import qualified Text.Parsec as Parsec
import qualified Wasp.Psl.Ast.ConfigBlock as Psl.ConfigBlock
import qualified Wasp.Psl.Ast.Schema as Psl.Schema
import qualified Wasp.Psl.Parser.ConfigBlock as Psl.Parser

spec_parsePslConfigBlock :: Spec
spec_parsePslConfigBlock = do
  describe "Datasource parsing" $ do
    it "Basic example" $ do
      let source =
            T.unpack
              [trimming|
                datasource db {
                  provider = "postgresql"
                  url      = env("DATABASE_URL")
                  extensions = [hstore(schema: "myHstoreSchema"), pg_trgm, postgis(version: "2.1")]
                }
              |]
          expectedAst =
            Psl.Schema.ConfigBlock $
              Psl.ConfigBlock.ConfigBlock
                Psl.ConfigBlock.Datasource
                "db"
                [ Psl.ConfigBlock.KeyValuePair "provider" "\"postgresql\"",
                  Psl.ConfigBlock.KeyValuePair "url" "env(\"DATABASE_URL\")",
                  Psl.ConfigBlock.KeyValuePair "extensions" "[hstore(schema: \"myHstoreSchema\"), pg_trgm, postgis(version: \"2.1\")]"
                ]
      Parsec.parse Psl.Parser.configBlock "" source `shouldBe` Right expectedAst

    it "Commented out fields" $ do
      let source =
            T.unpack
              [trimming|
                datasource db {
                  provider = "postgresql"
                  url      = env("DATABASE_URL")
                  // extensions = [hstore(schema: "myHstoreSchema"), pg_trgm, postgis(version: "2.1")]
                }
              |]
          expectedAst =
            Psl.Schema.ConfigBlock $
              Psl.ConfigBlock.ConfigBlock
                Psl.ConfigBlock.Datasource
                "db"
                [ Psl.ConfigBlock.KeyValuePair "provider" "\"postgresql\"",
                  Psl.ConfigBlock.KeyValuePair "url" "env(\"DATABASE_URL\")"
                ]
      Parsec.parse Psl.Parser.configBlock "" source `shouldBe` Right expectedAst

  describe "Generator parsing" $ do
    it "Basic example" $ do
      let source =
            T.unpack
              [trimming|
                generator client {
                  provider = "prisma-client-js"
                  previewFeatures = ["postgresqlExtensions"]
                }
              |]
          expectedAst =
            Psl.Schema.ConfigBlock $
              Psl.ConfigBlock.ConfigBlock
                Psl.ConfigBlock.Generator
                "client"
                [ Psl.ConfigBlock.KeyValuePair "provider" "\"prisma-client-js\"",
                  Psl.ConfigBlock.KeyValuePair "previewFeatures" "[\"postgresqlExtensions\"]"
                ]
      Parsec.parse Psl.Parser.configBlock "" source `shouldBe` Right expectedAst

    it "Commented out fields" $ do
      let source =
            T.unpack
              [trimming|
                  generator client {
                    provider = "prisma-client-js"
                    // previewFeatures = ["postgresqlExtensions"]
                  }
                |]
          expectedAst =
            Psl.Schema.ConfigBlock $
              Psl.ConfigBlock.ConfigBlock
                Psl.ConfigBlock.Generator
                "client"
                [ Psl.ConfigBlock.KeyValuePair "provider" "\"prisma-client-js\""
                ]
      Parsec.parse Psl.Parser.configBlock "" source `shouldBe` Right expectedAst
