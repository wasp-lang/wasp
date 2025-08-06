module Psl.Parser.ConfigBlockTest where

import qualified Data.Text as T
import NeatInterpolation (trimming)
import Test.Tasty.Hspec
import qualified Text.Megaparsec as Megaparsec
import qualified Wasp.Psl.Ast.Argument as Psl.Argument
import qualified Wasp.Psl.Ast.ConfigBlock as Psl.ConfigBlock
import qualified Wasp.Psl.Parser.ConfigBlock as Psl.Parser

spec_parsePslConfigBlock :: Spec
spec_parsePslConfigBlock = do
  describe "Datasource parsing" $ do
    it "Basic example" $ do
      let source =
            T.unpack
              [trimming|
                datasource db {
                  provider = "postgresql" // some inline comment
                  url      = env("DATABASE_URL")
                  extensions = [hstore(schema: "myHstoreSchema"), pg_trgm, postgis(version: "2.1")]
                }
              |]
          expectedAst =
            Psl.ConfigBlock.ConfigBlock
              Psl.ConfigBlock.Datasource
              "db"
              [ Psl.ConfigBlock.KeyValuePair "provider" $ Psl.Argument.StringExpr "postgresql",
                Psl.ConfigBlock.KeyValuePair "url" $
                  Psl.Argument.FuncExpr
                    "env"
                    [ Psl.Argument.ArgUnnamed $ Psl.Argument.StringExpr "DATABASE_URL"
                    ],
                Psl.ConfigBlock.KeyValuePair "extensions" $
                  Psl.Argument.ArrayExpr
                    [ Psl.Argument.FuncExpr "hstore" [Psl.Argument.ArgNamed "schema" $ Psl.Argument.StringExpr "myHstoreSchema"],
                      Psl.Argument.IdentifierExpr "pg_trgm",
                      Psl.Argument.FuncExpr "postgis" [Psl.Argument.ArgNamed "version" $ Psl.Argument.StringExpr "2.1"]
                    ]
              ]
      Megaparsec.parse Psl.Parser.configBlock "" source `shouldBe` Right expectedAst

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
            Psl.ConfigBlock.ConfigBlock
              Psl.ConfigBlock.Datasource
              "db"
              [ Psl.ConfigBlock.KeyValuePair "provider" $ Psl.Argument.StringExpr "postgresql",
                Psl.ConfigBlock.KeyValuePair "url" $ Psl.Argument.FuncExpr "env" [Psl.Argument.ArgUnnamed $ Psl.Argument.StringExpr "DATABASE_URL"]
              ]
      Megaparsec.parse Psl.Parser.configBlock "" source `shouldBe` Right expectedAst

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
            Psl.ConfigBlock.ConfigBlock
              Psl.ConfigBlock.Generator
              "client"
              [ Psl.ConfigBlock.KeyValuePair "provider" $ Psl.Argument.StringExpr "prisma-client-js",
                Psl.ConfigBlock.KeyValuePair "previewFeatures" $
                  Psl.Argument.ArrayExpr
                    [ Psl.Argument.StringExpr "postgresqlExtensions"
                    ]
              ]
      Megaparsec.parse Psl.Parser.configBlock "" source `shouldBe` Right expectedAst

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
            Psl.ConfigBlock.ConfigBlock
              Psl.ConfigBlock.Generator
              "client"
              [ Psl.ConfigBlock.KeyValuePair "provider" $ Psl.Argument.StringExpr "prisma-client-js"
              ]
      Megaparsec.parse Psl.Parser.configBlock "" source `shouldBe` Right expectedAst
