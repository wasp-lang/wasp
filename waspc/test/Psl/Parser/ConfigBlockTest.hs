module Psl.Parser.ConfigBlockTest where

import qualified Data.Text as T
import NeatInterpolation (trimming)
import Test.Tasty.Hspec
import qualified Text.Parsec as Parsec
import qualified Wasp.Psl.Ast.Schema as AST
import qualified Wasp.Psl.Parser.ConfigBlock as Psl.Parser

spec_parsePslConfigBlock :: Spec
spec_parsePslConfigBlock = do
  describe "Datasource parsing" $ do
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
          AST.SchemaDatasource $
            AST.Datasource
              "db"
              [ AST.ConfigBlockKeyValue "provider" "\"postgresql\"",
                AST.ConfigBlockKeyValue "url" "env(\"DATABASE_URL\")",
                AST.ConfigBlockKeyValue "extensions" "[hstore(schema: \"myHstoreSchema\"), pg_trgm, postgis(version: \"2.1\")]"
              ]

    it "Datasource is correctly parsed" $ do
      Parsec.parse Psl.Parser.configBlock "" source `shouldBe` Right expectedAst

  describe "Generator parsing" $ do
    let source =
          T.unpack
            [trimming|
              generator client {
                provider = "prisma-client-js"
                previewFeatures = ["postgresqlExtensions"]
              }
            |]
        expectedAst =
          AST.SchemaGenerator $
            AST.Generator
              "client"
              [ AST.ConfigBlockKeyValue "provider" "\"prisma-client-js\"",
                AST.ConfigBlockKeyValue "previewFeatures" "[\"postgresqlExtensions\"]"
              ]

    it "Generator is correctly parsed" $ do
      Parsec.parse Psl.Parser.configBlock "" source `shouldBe` Right expectedAst
