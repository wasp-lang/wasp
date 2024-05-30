module Project.DbTest where

import Test.Tasty.Hspec
import qualified Wasp.AppSpec.App.Db as AS.App.Db
import Wasp.Project.Db (getDbSystemFromPrismaSchema)
import qualified Wasp.Psl.Ast.Schema as Psl.Ast

spec_DbHelperTest :: Spec
spec_DbHelperTest = do
  describe "getDbSystemFromPrismaSchema" $ do
    it "Correctly extracts PostgreSQL" $ do
      let prismaSchema =
            Psl.Ast.Schema
              [ Psl.Ast.SchemaDatasource $
                  Psl.Ast.Datasource
                    "db"
                    [ Psl.Ast.ConfigBlockKeyValue "provider" "\"postgresql\"",
                      Psl.Ast.ConfigBlockKeyValue "url" "env(\"DATABASE_URL\")",
                      Psl.Ast.ConfigBlockKeyValue "extensions" "[hstore(schema: \"myHstoreSchema\"), pg_trgm, postgis(version: \"2.1\")]"
                    ],
                Psl.Ast.SchemaGenerator $
                  Psl.Ast.Generator
                    "client"
                    [ Psl.Ast.ConfigBlockKeyValue "provider" "\"prisma-client-js\"",
                      Psl.Ast.ConfigBlockKeyValue "previewFeatures" "[\"postgresqlExtensions\"]"
                    ]
              ]

      getDbSystemFromPrismaSchema prismaSchema `shouldBe` AS.App.Db.PostgreSQL

    it "Correctly extracts SQLite" $ do
      let prismaSchema =
            Psl.Ast.Schema
              [ Psl.Ast.SchemaDatasource $
                  Psl.Ast.Datasource
                    "db"
                    [ Psl.Ast.ConfigBlockKeyValue "provider" "\"sqlite\"",
                      Psl.Ast.ConfigBlockKeyValue "url" "env(\"DATABASE_URL\")"
                    ],
                Psl.Ast.SchemaGenerator $
                  Psl.Ast.Generator
                    "client"
                    [Psl.Ast.ConfigBlockKeyValue "provider" "\"prisma-client-js\""]
              ]

      getDbSystemFromPrismaSchema prismaSchema `shouldBe` AS.App.Db.SQLite

    it "Correctly extracts UnsupportedDbSystem" $ do
      let prismaSchema =
            Psl.Ast.Schema
              [ Psl.Ast.SchemaDatasource $
                  Psl.Ast.Datasource
                    "db"
                    [ Psl.Ast.ConfigBlockKeyValue "provider" "\"mongodb\"",
                      Psl.Ast.ConfigBlockKeyValue "url" "env(\"DATABASE_URL\")"
                    ],
                Psl.Ast.SchemaGenerator $
                  Psl.Ast.Generator
                    "client"
                    [Psl.Ast.ConfigBlockKeyValue "provider" "\"prisma-client-js\""]
              ]

      getDbSystemFromPrismaSchema prismaSchema `shouldBe` AS.App.Db.UnsupportedDbSystem "\"mongodb\""

    it "Correctly extracts MissingDbSystem when provider missing" $ do
      let prismaSchema =
            Psl.Ast.Schema
              [ Psl.Ast.SchemaDatasource $
                  Psl.Ast.Datasource
                    "db"
                    [ Psl.Ast.ConfigBlockKeyValue "url" "env(\"DATABASE_URL\")"
                    ],
                Psl.Ast.SchemaGenerator $
                  Psl.Ast.Generator
                    "client"
                    [Psl.Ast.ConfigBlockKeyValue "provider" "\"prisma-client-js\""]
              ]

      getDbSystemFromPrismaSchema prismaSchema `shouldBe` AS.App.Db.MissingDbSystem

    it "Correctly extracts MissingDbSystem when no datasource" $ do
      let prismaSchema =
            Psl.Ast.Schema
              [ Psl.Ast.SchemaGenerator $
                  Psl.Ast.Generator
                    "client"
                    [Psl.Ast.ConfigBlockKeyValue "provider" "\"prisma-client-js\""]
              ]

      getDbSystemFromPrismaSchema prismaSchema `shouldBe` AS.App.Db.MissingDbSystem
