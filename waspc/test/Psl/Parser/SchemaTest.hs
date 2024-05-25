{-# LANGUAGE DeriveDataTypeable #-}

module Psl.Parser.SchemaTest where

import qualified Data.Text as T
import NeatInterpolation (trimming)
import Test.Tasty.Hspec
import qualified Wasp.Psl.Ast.Schema as AST
import qualified Wasp.Psl.Parser.Schema as Psl.Parser

spec_parsePslSchema :: Spec
spec_parsePslSchema = do
  describe "Full Prisma file example" $ do
    let prismaSchema =
          T.unpack
            [trimming|
          // We parse the `datastore` declaration, but only use the `extensions` field
          datasource db {
            provider = "postgresql"
            url      = env("DATABASE_URL")
            extensions = [hstore(schema: "myHstoreSchema"), pg_trgm, postgis(version: "2.1")]
          }

          // We parse the `generator` declaration, but only use the `previewFeatures` field
          generator client {
            provider = "prisma-client-js"
            previewFeatures = ["postgresqlExtensions"]
          }

          model User {
            id      Int        @id @default(autoincrement())
            tasks   Task[]
            address String?
            votes   TaskVote[]
          }

          model Task {
            // Testing comments
            id          Int        @id @default(autoincrement())
            description String
            isDone      Boolean    @default(false)
            user        User       @relation(fields: [userId], references: [id])
            userId      Int
            votes       TaskVote[]
          }

          model TaskVote {
            user   User   @relation(fields: [userId], references: [id])
            userId Int
            task   Task   @relation(fields: [taskId], references: [id])
            taskId Int

            @@id([userId, taskId])
          }

          enum Role {
            USER
            ADMIN
          }
        |]
        expectedAst =
          AST.Schema
            [ AST.SchemaDatasource $
                AST.Datasource
                  "db"
                  [ AST.ConfigBlockKeyValue "provider" "\"postgresql\"",
                    AST.ConfigBlockKeyValue "url" "env(\"DATABASE_URL\")",
                    AST.ConfigBlockKeyValue "extensions" "[hstore(schema: \"myHstoreSchema\"), pg_trgm, postgis(version: \"2.1\")]"
                  ],
              AST.SchemaGenerator $
                AST.Generator
                  "client"
                  [ AST.ConfigBlockKeyValue "provider" "\"prisma-client-js\"",
                    AST.ConfigBlockKeyValue "previewFeatures" "[\"postgresqlExtensions\"]"
                  ],
              AST.SchemaModel $
                AST.Model
                  "User"
                  ( AST.Body
                      [ AST.ElementField $
                          AST.Field
                            "id"
                            AST.Int
                            []
                            [ AST.Attribute "id" [],
                              AST.Attribute "default" [AST.AttrArgUnnamed $ AST.AttrArgFunc "autoincrement"]
                            ],
                        AST.ElementField $
                          AST.Field
                            "tasks"
                            (AST.UserType "Task")
                            [AST.List]
                            [],
                        AST.ElementField $
                          AST.Field
                            "address"
                            AST.String
                            [AST.Optional]
                            [],
                        AST.ElementField $
                          AST.Field
                            "votes"
                            (AST.UserType "TaskVote")
                            [AST.List]
                            []
                      ]
                  ),
              AST.SchemaModel $
                AST.Model
                  "Task"
                  ( AST.Body
                      [ AST.ElementField $
                          AST.Field
                            "id"
                            AST.Int
                            []
                            [ AST.Attribute "id" [],
                              AST.Attribute "default" [AST.AttrArgUnnamed $ AST.AttrArgFunc "autoincrement"]
                            ],
                        AST.ElementField $
                          AST.Field
                            "description"
                            AST.String
                            []
                            [],
                        AST.ElementField $
                          AST.Field
                            "isDone"
                            AST.Boolean
                            []
                            [ AST.Attribute "default" [AST.AttrArgUnnamed $ AST.AttrArgIdentifier "false"]
                            ],
                        AST.ElementField $
                          AST.Field
                            "user"
                            (AST.UserType "User")
                            []
                            [ AST.Attribute
                                "relation"
                                [ AST.AttrArgNamed "fields" (AST.AttrArgFieldRefList ["userId"]),
                                  AST.AttrArgNamed "references" (AST.AttrArgFieldRefList ["id"])
                                ]
                            ],
                        AST.ElementField $
                          AST.Field
                            "userId"
                            AST.Int
                            []
                            [],
                        AST.ElementField $
                          AST.Field
                            "votes"
                            (AST.UserType "TaskVote")
                            [AST.List]
                            []
                      ]
                  ),
              AST.SchemaModel $
                AST.Model
                  "TaskVote"
                  ( AST.Body
                      [ AST.ElementField $
                          AST.Field
                            "user"
                            (AST.UserType "User")
                            []
                            [ AST.Attribute
                                "relation"
                                [ AST.AttrArgNamed "fields" (AST.AttrArgFieldRefList ["userId"]),
                                  AST.AttrArgNamed "references" (AST.AttrArgFieldRefList ["id"])
                                ]
                            ],
                        AST.ElementField $
                          AST.Field
                            "userId"
                            AST.Int
                            []
                            [],
                        AST.ElementField $
                          AST.Field
                            "task"
                            (AST.UserType "Task")
                            []
                            [ AST.Attribute
                                "relation"
                                [ AST.AttrArgNamed "fields" (AST.AttrArgFieldRefList ["taskId"]),
                                  AST.AttrArgNamed "references" (AST.AttrArgFieldRefList ["id"])
                                ]
                            ],
                        AST.ElementField $
                          AST.Field
                            "taskId"
                            AST.Int
                            []
                            [],
                        AST.ElementBlockAttribute $
                          AST.Attribute
                            "id"
                            [ AST.AttrArgUnnamed (AST.AttrArgFieldRefList ["userId", "taskId"])
                            ]
                      ]
                  ),
              AST.SchemaEnum $
                AST.PrismaEnum
                  "Role"
                  [ AST.EnumValue "USER" [],
                    AST.EnumValue "ADMIN" []
                  ]
            ]

    it "Prisma file is correctly parsed" $ do
      Psl.Parser.parsePrismaSchema prismaSchema `shouldBe` Right expectedAst
