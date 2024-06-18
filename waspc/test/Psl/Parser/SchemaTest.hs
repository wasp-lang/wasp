{-# LANGUAGE DeriveDataTypeable #-}

module Psl.Parser.SchemaTest where

import qualified Data.Text as T
import NeatInterpolation (trimming)
import Test.Tasty.Hspec
import qualified Wasp.Psl.Ast.Attribute as Psl.Attribute
import qualified Wasp.Psl.Ast.ConfigBlock as Psl.ConfigBlock
import qualified Wasp.Psl.Ast.Enum as Psl.Enum
import qualified Wasp.Psl.Ast.Model as Psl.Model
import qualified Wasp.Psl.Ast.Schema as Psl.Schema
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
          Psl.Schema.Schema
            [ Psl.Schema.ConfigBlock $
                Psl.ConfigBlock.ConfigBlock
                  Psl.ConfigBlock.Datasource
                  "db"
                  [ Psl.ConfigBlock.KeyValuePair "provider" "\"postgresql\"",
                    Psl.ConfigBlock.KeyValuePair "url" "env(\"DATABASE_URL\")",
                    Psl.ConfigBlock.KeyValuePair "extensions" "[hstore(schema: \"myHstoreSchema\"), pg_trgm, postgis(version: \"2.1\")]"
                  ],
              Psl.Schema.ConfigBlock $
                Psl.ConfigBlock.ConfigBlock
                  Psl.ConfigBlock.Generator
                  "client"
                  [ Psl.ConfigBlock.KeyValuePair "provider" "\"prisma-client-js\"",
                    Psl.ConfigBlock.KeyValuePair "previewFeatures" "[\"postgresqlExtensions\"]"
                  ],
              Psl.Schema.ModelBlock $
                Psl.Model.Model
                  "User"
                  ( Psl.Model.Body
                      [ Psl.Model.ElementField $
                          Psl.Model.Field
                            "id"
                            Psl.Model.Int
                            []
                            [ Psl.Attribute.Attribute "id" [],
                              Psl.Attribute.Attribute "default" [Psl.Attribute.AttrArgUnnamed $ Psl.Attribute.AttrArgFunc "autoincrement"]
                            ],
                        Psl.Model.ElementField $
                          Psl.Model.Field
                            "tasks"
                            (Psl.Model.UserType "Task")
                            [Psl.Model.List]
                            [],
                        Psl.Model.ElementField $
                          Psl.Model.Field
                            "address"
                            Psl.Model.String
                            [Psl.Model.Optional]
                            [],
                        Psl.Model.ElementField $
                          Psl.Model.Field
                            "votes"
                            (Psl.Model.UserType "TaskVote")
                            [Psl.Model.List]
                            []
                      ]
                  ),
              Psl.Schema.ModelBlock $
                Psl.Model.Model
                  "Task"
                  ( Psl.Model.Body
                      [ Psl.Model.ElementField $
                          Psl.Model.Field
                            "id"
                            Psl.Model.Int
                            []
                            [ Psl.Attribute.Attribute "id" [],
                              Psl.Attribute.Attribute "default" [Psl.Attribute.AttrArgUnnamed $ Psl.Attribute.AttrArgFunc "autoincrement"]
                            ],
                        Psl.Model.ElementField $
                          Psl.Model.Field
                            "description"
                            Psl.Model.String
                            []
                            [],
                        Psl.Model.ElementField $
                          Psl.Model.Field
                            "isDone"
                            Psl.Model.Boolean
                            []
                            [ Psl.Attribute.Attribute "default" [Psl.Attribute.AttrArgUnnamed $ Psl.Attribute.AttrArgIdentifier "false"]
                            ],
                        Psl.Model.ElementField $
                          Psl.Model.Field
                            "user"
                            (Psl.Model.UserType "User")
                            []
                            [ Psl.Attribute.Attribute
                                "relation"
                                [ Psl.Attribute.AttrArgNamed "fields" (Psl.Attribute.AttrArgFieldRefList ["userId"]),
                                  Psl.Attribute.AttrArgNamed "references" (Psl.Attribute.AttrArgFieldRefList ["id"])
                                ]
                            ],
                        Psl.Model.ElementField $
                          Psl.Model.Field
                            "userId"
                            Psl.Model.Int
                            []
                            [],
                        Psl.Model.ElementField $
                          Psl.Model.Field
                            "votes"
                            (Psl.Model.UserType "TaskVote")
                            [Psl.Model.List]
                            []
                      ]
                  ),
              Psl.Schema.ModelBlock $
                Psl.Model.Model
                  "TaskVote"
                  ( Psl.Model.Body
                      [ Psl.Model.ElementField $
                          Psl.Model.Field
                            "user"
                            (Psl.Model.UserType "User")
                            []
                            [ Psl.Attribute.Attribute
                                "relation"
                                [ Psl.Attribute.AttrArgNamed "fields" (Psl.Attribute.AttrArgFieldRefList ["userId"]),
                                  Psl.Attribute.AttrArgNamed "references" (Psl.Attribute.AttrArgFieldRefList ["id"])
                                ]
                            ],
                        Psl.Model.ElementField $
                          Psl.Model.Field
                            "userId"
                            Psl.Model.Int
                            []
                            [],
                        Psl.Model.ElementField $
                          Psl.Model.Field
                            "task"
                            (Psl.Model.UserType "Task")
                            []
                            [ Psl.Attribute.Attribute
                                "relation"
                                [ Psl.Attribute.AttrArgNamed "fields" (Psl.Attribute.AttrArgFieldRefList ["taskId"]),
                                  Psl.Attribute.AttrArgNamed "references" (Psl.Attribute.AttrArgFieldRefList ["id"])
                                ]
                            ],
                        Psl.Model.ElementField $
                          Psl.Model.Field
                            "taskId"
                            Psl.Model.Int
                            []
                            [],
                        Psl.Model.ElementBlockAttribute $
                          Psl.Attribute.Attribute
                            "id"
                            [ Psl.Attribute.AttrArgUnnamed (Psl.Attribute.AttrArgFieldRefList ["userId", "taskId"])
                            ]
                      ]
                  ),
              Psl.Schema.EnumBlock $
                Psl.Enum.Enum
                  "Role"
                  [ Psl.Enum.ElementValue "USER" [],
                    Psl.Enum.ElementValue "ADMIN" []
                  ]
            ]

    it "Prisma file is correctly parsed" $ do
      Psl.Parser.parsePrismaSchema prismaSchema `shouldBe` Right expectedAst
