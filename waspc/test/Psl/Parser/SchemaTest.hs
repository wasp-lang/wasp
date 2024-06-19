{-# LANGUAGE DeriveDataTypeable #-}

module Psl.Parser.SchemaTest where

import qualified Data.Text as T
import NeatInterpolation (trimming)
import Test.Tasty.Hspec
import qualified Wasp.Psl.Ast.Argument as Psl.Argument
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
            url      = env("DATABASE_URL") // some inline comment
            extensions = [hstore(schema: "myHstoreSchema"), pg_trgm, postgis(version: "2.1")]
          }

          // We parse the `generator` declaration, but only use the `previewFeatures` field
          generator client {
            provider = "prisma-client-js" // some inline comment
            previewFeatures = ["postgresqlExtensions"]
          }

          model User {
            id      Int        @id @default(autoincrement())
            tasks   Task[]
            address String? // Testing comments
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
            USER // inline comment
            ADMIN
          }
        |]
        expectedAst =
          Psl.Schema.Schema
            [ Psl.Schema.ConfigBlock $
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
                        [ Psl.Argument.FuncExpr
                            "hstore"
                            [ Psl.Argument.ArgNamed "schema" $ Psl.Argument.StringExpr "myHstoreSchema"
                            ],
                          Psl.Argument.IdentifierExpr "pg_trgm",
                          Psl.Argument.FuncExpr
                            "postgis"
                            [ Psl.Argument.ArgNamed "version" $ Psl.Argument.StringExpr "2.1"
                            ]
                        ]
                  ],
              Psl.Schema.ConfigBlock $
                Psl.ConfigBlock.ConfigBlock
                  Psl.ConfigBlock.Generator
                  "client"
                  [ Psl.ConfigBlock.KeyValuePair "provider" $ Psl.Argument.StringExpr "prisma-client-js",
                    Psl.ConfigBlock.KeyValuePair "previewFeatures" $
                      Psl.Argument.ArrayExpr [Psl.Argument.StringExpr "postgresqlExtensions"]
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
                              Psl.Attribute.Attribute
                                "default"
                                [Psl.Argument.ArgUnnamed $ Psl.Argument.FuncExpr "autoincrement" []]
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
                              Psl.Attribute.Attribute
                                "default"
                                [ Psl.Argument.ArgUnnamed $ Psl.Argument.FuncExpr "autoincrement" []
                                ]
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
                            [ Psl.Attribute.Attribute "default" [Psl.Argument.ArgUnnamed $ Psl.Argument.IdentifierExpr "false"]
                            ],
                        Psl.Model.ElementField $
                          Psl.Model.Field
                            "user"
                            (Psl.Model.UserType "User")
                            []
                            [ Psl.Attribute.Attribute
                                "relation"
                                [ Psl.Argument.ArgNamed
                                    "fields"
                                    ( Psl.Argument.ArrayExpr
                                        [ Psl.Argument.IdentifierExpr "userId"
                                        ]
                                    ),
                                  Psl.Argument.ArgNamed
                                    "references"
                                    ( Psl.Argument.ArrayExpr
                                        [ Psl.Argument.IdentifierExpr "id"
                                        ]
                                    )
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
                                [ Psl.Argument.ArgNamed
                                    "fields"
                                    ( Psl.Argument.ArrayExpr
                                        [ Psl.Argument.IdentifierExpr "userId"
                                        ]
                                    ),
                                  Psl.Argument.ArgNamed
                                    "references"
                                    ( Psl.Argument.ArrayExpr
                                        [ Psl.Argument.IdentifierExpr "id"
                                        ]
                                    )
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
                                [ Psl.Argument.ArgNamed
                                    "fields"
                                    ( Psl.Argument.ArrayExpr
                                        [ Psl.Argument.IdentifierExpr "taskId"
                                        ]
                                    ),
                                  Psl.Argument.ArgNamed
                                    "references"
                                    ( Psl.Argument.ArrayExpr
                                        [ Psl.Argument.IdentifierExpr "id"
                                        ]
                                    )
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
                            [ Psl.Argument.ArgUnnamed
                                ( Psl.Argument.ArrayExpr
                                    [ Psl.Argument.IdentifierExpr "userId",
                                      Psl.Argument.IdentifierExpr "taskId"
                                    ]
                                )
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
