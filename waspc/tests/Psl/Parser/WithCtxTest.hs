{-# LANGUAGE DeriveDataTypeable #-}

module Psl.Parser.WithCtxTest where

import Data.Either (isLeft)
import qualified Data.Text as T
import NeatInterpolation (trimming)
import Test.Tasty.Hspec
import qualified Wasp.Psl.Ast.Argument as Psl.Argument
import qualified Wasp.Psl.Ast.Attribute as Psl.Attribute
import qualified Wasp.Psl.Ast.Model as Psl.Model
import qualified Wasp.Psl.Ast.Schema as Psl.Schema
import Wasp.Psl.Ast.WithCtx (commentedNode)
import qualified Wasp.Psl.Ast.WithCtx as Psl.WithCtx
import qualified Wasp.Psl.Parser.Schema as Psl.Parser

spec_parsePslWithCtx :: Spec
spec_parsePslWithCtx = do
  describe "Triple slash documentation comments" $ do
    it "Prisma file is correctly parsed" $ do
      let prismaSchema =
            T.unpack
              [trimming|
                model MyModel {
                  /// Doc comment attached to `prop1`
                  prop1    Int    @id @default(autoincrement())
                  // Regular comment that should be ignored
                  prop2  String @unique
                  /// Multiline comments
                  /// For prop3
                  prop3  String @unique
                }
              |]
          expectedAst =
            Psl.Schema.Schema $
              Psl.WithCtx.empty
                <$> [ Psl.Schema.ModelBlock $
                        Psl.Model.Model
                          "MyModel"
                          ( Psl.Model.Body
                              [ commentedNode [" Doc comment attached to `prop1`"] $
                                  Psl.Model.ElementField $
                                    Psl.Model.Field
                                      "prop1"
                                      Psl.Model.Int
                                      []
                                      [ Psl.Attribute.Attribute "id" [],
                                        Psl.Attribute.Attribute
                                          "default"
                                          [Psl.Argument.ArgUnnamed $ Psl.Argument.FuncExpr "autoincrement" []]
                                      ],
                                Psl.WithCtx.empty $
                                  Psl.Model.ElementField $
                                    Psl.Model.Field
                                      "prop2"
                                      Psl.Model.String
                                      []
                                      [Psl.Attribute.Attribute "unique" []],
                                commentedNode
                                  [ " Multiline comments",
                                    " For prop3"
                                  ]
                                  $ Psl.Model.ElementField $
                                    Psl.Model.Field
                                      "prop3"
                                      Psl.Model.String
                                      []
                                      [Psl.Attribute.Attribute "unique" []]
                              ]
                          )
                    ]

      Psl.Parser.parsePrismaSchema prismaSchema `shouldBe` Right expectedAst

    it "Prisma-zod example" $ do
      let prismaSchema =
            T.unpack
              [trimming|
                model Post {
                  /// The unique identifier for the post
                  /// @zod.uuid()
                  id String @id @default(uuid())

                  /// A brief title that describes the contents of the post
                  /// @zod.max(255, { message: "The title must be shorter than 256 characters" })
                  title String

                  /// @zod.max(10240)
                  contents String
                }
              |]
          expectedAst =
            Psl.Schema.Schema $
              Psl.WithCtx.empty
                <$> [ Psl.Schema.ModelBlock $
                        Psl.Model.Model
                          "Post"
                          ( Psl.Model.Body
                              [ commentedNode
                                  [ " The unique identifier for the post",
                                    " @zod.uuid()"
                                  ]
                                  $ Psl.Model.ElementField $
                                    Psl.Model.Field
                                      "id"
                                      Psl.Model.String
                                      []
                                      [ Psl.Attribute.Attribute "id" [],
                                        Psl.Attribute.Attribute
                                          "default"
                                          [Psl.Argument.ArgUnnamed $ Psl.Argument.FuncExpr "uuid" []]
                                      ],
                                commentedNode
                                  [ " A brief title that describes the contents of the post",
                                    " @zod.max(255, { message: \"The title must be shorter than 256 characters\" })"
                                  ]
                                  $ Psl.Model.ElementField $
                                    Psl.Model.Field
                                      "title"
                                      Psl.Model.String
                                      []
                                      [],
                                commentedNode [" @zod.max(10240)"] $
                                  Psl.Model.ElementField $
                                    Psl.Model.Field
                                      "contents"
                                      Psl.Model.String
                                      []
                                      []
                              ]
                          )
                    ]

      Psl.Parser.parsePrismaSchema prismaSchema `shouldBe` Right expectedAst

    it "Trailing triple-slash comment fails to parse" $ do
      let prismaSchema =
            T.unpack
              [trimming|
                model MyModel {
                  prop1 Int @id /// Trailing documentation comment
                }
              |]

      Psl.Parser.parsePrismaSchema prismaSchema `shouldSatisfy` isLeft
