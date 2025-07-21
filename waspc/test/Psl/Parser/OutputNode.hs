{-# LANGUAGE DeriveDataTypeable #-}

module Psl.Parser.SchemaTest where

import qualified Data.Text as T
import NeatInterpolation (trimming)
import Test.Tasty.Hspec
import qualified Wasp.Psl.Ast.Argument as Psl.Argument
import qualified Wasp.Psl.Ast.Attribute as Psl.Attribute
import qualified Wasp.Psl.Ast.Model as Psl.Model
import Wasp.Psl.Ast.OutputNode (commentedNode, justNode)
import qualified Wasp.Psl.Ast.Schema as Psl.Schema
import qualified Wasp.Psl.Parser.Schema as Psl.Parser

spec_parsePslNode :: Spec
spec_parsePslNode = do
  describe "Triple slash documentation comments" $ do
    it "Prisma file is correctly parsed" $ do
      let prismaSchema =
            T.unpack
              [trimming|
                model MyModel {
                  /// Simple comment attached to `prop1`
                  prop1    Int    @id @default(autoincrement())
                  // Random comment that should be ignored
                  prop2  String @unique /// Simple comment attached to `prop2`
                  /// Multiline leading comments
                  /// For prop3
                  prop3  String @unique /// And also trailing!
                }
              |]
          expectedAst =
            Psl.Schema.Schema
              [ justNode $
                  Psl.Schema.ModelBlock $
                    Psl.Model.Model
                      "MyModel"
                      ( Psl.Model.Body
                          [ commentedNode ["Simple comment attached to `prop1`"] $
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
                            commentedNode ["Simple comment attached to `prop2`"] $
                              Psl.Model.ElementField $
                                Psl.Model.Field
                                  "prop2"
                                  Psl.Model.String
                                  []
                                  [Psl.Attribute.Attribute "unique" []],
                            commentedNode
                              [ "Multiline leading comments",
                                "For prop3",
                                "And also trailing!"
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
