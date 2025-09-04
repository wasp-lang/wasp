{-# LANGUAGE DeriveDataTypeable #-}

module Psl.Generator.WithCtxTest where

import qualified Data.Text as T
import NeatInterpolation (trimming)
import Test.Tasty.Hspec
import qualified Wasp.Psl.Ast.Argument as Psl.Argument
import qualified Wasp.Psl.Ast.Attribute as Psl.Attribute
import qualified Wasp.Psl.Ast.Model as Psl.Model
import qualified Wasp.Psl.Ast.Schema as Psl.Schema
import Wasp.Psl.Ast.WithCtx (commentedNode)
import qualified Wasp.Psl.Generator.Schema as Psl.Generator
import Wasp.Util (trim)

spec_generatePslWithCtx :: Spec
spec_generatePslWithCtx = do
  describe "Triple slash documentation comments" $ do
    it "Prisma model with leading triple slash comments is correctly generated" $ do
      let inputAst =
            Psl.Schema.Schema
              [ commentedNode [" Documentation comment for `MyModel`"] $
                  Psl.Schema.ModelBlock $
                    Psl.Model.Model
                      "MyModel"
                      ( Psl.Model.Body
                          [ commentedNode [" Simple comment attached to `prop1`"] $
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
                            commentedNode [" Simple comment attached to `prop2`"] $
                              Psl.Model.ElementField $
                                Psl.Model.Field
                                  "prop2"
                                  Psl.Model.String
                                  []
                                  [Psl.Attribute.Attribute "unique" []],
                            commentedNode
                              [ " Multiline leading comments",
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

          expectedPrismaSchema =
            T.unpack
              [trimming|
                /// Documentation comment for `MyModel`
                model MyModel {
                  /// Simple comment attached to `prop1`
                  prop1 Int @id @default(autoincrement())
                  /// Simple comment attached to `prop2`
                  prop2 String @unique
                  /// Multiline leading comments
                  /// For prop3
                  prop3 String @unique
                }
              |]

      trim (Psl.Generator.generateSchema inputAst) `shouldBe` expectedPrismaSchema
