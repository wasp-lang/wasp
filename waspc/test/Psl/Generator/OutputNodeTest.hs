{-# LANGUAGE DeriveDataTypeable #-}

module Psl.Generator.OutputNodeTest where

import Data.Function (on)
import qualified Data.Text as T
import NeatInterpolation (trimming)
import Test.Tasty.Hspec
import qualified Wasp.Psl.Ast.Argument as Psl.Argument
import qualified Wasp.Psl.Ast.Attribute as Psl.Attribute
import qualified Wasp.Psl.Ast.Model as Psl.Model
import Wasp.Psl.Ast.OutputNode (commentedNode)
import qualified Wasp.Psl.Ast.Schema as Psl.Schema
import qualified Wasp.Psl.Generator.Schema as Psl.Generator

spec_generatePslNode :: Spec
spec_generatePslNode = do
  describe "Triple slash documentation comments" $ do
    it "Prisma file is correctly generated" $ do
      let inputAst =
            Psl.Schema.ModelBlock $
              pure $
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
                            " For prop3",
                            " And also trailing!"
                          ]
                          $ Psl.Model.ElementField $
                            Psl.Model.Field
                              "prop3"
                              Psl.Model.String
                              []
                              [Psl.Attribute.Attribute "unique" []]
                      ]
                  )

          expectedPrismaSchema =
            T.unpack
              [trimming|
                model MyModel {
                  /// Simple comment attached to `prop1`
                  prop1 Int @id @default(autoincrement())

                  /// Simple comment attached to `prop2`
                  prop2 String @unique

                  /// Multiline leading comments
                  /// For prop3
                  /// And also trailing!
                  prop3 String @unique
                }
              |]

      Psl.Generator.generateSchemaBlock inputAst `shouldBe_ignoringWhitespace` expectedPrismaSchema

    it "Prisma-zod example" $ do
      let inputAst =
            Psl.Schema.ModelBlock $
              pure $
                Psl.Model.Model
                  "Post"
                  ( Psl.Model.Body
                      [ commentedNode [" @zod.uuid()"] $
                          Psl.Model.ElementField $
                            Psl.Model.Field
                              "id"
                              Psl.Model.String
                              []
                              [ Psl.Attribute.Attribute "id" [],
                                Psl.Attribute.Attribute
                                  "default"
                                  [Psl.Argument.ArgUnnamed $ Psl.Argument.FuncExpr "uuid" []]
                              ],
                        commentedNode [" @zod.max(255, { message: \"The title must be shorter than 256 characters\" })"] $
                          Psl.Model.ElementField $
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

          expectedPrismaSchema =
            T.unpack
              [trimming|
                model Post {
                  /// @zod.uuid()
                  id String @id @default(uuid())

                  /// @zod.max(255, { message: "The title must be shorter than 256 characters" })
                  title String

                  /// @zod.max(10240)
                  contents String
                }
              |]

      Psl.Generator.generateSchemaBlock inputAst `shouldBe_ignoringWhitespace` expectedPrismaSchema

shouldBe_ignoringWhitespace :: String -> String -> Expectation
shouldBe_ignoringWhitespace = shouldBe `on` removeWhitespace

-- | Removes leading indentation and empty lines from the string.
removeWhitespace :: String -> String
removeWhitespace = unlines . filter (not . null) . map (dropWhile (== ' ')) . lines
