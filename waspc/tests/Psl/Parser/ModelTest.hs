module Psl.Parser.ModelTest where

import Data.Either (isLeft)
import qualified Data.Text as T
import NeatInterpolation (trimming)
import Psl.Common.ModelTest (sampleBodyAst, sampleBodySchema)
import Test.Tasty.Hspec
import qualified Text.Megaparsec as Megaparsec
import qualified Wasp.Psl.Ast.Argument as Psl.Argument
import qualified Wasp.Psl.Ast.Attribute as Psl.Attribute
import qualified Wasp.Psl.Ast.Model as Psl.Model
import qualified Wasp.Psl.Ast.WithCtx as Psl.WithCtx
import Wasp.Psl.Parser.Model (model, parseBody)

spec_parsePslModel :: Spec
spec_parsePslModel = do
  describe "Complex example" $ do
    let pslModel =
          [trimming|
          model User {
            ${sampleBodySchema}
          }
        |]

    it "Body parser correctly parses" $ do
      parseBody (T.unpack sampleBodySchema) `shouldBe` Right sampleBodyAst

    it "Model parser correctly parses" $ do
      Megaparsec.parse model "" (T.unpack pslModel) `shouldBe` Right (Psl.Model.Model "User" sampleBodyAst)

  describe "Body parser" $ do
    describe "Fails if input is not valid PSL" $ do
      let runTest psl = it psl $ isLeft (parseBody psl) `shouldBe` True
      mapM_
        runTest
        [ "  noType",
          "  @startsWithAttribute",
          "  @@@tooManyMonkeys"
        ]

  -- Based on https://github.com/wasp-lang/wasp/issues/2019
  -- Model names like Internet have Int as a prefix, which is a Prisma field type.
  -- This test checks if the parser can handle this case.
  describe "Prefixing a model name with Prisma field type should work" $ do
    let modelPsl =
          [trimming|
            model User {
              id Int @id @default(value: autoincrement())
              internets Internet[]
              strings Strings[]
            }
          |]
        modelAst =
          Psl.Model.Model
            "User"
            ( Psl.Model.Body $
                Psl.WithCtx.empty
                  <$> [ Psl.Model.ElementField
                          ( Psl.Model.Field
                              "id"
                              Psl.Model.Int
                              []
                              [ Psl.Attribute.Attribute "id" [],
                                Psl.Attribute.Attribute
                                  "default"
                                  [ Psl.Argument.ArgNamed "value" (Psl.Argument.FuncExpr "autoincrement" [])
                                  ]
                              ]
                          ),
                        Psl.Model.ElementField
                          ( Psl.Model.Field "internets" (Psl.Model.UserType "Internet") [Psl.Model.List] []
                          ),
                        Psl.Model.ElementField
                          ( Psl.Model.Field "strings" (Psl.Model.UserType "Strings") [Psl.Model.List] []
                          )
                      ]
            )

    it "Model is correctly parsed" $ do
      Megaparsec.parse model "" (T.unpack modelPsl) `shouldBe` Right modelAst

  -- Prisma PSL allows for model fields to be named with decl keywords.
  describe "Model fields can be named with reserved names" $ do
    let modelPsl =
          [trimming|
            model User {
              model Int
              type String
              view Boolean
              enum Float
              generator Decimal
              datasource String
            }
        |]
        modelAst =
          Psl.Model.Model
            "User"
            ( Psl.Model.Body $
                Psl.WithCtx.empty
                  <$> [ Psl.Model.ElementField (Psl.Model.Field "model" Psl.Model.Int [] []),
                        Psl.Model.ElementField (Psl.Model.Field "type" Psl.Model.String [] []),
                        Psl.Model.ElementField (Psl.Model.Field "view" Psl.Model.Boolean [] []),
                        Psl.Model.ElementField (Psl.Model.Field "enum" Psl.Model.Float [] []),
                        Psl.Model.ElementField (Psl.Model.Field "generator" Psl.Model.Decimal [] []),
                        Psl.Model.ElementField (Psl.Model.Field "datasource" Psl.Model.String [] [])
                      ]
            )

    it "Model is correctly parsed" $ do
      Megaparsec.parse model "" (T.unpack modelPsl) `shouldBe` Right modelAst
