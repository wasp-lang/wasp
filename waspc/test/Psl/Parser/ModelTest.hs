module Psl.Parser.ModelTest where

import Data.Either (isLeft)
import qualified Data.Text as T
import NeatInterpolation (trimming)
import Psl.Common.ModelTest (sampleBodyAst, sampleBodySchema)
import Test.Tasty.Hspec
import qualified Text.Parsec as Parsec
import qualified Wasp.Psl.Ast.Attribute as Psl.Attribute
import qualified Wasp.Psl.Ast.Model as Psl.Model
import qualified Wasp.Psl.Ast.Schema as Psl.Schema
import Wasp.Psl.Parser.Model (body, model)

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
      Parsec.parse body "" (T.unpack sampleBodySchema) `shouldBe` Right sampleBodyAst

    it "Model parser correctly parses" $ do
      Parsec.parse model "" (T.unpack pslModel) `shouldBe` Right (Psl.Schema.SchemaModel $ Psl.Model.Model "User" sampleBodyAst)

  describe "Body parser" $ do
    describe "Fails if input is not valid PSL" $ do
      let runTest psl = it psl $ isLeft (Parsec.parse body "" psl) `shouldBe` True
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
          Psl.Schema.SchemaModel $
            Psl.Model.Model
              "User"
              ( Psl.Model.ModelBody
                  [ Psl.Model.ModelElementField
                      ( Psl.Model.ModelField
                          "id"
                          Psl.Model.Int
                          []
                          [ Psl.Attribute.Attribute "id" [],
                            Psl.Attribute.Attribute
                              "default"
                              [ Psl.Attribute.AttrArgNamed "value" (Psl.Attribute.AttrArgFunc "autoincrement")
                              ]
                          ]
                      ),
                    Psl.Model.ModelElementField
                      ( Psl.Model.ModelField "internets" (Psl.Model.UserType "Internet") [Psl.Model.List] []
                      ),
                    Psl.Model.ModelElementField
                      ( Psl.Model.ModelField "strings" (Psl.Model.UserType "Strings") [Psl.Model.List] []
                      )
                  ]
              )

    it "Model is correctly parsed" $ do
      Parsec.parse model "" (T.unpack modelPsl) `shouldBe` Right modelAst

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
          Psl.Schema.SchemaModel $
            Psl.Model.Model
              "User"
              ( Psl.Model.ModelBody
                  [ Psl.Model.ModelElementField (Psl.Model.ModelField "model" Psl.Model.Int [] []),
                    Psl.Model.ModelElementField (Psl.Model.ModelField "type" Psl.Model.String [] []),
                    Psl.Model.ModelElementField (Psl.Model.ModelField "view" Psl.Model.Boolean [] []),
                    Psl.Model.ModelElementField (Psl.Model.ModelField "enum" Psl.Model.Float [] []),
                    Psl.Model.ModelElementField (Psl.Model.ModelField "generator" Psl.Model.Decimal [] []),
                    Psl.Model.ModelElementField (Psl.Model.ModelField "datasource" Psl.Model.String [] [])
                  ]
              )

    it "Model is correctly parsed" $ do
      Parsec.parse model "" (T.unpack modelPsl) `shouldBe` Right modelAst
