module Psl.Parser.ModelTest where

import Data.Either (isLeft)
import qualified Data.Text as T
import NeatInterpolation (trimming)
import Psl.Common.ModelTest (sampleBodyAst, sampleBodySchema)
import Test.Tasty.Hspec
import qualified Text.Parsec as Parsec
import qualified Wasp.Psl.Ast.Schema as AST
import Wasp.Psl.Parser.Model (model, modelAttrArgument, modelBody)

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
      Parsec.parse modelBody "" (T.unpack sampleBodySchema) `shouldBe` Right sampleBodyAst

    it "Model parser correctly parses" $ do
      Parsec.parse model "" (T.unpack pslModel) `shouldBe` Right (AST.SchemaModel $ AST.Model "User" sampleBodyAst)

  describe "Body parser" $ do
    describe "Fails if input is not valid PSL" $ do
      let runTest psl = it psl $ isLeft (Parsec.parse modelBody "" psl) `shouldBe` True
      mapM_
        runTest
        [ "  noType",
          "  @startsWithAttribute",
          "  @@@tooManyMonkeys"
        ]

  describe "Attribute argument parser" $ do
    let tests =
          [ ( "[foo, bar],",
              AST.AttrArgUnnamed (AST.AttrArgFieldRefList ["foo", "bar"])
            ),
            ( "\"test\")",
              AST.AttrArgUnnamed (AST.AttrArgString "test")
            ),
            ( "foo: bar(),",
              AST.AttrArgNamed "foo" (AST.AttrArgFunc "bar")
            ),
            ( "Bob,",
              AST.AttrArgUnnamed (AST.AttrArgIdentifier "Bob")
            ),
            ( "42.3)",
              AST.AttrArgUnnamed (AST.AttrArgNumber "42.3")
            ),
            ( "2 + 3,",
              AST.AttrArgUnnamed (AST.AttrArgUnknown "2 + 3")
            )
          ]
    let runTest (psl, expected) =
          it ("correctly parses " ++ psl) $ Parsec.parse modelAttrArgument "" psl `shouldBe` Right expected
    mapM_ runTest tests

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
          AST.SchemaModel $
            AST.Model
              "User"
              ( AST.Body
                  [ AST.ElementField
                      ( AST.Field
                          "id"
                          AST.Int
                          []
                          [ AST.Attribute "id" [],
                            AST.Attribute
                              "default"
                              [ AST.AttrArgNamed "value" (AST.AttrArgFunc "autoincrement")
                              ]
                          ]
                      ),
                    AST.ElementField
                      ( AST.Field "internets" (AST.UserType "Internet") [AST.List] []
                      ),
                    AST.ElementField
                      ( AST.Field "strings" (AST.UserType "Strings") [AST.List] []
                      )
                  ]
              )

    it "Model is correctly parsed" $ do
      Parsec.parse model "" (T.unpack modelPsl) `shouldBe` Right modelAst
