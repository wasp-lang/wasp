module Psl.Parser.ModelTest where

import Data.Either (isLeft)
import Psl.Common.ModelTest (sampleBodyAst, sampleBodySchema)
import Test.Tasty.Hspec
import qualified Text.Parsec as Parsec
import qualified Wasp.Psl.Ast.Model as AST
import Wasp.Psl.Parser.Model (attrArgument, body, model)

spec_parsePslModel :: Spec
spec_parsePslModel = do
  describe "Complex example" $ do
    let pslModel = "model User {\n" ++ sampleBodySchema ++ "\n}"
        expectedModelAst = AST.Model "User" sampleBodyAst

    it "Body parser correctly parses" $ do
      Parsec.parse body "" sampleBodySchema `shouldBe` Right sampleBodyAst

    it "Model parser correctly parses" $ do
      Parsec.parse model "" pslModel `shouldBe` Right expectedModelAst

  describe "Body parser" $ do
    describe "Fails if input is not valid PSL" $ do
      let runTest psl = it psl $ isLeft (Parsec.parse body "" psl) `shouldBe` True
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
          it ("correctly parses " ++ psl) $ Parsec.parse attrArgument "" psl `shouldBe` Right expected
    mapM_ runTest tests
