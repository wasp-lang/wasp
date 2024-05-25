module Psl.Parser.CommonTest where

import Test.Tasty.Hspec
import qualified Text.Parsec as Parsec
import qualified Wasp.Psl.Ast.Schema as AST
import Wasp.Psl.Parser.Common (attrArgument)

spec_parseCommonPslPart :: Spec
spec_parseCommonPslPart = do
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
