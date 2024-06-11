module Psl.Parser.AttributeTest where

import Test.Tasty.Hspec
import qualified Text.Parsec as Parsec
import qualified Wasp.Psl.Ast.Attribute as Psl.Attribute
import Wasp.Psl.Parser.Attribute (attrArgument)

spec_parseAttributePslPart :: Spec
spec_parseAttributePslPart = do
  describe "Attribute argument parser" $ do
    let tests =
          [ ( "[foo, bar],",
              Psl.Attribute.AttrArgUnnamed (Psl.Attribute.AttrArgFieldRefList ["foo", "bar"])
            ),
            ( "\"test\")",
              Psl.Attribute.AttrArgUnnamed (Psl.Attribute.AttrArgString "test")
            ),
            ( "foo: bar(),",
              Psl.Attribute.AttrArgNamed "foo" (Psl.Attribute.AttrArgFunc "bar")
            ),
            ( "Bob,",
              Psl.Attribute.AttrArgUnnamed (Psl.Attribute.AttrArgIdentifier "Bob")
            ),
            ( "42.3)",
              Psl.Attribute.AttrArgUnnamed (Psl.Attribute.AttrArgNumber "42.3")
            ),
            ( "2 + 3,",
              Psl.Attribute.AttrArgUnnamed (Psl.Attribute.AttrArgUnknown "2 + 3")
            )
          ]
    let runTest (psl, expected) =
          it ("correctly parses " ++ psl) $ Parsec.parse attrArgument "" psl `shouldBe` Right expected
    mapM_ runTest tests
