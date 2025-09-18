module Psl.Parser.ArgumentTest where

import Test.Tasty.Hspec
import qualified Text.Megaparsec as Megaparsec
import qualified Wasp.Psl.Ast.Argument as Psl.Argument
import Wasp.Psl.Parser.Argument (argument)

spec_parseArgumentPslPart :: Spec
spec_parseArgumentPslPart = do
  describe "Arguments parser" $ do
    let tests =
          [ ( "[foo, bar],",
              Psl.Argument.ArgUnnamed
                ( Psl.Argument.ArrayExpr
                    [ Psl.Argument.IdentifierExpr "foo",
                      Psl.Argument.IdentifierExpr "bar"
                    ]
                )
            ),
            ( "\"test\")",
              Psl.Argument.ArgUnnamed (Psl.Argument.StringExpr "test")
            ),
            ( "foo: bar()",
              Psl.Argument.ArgNamed "foo" (Psl.Argument.FuncExpr "bar" [])
            ),
            ( "Bob",
              Psl.Argument.ArgUnnamed (Psl.Argument.IdentifierExpr "Bob")
            ),
            ( "42.3",
              Psl.Argument.ArgUnnamed (Psl.Argument.NumberExpr "42.3")
            ),
            ( "[hstore(schema: \"myHstoreSchema\"), pg_trgm, postgis(version: \"2.1\")]",
              Psl.Argument.ArgUnnamed $
                Psl.Argument.ArrayExpr
                  [ Psl.Argument.FuncExpr "hstore" [Psl.Argument.ArgNamed "schema" (Psl.Argument.StringExpr "myHstoreSchema")],
                    Psl.Argument.IdentifierExpr "pg_trgm",
                    Psl.Argument.FuncExpr "postgis" [Psl.Argument.ArgNamed "version" (Psl.Argument.StringExpr "2.1")]
                  ]
            ),
            ( "[]",
              Psl.Argument.ArgUnnamed (Psl.Argument.ArrayExpr [])
            )
          ]
    let runTest (psl, expected) =
          it ("correctly parses " ++ psl) $ Megaparsec.parse argument "" psl `shouldBe` Right expected
    mapM_ runTest tests
