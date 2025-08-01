module Psl.Parser.AttributeTest where

import Test.Tasty.Hspec
import qualified Text.Megaparsec as Megaparsec
import qualified Wasp.Psl.Ast.Argument as Psl.Argument
import qualified Wasp.Psl.Ast.Attribute as Psl.Attribute
import Wasp.Psl.Parser.Attribute (attribute, blockAttribute)
import Wasp.Psl.Parser.Common (Parser)

spec_parseAttributePslPart :: Spec
spec_parseAttributePslPart = do
  describe "Inline attribute parser" $ do
    let tests =
          [ ( "@id",
              Psl.Attribute.Attribute "id" []
            ),
            ( "@default(autoincrement())",
              Psl.Attribute.Attribute "default" [Psl.Argument.ArgUnnamed (Psl.Argument.FuncExpr "autoincrement" [])]
            ),
            ( "@default(false)",
              Psl.Attribute.Attribute "default" [Psl.Argument.ArgUnnamed (Psl.Argument.IdentifierExpr "false")]
            ),
            ( "@default(value: false)",
              Psl.Attribute.Attribute "default" [Psl.Argument.ArgNamed "value" (Psl.Argument.IdentifierExpr "false")]
            ),
            ( "@id(name: \"fullName\", fields: [firstName, lastName])",
              Psl.Attribute.Attribute
                "id"
                [ Psl.Argument.ArgNamed "name" (Psl.Argument.StringExpr "fullName"),
                  Psl.Argument.ArgNamed
                    "fields"
                    ( Psl.Argument.ArrayExpr
                        [ Psl.Argument.IdentifierExpr "firstName",
                          Psl.Argument.IdentifierExpr "lastName"
                        ]
                    )
                ]
            ),
            ( "@db.VarChar(255)",
              Psl.Attribute.Attribute "db.VarChar" [Psl.Argument.ArgUnnamed (Psl.Argument.NumberExpr "255")]
            ),
            ( "@db.ObjectId",
              Psl.Attribute.Attribute "db.ObjectId" []
            ),
            -- Based on https://github.com/wasp-lang/wasp/issues/2137
            ( "@default(dbgenerated(\"gen_random_uuid()\"))",
              Psl.Attribute.Attribute
                "default"
                [ Psl.Argument.ArgUnnamed
                    ( Psl.Argument.FuncExpr
                        "dbgenerated"
                        [ Psl.Argument.ArgUnnamed $ Psl.Argument.StringExpr "gen_random_uuid()"
                        ]
                    )
                ]
            ),
            ( "@default([])",
              Psl.Attribute.Attribute
                "default"
                [ Psl.Argument.ArgUnnamed $ Psl.Argument.ArrayExpr []
                ]
            )
          ]
    runTestsFor attribute tests

  describe "Block attribute parser" $ do
    let tests =
          [ ( "@@id(name: \"fullName\", fields: [firstName, lastName])",
              Psl.Attribute.Attribute
                "id"
                [ Psl.Argument.ArgNamed "name" (Psl.Argument.StringExpr "fullName"),
                  Psl.Argument.ArgNamed
                    "fields"
                    ( Psl.Argument.ArrayExpr
                        [ Psl.Argument.IdentifierExpr "firstName",
                          Psl.Argument.IdentifierExpr "lastName"
                        ]
                    )
                ]
            ),
            ( "@@index([firstName,lastName])",
              Psl.Attribute.Attribute
                "index"
                [ Psl.Argument.ArgUnnamed
                    ( Psl.Argument.ArrayExpr
                        [ Psl.Argument.IdentifierExpr "firstName",
                          Psl.Argument.IdentifierExpr "lastName"
                        ]
                    )
                ]
            ),
            ( "@@unique([firstName, lastName])",
              Psl.Attribute.Attribute
                "unique"
                [ Psl.Argument.ArgUnnamed
                    ( Psl.Argument.ArrayExpr
                        [ Psl.Argument.IdentifierExpr "firstName",
                          Psl.Argument.IdentifierExpr "lastName"
                        ]
                    )
                ]
            )
          ]

    runTestsFor blockAttribute tests
  where
    runTestsFor :: Parser Psl.Attribute.Attribute -> [(String, Psl.Attribute.Attribute)] -> Spec
    runTestsFor parser tests = do
      let runTest (psl, expected) = it ("correctly parses " ++ psl) $ Megaparsec.parse parser "" psl `shouldBe` Right expected
      mapM_ runTest tests
