module Analyzer.ParserTest where

import Data.Either (isLeft)
import Test.Tasty.Hspec
import Wasp.Analyzer.Parser

spec_Parser :: Spec
spec_Parser = do
  describe "Analyzer.Parser" $ do
    it "Parses decls, dicts, and literals" $ do
      let source =
            unlines
              [ "test Decl {",
                "  string: \"Hello Wasp =}\",",
                "  escapedString: \"Look, a \\\"\",",
                "  integer: 42,",
                "  real: 3.14,",
                "  yes: true,",
                "  no: false,",
                "  ident: Wasp",
                "}"
              ]
      let ast =
            AST
              [ Decl "test" "Decl" $
                  Dict
                    [ ("string", StringLiteral "Hello Wasp =}"),
                      ("escapedString", StringLiteral "Look, a \""),
                      ("integer", IntegerLiteral 42),
                      ("real", DoubleLiteral 3.14),
                      ("yes", BoolLiteral True),
                      ("no", BoolLiteral False),
                      ("ident", Var "Wasp")
                    ]
              ]
      parse source `shouldBe` Right ast

    it "Parses external imports" $ do
      let source =
            unlines
              [ "test Imports {",
                "  module: import Page from \"page.jsx\",",
                "  field: import { Page } from \"page.jsx\"",
                "}"
              ]
      let ast =
            AST
              [ Decl "test" "Imports" $
                  Dict
                    [ ("module", ExtImport (ExtImportModule "Page") "page.jsx"),
                      ("field", ExtImport (ExtImportField "Page") "page.jsx")
                    ]
              ]
      parse source `shouldBe` Right ast

    it "Parses unary lists" $ do
      let source = "test Decl [ 1 ]"
      let ast = AST [Decl "test" "Decl" $ List [IntegerLiteral 1]]
      parse source `shouldBe` Right ast

    it "Parses lists of multiple elements" $ do
      let source = "test Decl [ 1, 2, 3 ]"
      let ast = AST [Decl "test" "Decl" $ List [IntegerLiteral 1, IntegerLiteral 2, IntegerLiteral 3]]
      parse source `shouldBe` Right ast

    it "Parses empty dictionaries and lists" $ do
      let source = "test Decl { dict: {}, list: [] }"
      let ast =
            AST
              [ Decl "test" "Decl" $
                  Dict
                    [ ("dict", Dict []),
                      ("list", List [])
                    ]
              ]
      parse source `shouldBe` Right ast

    it "Allows trailing commas in lists and dictionaries" $ do
      let source =
            unlines
              [ "test Decl {",
                "  list: [ 1, ],",
                "}"
              ]
      let ast =
            AST
              [ Decl "test" "Decl" $
                  Dict
                    [("list", List [IntegerLiteral 1])]
              ]
      parse source `shouldBe` Right ast

    it "Parses quoted PSL" $ do
      let source =
            unlines
              [ "test PSL {=psl",
                "  id Int @id",
                "psl=}"
              ]
      let ast = AST [Decl "test" "PSL" $ Quoter "psl" "\n  id Int @id\n"]
      parse source `shouldBe` Right ast

    it "Parses quoted JSON" $ do
      let source =
            unlines
              [ "test JSON {=json",
                "  \"key\": \"value\"",
                "json=}"
              ]
      let ast = AST [Decl "test" "JSON" $ Quoter "json" "\n  \"key\": \"value\"\n"]
      parse source `shouldBe` Right ast

    it "Parses multiple quoters" $ do
      let source =
            unlines
              [ "test JSON {=json",
                "  \"key\": \"value\"",
                "json=}",
                "test JSON2 {=json",
                "  \"key\": \"value\"",
                "json=}"
              ]
      let ast =
            AST
              [ Decl "test" "JSON" $ Quoter "json" "\n  \"key\": \"value\"\n",
                Decl "test" "JSON2" $ Quoter "json" "\n  \"key\": \"value\"\n"
              ]
      parse source `shouldBe` Right ast

    it "Fails to parse a quoter with unmatched tags" $ do
      let source = "test Failure {=a b=}"
      parse source `shouldSatisfy` isLeft

    it "Parses nested quoters correctly" $ do
      parse "test Case1 {=foo {=foo foo=} foo=}" `shouldSatisfy` isLeft
      parse "test Case2 {=foo foo=} foo=}" `shouldSatisfy` isLeft
      parse "test Case3 {=foo {=foo foo=}"
        `shouldBe` Right (AST [Decl "test" "Case3" $ Quoter "foo" " {=foo "])
      parse "test Case4 {=foo {=bar foo=}"
        `shouldBe` Right (AST [Decl "test" "Case4" $ Quoter "foo" " {=bar "])
      parse "test Case5 {=foo bar=} foo=}"
        `shouldBe` Right (AST [Decl "test" "Case5" $ Quoter "foo" " bar=} "])
      parse "test Case6 {=foo {=bar bar=} foo=}"
        `shouldBe` Right (AST [Decl "test" "Case6" $ Quoter "foo" " {=bar bar=} "])

    it "Requires dictionaries to have an ending bracket" $ do
      let source = "test Decl {"
      let expected =
            Left $
              ParseError $
                Token
                  { tokenType = TEOF,
                    tokenPosition = SourcePosition 1 12,
                    tokenLexeme = ""
                  }
      parse source `shouldBe` expected

    it "Parses multiple statements" $ do
      let source =
            unlines
              [ "constant Pi 3.14159",
                "constant E  2.71828"
              ]
      let ast =
            AST
              [ Decl "constant" "Pi" $ DoubleLiteral 3.14159,
                Decl "constant" "E" $ DoubleLiteral 2.71828
              ]
      parse source `shouldBe` Right ast
