module Analyzer.ParserTest where

import Analyzer.TestUtil
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
                "  ident: Wasp,",
                "  innerDict: { innerDictReal: 2.17 }",
                "}"
              ]
      let ast =
            AST
              [ wctx 1 1 $
                  Decl "test" "Decl" $
                    wctx 1 11 $
                      Dict
                        [ ("string", wctx 2 11 $ StringLiteral "Hello Wasp =}"),
                          ("escapedString", wctx 3 18 $ StringLiteral "Look, a \""),
                          ("integer", wctx 4 12 $ IntegerLiteral 42),
                          ("real", wctx 5 9 $ DoubleLiteral 3.14),
                          ("yes", wctx 6 8 $ BoolLiteral True),
                          ("no", wctx 7 7 $ BoolLiteral False),
                          ("ident", wctx 8 10 $ Var "Wasp"),
                          ( "innerDict",
                            wctx 9 14 $
                              Dict
                                [ ("innerDictReal", wctx 9 31 $ DoubleLiteral 2.17)
                                ]
                          )
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
              [ wctx 1 1 $
                  Decl "test" "Imports" $
                    wctx 1 14 $
                      Dict
                        [ ("module", wctx 2 11 $ ExtImport (ExtImportModule "Page") "page.jsx"),
                          ("field", wctx 3 10 $ ExtImport (ExtImportField "Page") "page.jsx")
                        ]
              ]
      parse source `shouldBe` Right ast

    it "Parses unary lists" $ do
      let source = "test Decl [ 1 ]"
      let ast = AST [wctx 1 1 $ Decl "test" "Decl" $ wctx 1 11 $ List [wctx 1 13 $ IntegerLiteral 1]]
      parse source `shouldBe` Right ast

    it "Parses lists of multiple elements" $ do
      let source = "test Decl [ 1, 2, 3 ]"
      let ast =
            AST
              [ wctx 1 1 $
                  Decl "test" "Decl" $
                    wctx 1 11 $
                      List
                        [ wctx 1 13 $ IntegerLiteral 1,
                          wctx 1 16 $ IntegerLiteral 2,
                          wctx 1 19 $ IntegerLiteral 3
                        ]
              ]
      parse source `shouldBe` Right ast

    it "Parses empty dictionaries and lists" $ do
      let source = "test Decl { dict: {}, list: [] }"
      let ast =
            AST
              [ wctx 1 1 $
                  Decl "test" "Decl" $
                    wctx 1 11 $
                      Dict
                        [ ("dict", wctx 1 19 $ Dict []),
                          ("list", wctx 1 29 $ List [])
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
              [ wctx 1 1 $
                  Decl "test" "Decl" $
                    wctx 1 11 $
                      Dict
                        [("list", wctx 2 9 $ List [wctx 2 11 $ IntegerLiteral 1])]
              ]
      parse source `shouldBe` Right ast

    it "Parses tuples" $ do
      let source =
            unlines
              [ "test Pair (1, \"foo\")",
                "test Triple (1, \"foo\", 2)",
                "test Quadruple (1, \"foo\", 2, true)",
                "test TrailingComma (42, 314,)"
              ]
      let ast =
            AST
              [ wctx 1 1 $
                  Decl "test" "Pair" $
                    wctx 1 11 $
                      Tuple
                        ( wctx 1 12 $ IntegerLiteral 1,
                          wctx 1 15 $ StringLiteral "foo",
                          []
                        ),
                wctx 2 1 $
                  Decl "test" "Triple" $
                    wctx 2 13 $
                      Tuple
                        ( wctx 2 14 $ IntegerLiteral 1,
                          wctx 2 17 $ StringLiteral "foo",
                          [wctx 2 24 $ IntegerLiteral 2]
                        ),
                wctx 3 1 $
                  Decl "test" "Quadruple" $
                    wctx 3 16 $
                      Tuple
                        ( wctx 3 17 $ IntegerLiteral 1,
                          wctx 3 20 $ StringLiteral "foo",
                          [ wctx 3 27 $ IntegerLiteral 2,
                            wctx 3 30 $ BoolLiteral True
                          ]
                        ),
                wctx 4 1 $
                  Decl "test" "TrailingComma" $
                    wctx 4 20 $
                      Tuple
                        ( wctx 4 21 $ IntegerLiteral 42,
                          wctx 4 25 $ IntegerLiteral 314,
                          []
                        )
              ]
      parse source `shouldBe` Right ast

    it "Parses quoted PSL" $ do
      let source =
            unlines
              [ "test PSL {=psl",
                "  id Int @id",
                "psl=}"
              ]
      let ast =
            AST
              [ wctx 1 1 $
                  Decl "test" "PSL" $
                    wctx 1 10 $
                      Quoter "psl" "\n  id Int @id\n"
              ]
      parse source `shouldBe` Right ast

    it "Parses quoted JSON" $ do
      let source =
            unlines
              [ "test JSON {=json",
                "  \"key\": \"value\"",
                "json=}"
              ]
      let ast =
            AST
              [ wctx 1 1 $
                  Decl "test" "JSON" $
                    wctx 1 11 $ Quoter "json" "\n  \"key\": \"value\"\n"
              ]
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
              [ wctx 1 1 $ Decl "test" "JSON" $ wctx 1 11 $ Quoter "json" "\n  \"key\": \"value\"\n",
                wctx 4 1 $ Decl "test" "JSON2" $ wctx 4 12 $ Quoter "json" "\n  \"key\": \"value\"\n"
              ]
      parse source `shouldBe` Right ast

    it "Fails to parse a quoter with unmatched tags" $ do
      let source = "test Failure {=a b=}"
      parse source `shouldSatisfy` isLeft

    it "Parses nested quoters correctly" $ do
      parse "test Case1 {=foo {=foo foo=} foo=}" `shouldSatisfy` isLeft
      parse "test Case2 {=foo foo=} foo=}" `shouldSatisfy` isLeft
      parse "test Case3 {=foo {=foo foo=}"
        `shouldBe` Right (AST [wctx 1 1 $ Decl "test" "Case3" $ wctx 1 12 $ Quoter "foo" " {=foo "])
      parse "test Case4 {=foo {=bar foo=}"
        `shouldBe` Right (AST [wctx 1 1 $ Decl "test" "Case4" $ wctx 1 12 $ Quoter "foo" " {=bar "])
      parse "test Case5 {=foo bar=} foo=}"
        `shouldBe` Right (AST [wctx 1 1 $ Decl "test" "Case5" $ wctx 1 12 $ Quoter "foo" " bar=} "])
      parse "test Case6 {=foo {=bar bar=} foo=}"
        `shouldBe` Right (AST [wctx 1 1 $ Decl "test" "Case6" $ wctx 1 12 $ Quoter "foo" " {=bar bar=} "])

    it "Requires dictionaries to have an ending bracket" $ do
      let source = "test Decl {"
      let expected =
            Left $
              UnexpectedToken
                ( Token
                    { tokenType = TEOF,
                      tokenPosition = SourcePosition 1 12,
                      tokenLexeme = ""
                    }
                )
                ["}", "<identifier>"]
      parse source `shouldBe` expected

    it "Parses multiple statements" $ do
      let source =
            unlines
              [ "constant Pi 3.14159",
                "constant E  2.71828"
              ]
      let ast =
            AST
              [ wctx 1 1 $ Decl "constant" "Pi" $ wctx 1 13 $ DoubleLiteral 3.14159,
                wctx 2 1 $ Decl "constant" "E" $ wctx 2 13 $ DoubleLiteral 2.71828
              ]
      parse source `shouldBe` Right ast

    describe "Fails with UnexpectedChar error if unrecognized character is encountered" $ do
      it "e.g. when it encounters '^' after declaration name" $ do
        let source = "test Decl ^ {}"
        let expected = Left $ UnexpectedChar '^' $ SourcePosition 1 11
        parse source `shouldBe` expected

      it "e.g. when the identifier contains '!'" $ do
        let source = "test De!cl {}"
        let expected = Left $ UnexpectedChar '!' $ SourcePosition 1 8
        parse source `shouldBe` expected

    describe "Fails with ParseError error if unexpected token is encountered" $ do
      it "When string follows identifier" $ do
        let source = "test \"Declaration\" {}"
        let expected =
              Left $
                UnexpectedToken
                  ( Token
                      { tokenType = TString "Declaration",
                        tokenPosition = SourcePosition 1 6,
                        tokenLexeme = "\"Declaration\""
                      }
                  )
                  ["<identifier>"]
        parse source `shouldBe` expected

      it "When dictionary is missing a comma between the two fields" $ do
        let source =
              unlines
                [ "test Declaration {",
                  "  a: 1",
                  "  b: 2 ",
                  "}"
                ]
        let expected =
              Left $
                UnexpectedToken
                  ( Token
                      { tokenType = TIdentifier "b",
                        tokenPosition = SourcePosition 3 3,
                        tokenLexeme = "b"
                      }
                  )
                  ["}", ","]
        parse source `shouldBe` expected
