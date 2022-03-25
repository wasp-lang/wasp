module Analyzer.ParserTest where

import Analyzer.TestUtil
import Data.Either (isLeft)
import Test.Tasty.Hspec
import Wasp.Analyzer.Parser

spec_IsValidWaspIdentifier :: Spec
spec_IsValidWaspIdentifier = do
  it "Correctly identifies valid identifiers" $ do
    all isValidWaspIdentifier ["foo", "foo12", "_12", "_"]
  it "Correctly identifies invalid identifiers" $ do
    not (any isValidWaspIdentifier ["true", "12", "12_", "1foo"])

spec_ParseExpression :: Spec
spec_ParseExpression = do
  it "Parses identifiers" $ do
    parseExpression "foo" `shouldBe` Right (Var "foo")
    parseExpression "foo12" `shouldBe` Right (Var "foo12")
    parseExpression "_" `shouldBe` Right (Var "_")
    parseExpression "_12" `shouldBe` Right (Var "_12")
  it "Parses double literals" $ do
    parseExpression "0.5" `shouldBe` Right (DoubleLiteral 0.5)
  it "Parses int literals" $ do
    parseExpression "5" `shouldBe` Right (IntegerLiteral 5)

spec_ParseStatements :: Spec
spec_ParseStatements = do
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
              "  // This is a comment",
              "  innerDict: { innerDictReal: 2.17 }",
              "}"
            ]
    let ast =
          AST
            [ wctx (1, 1) (11, 1) $
                Decl "test" "Decl" $
                  wctx (1, 11) (11, 1) $
                    Dict
                      [ ("string", wctx (2, 11) (2, 25) $ StringLiteral "Hello Wasp =}"),
                        ("escapedString", wctx (3, 18) (3, 29) $ StringLiteral "Look, a \""),
                        ("integer", wctx (4, 12) (4, 13) $ IntegerLiteral 42),
                        ("real", wctx (5, 9) (5, 12) $ DoubleLiteral 3.14),
                        ("yes", wctx (6, 8) (6, 11) $ BoolLiteral True),
                        ("no", wctx (7, 7) (7, 11) $ BoolLiteral False),
                        ("ident", wctx (8, 10) (8, 13) $ Var "Wasp"),
                        ( "innerDict",
                          wctx (10, 14) (10, 36) $
                            Dict
                              [ ("innerDictReal", wctx (10, 31) (10, 34) $ DoubleLiteral 2.17)
                              ]
                        )
                      ]
            ]
    parseStatements source `shouldBe` Right ast

  it "Parses comments" $ do
    let source =
          unlines
            [ "  // This is some // comment",
              "/* comment",
              "  span//ning",
              "multi/*ple lines */",
              "test /* *hi* */ Decl 42  // One more comment",
              "// And here is final comment"
            ]
    let ast =
          AST
            [ wctx (5, 1) (5, 23) $
                Decl "test" "Decl" $ wctx (5, 22) (5, 23) $ IntegerLiteral 42
            ]
    parseStatements source `shouldBe` Right ast

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
            [ wctx (1, 1) (4, 1) $
                Decl "test" "Imports" $
                  wctx (1, 14) (4, 1) $
                    Dict
                      [ ("module", wctx (2, 11) (2, 37) $ ExtImport (ExtImportModule "Page") "page.jsx"),
                        ("field", wctx (3, 10) (3, 40) $ ExtImport (ExtImportField "Page") "page.jsx")
                      ]
            ]
    parseStatements source `shouldBe` Right ast

  it "Parses unary lists" $ do
    let source = "test Decl [ 1 ]"
    let ast =
          AST
            [ wctx (1, 1) (1, 15) $
                Decl "test" "Decl" $ wctx (1, 11) (1, 15) $ List [wctx (1, 13) (1, 13) $ IntegerLiteral 1]
            ]
    parseStatements source `shouldBe` Right ast

  it "Parses lists of multiple elements" $ do
    let source = "test Decl [ 1, 2, 3 ]"
    let ast =
          AST
            [ wctx (1, 1) (1, 21) $
                Decl "test" "Decl" $
                  wctx (1, 11) (1, 21) $
                    List
                      [ wctx (1, 13) (1, 13) $ IntegerLiteral 1,
                        wctx (1, 16) (1, 16) $ IntegerLiteral 2,
                        wctx (1, 19) (1, 19) $ IntegerLiteral 3
                      ]
            ]
    parseStatements source `shouldBe` Right ast

  it "Parses empty dictionaries and lists" $ do
    let source = "test Decl { dict: {}, list: [] }"
    let ast =
          AST
            [ wctx (1, 1) (1, 32) $
                Decl "test" "Decl" $
                  wctx (1, 11) (1, 32) $
                    Dict
                      [ ("dict", wctx (1, 19) (1, 20) $ Dict []),
                        ("list", wctx (1, 29) (1, 30) $ List [])
                      ]
            ]
    parseStatements source `shouldBe` Right ast

  it "Allows trailing commas in lists and dictionaries" $ do
    let source =
          unlines
            [ "test Decl {",
              "  list: [ 1, ],",
              "}"
            ]
    let ast =
          AST
            [ wctx (1, 1) (3, 1) $
                Decl "test" "Decl" $
                  wctx (1, 11) (3, 1) $
                    Dict
                      [("list", wctx (2, 9) (2, 14) $ List [wctx (2, 11) (2, 11) $ IntegerLiteral 1])]
            ]
    parseStatements source `shouldBe` Right ast

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
            [ wctx (1, 1) (1, 20) $
                Decl "test" "Pair" $
                  wctx (1, 11) (1, 20) $
                    Tuple
                      ( wctx (1, 12) (1, 12) $ IntegerLiteral 1,
                        wctx (1, 15) (1, 19) $ StringLiteral "foo",
                        []
                      ),
              wctx (2, 1) (2, 25) $
                Decl "test" "Triple" $
                  wctx (2, 13) (2, 25) $
                    Tuple
                      ( wctx (2, 14) (2, 14) $ IntegerLiteral 1,
                        wctx (2, 17) (2, 21) $ StringLiteral "foo",
                        [wctx (2, 24) (2, 24) $ IntegerLiteral 2]
                      ),
              wctx (3, 1) (3, 34) $
                Decl "test" "Quadruple" $
                  wctx (3, 16) (3, 34) $
                    Tuple
                      ( wctx (3, 17) (3, 17) $ IntegerLiteral 1,
                        wctx (3, 20) (3, 24) $ StringLiteral "foo",
                        [ wctx (3, 27) (3, 27) $ IntegerLiteral 2,
                          wctx (3, 30) (3, 33) $ BoolLiteral True
                        ]
                      ),
              wctx (4, 1) (4, 29) $
                Decl "test" "TrailingComma" $
                  wctx (4, 20) (4, 29) $
                    Tuple
                      ( wctx (4, 21) (4, 22) $ IntegerLiteral 42,
                        wctx (4, 25) (4, 27) $ IntegerLiteral 314,
                        []
                      )
            ]
    parseStatements source `shouldBe` Right ast

  it "Parses quoted PSL" $ do
    let source =
          unlines
            [ "test PSL {=psl",
              "  id Int @id",
              "psl=}"
            ]
    let ast =
          AST
            [ wctx (1, 1) (3, 5) $
                Decl "test" "PSL" $
                  wctx (1, 10) (3, 5) $
                    Quoter "psl" "\n  id Int @id\n"
            ]
    parseStatements source `shouldBe` Right ast

  it "Parses quoted JSON" $ do
    let source =
          unlines
            [ "test JSON {=json",
              "  \"key\": \"value\"",
              "json=}"
            ]
    let ast =
          AST
            [ wctx (1, 1) (3, 6) $
                Decl "test" "JSON" $
                  wctx (1, 11) (3, 6) $ Quoter "json" "\n  \"key\": \"value\"\n"
            ]
    parseStatements source `shouldBe` Right ast

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
            [ wctx (1, 1) (3, 6) $ Decl "test" "JSON" $ wctx (1, 11) (3, 6) $ Quoter "json" "\n  \"key\": \"value\"\n",
              wctx (4, 1) (6, 6) $ Decl "test" "JSON2" $ wctx (4, 12) (6, 6) $ Quoter "json" "\n  \"key\": \"value\"\n"
            ]
    parseStatements source `shouldBe` Right ast

  it "Fails to parseStatements a quoter with unmatched tags" $ do
    let source = "test Failure {=a b=}"
    parseStatements source `shouldSatisfy` isLeft

  it "Parses nested quoters correctly" $ do
    parseStatements "test Case1 {=foo {=foo foo=} foo=}" `shouldSatisfy` isLeft
    parseStatements "test Case2 {=foo foo=} foo=}" `shouldSatisfy` isLeft
    parseStatements "test Case3 {=foo {=foo foo=}"
      `shouldBe` Right (AST [wctx (1, 1) (1, 28) $ Decl "test" "Case3" $ wctx (1, 12) (1, 28) $ Quoter "foo" " {=foo "])
    parseStatements "test Case4 {=foo {=bar foo=}"
      `shouldBe` Right (AST [wctx (1, 1) (1, 28) $ Decl "test" "Case4" $ wctx (1, 12) (1, 28) $ Quoter "foo" " {=bar "])
    parseStatements "test Case5 {=foo bar=} foo=}"
      `shouldBe` Right (AST [wctx (1, 1) (1, 28) $ Decl "test" "Case5" $ wctx (1, 12) (1, 28) $ Quoter "foo" " bar=} "])
    parseStatements "test Case6 {=foo {=bar bar=} foo=}"
      `shouldBe` Right (AST [wctx (1, 1) (1, 34) $ Decl "test" "Case6" $ wctx (1, 12) (1, 34) $ Quoter "foo" " {=bar bar=} "])

  it "Requires dictionaries to have an ending bracket" $ do
    let source = "test Decl {"
    let expected =
          Left $
            UnexpectedToken
              ( Token
                  { tokenType = TEOF,
                    tokenStartPosition = SourcePosition 1 12,
                    tokenLexeme = ""
                  }
              )
              ["}", "<identifier>"]
    parseStatements source `shouldBe` expected

  it "Parses multiple statements" $ do
    let source =
          unlines
            [ "constant Pi 3.14159",
              "constant E  2.71828"
            ]
    let ast =
          AST
            [ wctx (1, 1) (1, 19) $ Decl "constant" "Pi" $ wctx (1, 13) (1, 19) $ DoubleLiteral 3.14159,
              wctx (2, 1) (2, 19) $ Decl "constant" "E" $ wctx (2, 13) (2, 19) $ DoubleLiteral 2.71828
            ]
    parseStatements source `shouldBe` Right ast

  describe "Fails with UnexpectedChar error if unrecognized character is encountered" $ do
    it "e.g. when it encounters '^' after declaration name" $ do
      let source = "test Decl ^ {}"
      let expected = Left $ UnexpectedChar '^' $ SourcePosition 1 11
      parseStatements source `shouldBe` expected

    it "e.g. when the identifier contains '!'" $ do
      let source = "test De!cl {}"
      let expected = Left $ UnexpectedChar '!' $ SourcePosition 1 8
      parseStatements source `shouldBe` expected

  describe "Fails with ParseError error if unexpected token is encountered" $ do
    it "When string follows identifier" $ do
      let source = "test \"Declaration\" {}"
      let expected =
            Left $
              UnexpectedToken
                ( Token
                    { tokenType = TString "Declaration",
                      tokenStartPosition = SourcePosition 1 6,
                      tokenLexeme = "\"Declaration\""
                    }
                )
                ["<identifier>"]
      parseStatements source `shouldBe` expected

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
                      tokenStartPosition = SourcePosition 3 3,
                      tokenLexeme = "b"
                    }
                )
                ["}", ","]
      parseStatements source `shouldBe` expected
