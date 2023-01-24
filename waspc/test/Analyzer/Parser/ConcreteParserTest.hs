{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Analyzer.Parser.ConcreteParserTest where

import Control.DeepSeq (deepseq)
import Test.Tasty.Hspec (Spec, describe, it)
import Test.Tasty.QuickCheck
import Util.Diff
import Wasp.Analyzer.Parser.CST
import Wasp.Analyzer.Parser.ConcreteParser
import Wasp.Analyzer.Parser.ConcreteParser.ParseError (errorSpan, getErrorMessage)
import qualified Wasp.Analyzer.Parser.Lexer as L
import Wasp.Analyzer.Parser.SourceSpan (SourceSpan (..))
import qualified Wasp.Analyzer.Parser.Token as T
import qualified Wasp.Analyzer.Parser.TokenSet as TokenSet

-- TODO: Do we really need all these tests now that we have "e2e" tests for parser in test/Analyzer/ParserTest/?

token :: T.TokenKind -> String -> T.Token
token kind text = T.Token {T.tokenKind = kind, T.tokenWidth = length text}

node :: SyntaxKind -> Int -> [SyntaxNode] -> SyntaxNode
node kind width children = SyntaxNode {snodeKind = kind, snodeWidth = width, snodeChildren = children}

spec_ParseCSTExpression :: Spec
spec_ParseCSTExpression = do
  it "Parses identifiers" $ do
    parseCSTExpression [token T.Identifier "foo"]
      `shouldBeWithDiff` ([], [node Var 3 []])
  it "Parses double literals" $ do
    parseCSTExpression [token T.Double "0.5"]
      `shouldBeWithDiff` ([], [node Double 3 []])
  it "Parses int literals" $ do
    parseCSTExpression [token T.Int "0.5"]
      `shouldBeWithDiff` ([], [node Int 3 []])
  it "Parses true literals" $ do
    parseCSTExpression [token T.KwTrue "true"]
      `shouldBeWithDiff` ([], [node BoolTrue 4 []])
  it "Parses false literals" $ do
    parseCSTExpression [token T.KwFalse "false"]
      `shouldBeWithDiff` ([], [node BoolFalse 5 []])
  it "Parses external imports" $ do
    parseCSTExpression [token T.KwImport "import", token T.Identifier "main", token T.KwFrom "from", token T.String "\"@server/main.js\""]
      `shouldBeWithDiff` ( [],
                           [ node
                               ExtImport
                               31
                               [ node (Token T.KwImport) 6 [],
                                 node ExtImportModule 4 [],
                                 node (Token T.KwFrom) 4 [],
                                 node ExtImportPath 17 []
                               ]
                           ]
                         )
  it "Parses quoted expressions" $ do
    parseCSTExpression [token T.LQuote "{=txt", token T.Quoted "h", token T.Quoted "e", token T.Quoted "llo", token T.RQuote "txt=}"]
      `shouldBeWithDiff` ( [],
                           [ node
                               Quoter
                               15
                               [ node (Token T.LQuote) 5 [],
                                 node (Token T.Quoted) 1 [],
                                 node (Token T.Quoted) 1 [],
                                 node (Token T.Quoted) 3 [],
                                 node (Token T.RQuote) 5 []
                               ]
                           ]
                         )

  describe "Lists" $ do
    it "Parses empty lists" $ do
      parseCSTExpression [token T.LSquare "[", token T.RSquare "]"]
        `shouldBeWithDiff` ([], [node List 2 [node (Token T.LSquare) 1 [], node (Token T.RSquare) 1 []]])
    it "Parses list with items" $ do
      parseCSTExpression [token T.LSquare "[", token T.Identifier "foo", token T.Comma ",", token T.Int "3", token T.RSquare "]"]
        `shouldBeWithDiff` ( [],
                             [ node
                                 List
                                 7
                                 [ node (Token T.LSquare) 1 [],
                                   node Var 3 [],
                                   node (Token T.Comma) 1 [],
                                   node Int 1 [],
                                   node (Token T.RSquare) 1 []
                                 ]
                             ]
                           )
    it "Parses lists with a trailing comma" $ do
      parseCSTExpression [token T.LSquare "[", token T.Int "1", token T.Comma ",", token T.RSquare "]"]
        `shouldBeWithDiff` ( [],
                             [ node
                                 List
                                 4
                                 [ node (Token T.LSquare) 1 [],
                                   node Int 1 [],
                                   node (Token T.Comma) 1 [],
                                   node (Token T.RSquare) 1 []
                                 ]
                             ]
                           )

spec_ConcreteParser :: Spec
spec_ConcreteParser =
  describe "Wasp.Backend.ConcreteParser" $ do
    it "works on a simple example" $ do
      let tokens =
            [ token T.Identifier "app",
              token T.White " ",
              token T.Identifier "Test",
              token T.White " ",
              token T.LCurly "{",
              token T.Newline "\n",
              token T.White "  ",
              token T.Comment "// this is the title of the app",
              token T.Newline "\n",
              token T.White "  ",
              token T.Identifier "title",
              token T.Colon ":",
              token T.String "\"hello world\"",
              token T.Newline "\n",
              token T.RCurly "}"
            ]
      let errors = []
      let tree =
            node
              Program
              68
              [ node
                  Decl
                  68
                  [ node DeclType 3 [],
                    node (Token T.White) 1 [],
                    node DeclName 4 [],
                    node (Token T.White) 1 [],
                    node
                      Dict
                      59
                      [ node (Token T.LCurly) 1 [],
                        node (Token T.Newline) 1 [],
                        node (Token T.White) 2 [],
                        node (Token T.Comment) 31 [],
                        node (Token T.Newline) 1 [],
                        node (Token T.White) 2 [],
                        node
                          DictEntry
                          19
                          [ node DictKey 5 [],
                            node (Token T.Colon) 1 [],
                            node String 13 []
                          ],
                        node (Token T.Newline) 1 [],
                        node (Token T.RCurly) 1 []
                      ]
                  ]
              ]
      let (actualErrors, actualTrees) = parseCST tokens
      actualTrees `shouldBeWithDiff` [tree]
      actualErrors `shouldBeWithDiff` errors

    it "works for incomplete source" $ do
      let tokens =
            [ token T.Identifier "app",
              token T.Identifier "Test",
              token T.LCurly "{",
              token T.Identifier "title",
              token T.Colon ":"
            ]
      let errors = [UnexpectedEOF 14 $ TokenSet.fromList [T.LParen, T.LCurly, T.LSquare, T.KwImport, T.KwTrue, T.KwFalse, T.String, T.Double, T.Int, T.Identifier, T.LQuote]]
      let tree =
            node
              Program
              14
              [ node
                  Decl
                  14
                  [ node DeclType 3 [],
                    node DeclName 4 [],
                    node
                      Dict
                      7
                      [ node (Token T.LCurly) 1 [],
                        node
                          DictEntry
                          6
                          [ node DictKey 5 [],
                            node (Token T.Colon) 1 [],
                            node Error 0 []
                          ]
                      ]
                  ]
              ]
      parseCST tokens `shouldBeWithDiff` (errors, [tree])

    it "works when incomplete with closing bracket" $ do
      let tokens =
            [ token T.Identifier "route",
              token T.White " ",
              token T.Identifier "TestRoute",
              token T.White " ",
              token T.LCurly "{",
              token T.Newline "\n",
              token T.White "  ",
              token T.Identifier "path",
              token T.Colon ":",
              token T.White " ",
              token T.String "\"/\"",
              token T.Comma ",",
              token T.Newline "\n",
              token T.White "  ",
              token T.Identifier "to",
              token T.Colon ":",
              token T.White " ",
              token T.Newline "\n",
              token T.RCurly "}"
            ]
      let errors =
            [ UnexpectedToken (SourceSpan 38 39) T.RCurly $ TokenSet.fromList [T.LParen, T.LCurly, T.LSquare, T.KwImport, T.KwTrue, T.KwFalse, T.String, T.Double, T.Int, T.Identifier, T.LQuote]
            ]
      let tree =
            node
              Program
              39
              [ node
                  Decl
                  39
                  [ node DeclType 5 [],
                    node (Token T.White) 1 [],
                    node DeclName 9 [],
                    node (Token T.White) 1 [],
                    node
                      Dict
                      23
                      [ node (Token T.LCurly) 1 [],
                        node (Token T.Newline) 1 [],
                        node (Token T.White) 2 [],
                        node
                          DictEntry
                          9
                          [ node DictKey 4 [],
                            node (Token T.Colon) 1 [],
                            node (Token T.White) 1 [],
                            node String 3 []
                          ],
                        node (Token T.Comma) 1 [],
                        node (Token T.Newline) 1 [],
                        node (Token T.White) 2 [],
                        node
                          DictEntry
                          5
                          [ node DictKey 2 [],
                            node (Token T.Colon) 1 [],
                            node (Token T.White) 1 [],
                            node (Token T.Newline) 1 [],
                            node Error 0 []
                          ],
                        node (Token T.RCurly) 1 []
                      ]
                  ]
              ]
      parseCST tokens `shouldBeWithDiff` (errors, [tree])

    -- DEPENDS ON: LexerTest#never fails to lex
    -- TODO: remove dependency by making arbitrary instance for Token
    --
    -- The point of this test is to ensure some sort of parse tree is always built
    it "never fails to parse" $
      property $ \source -> parseCST (L.lex source) `deepseq` True

instance Diffable SyntaxNode where
  toLines n = lines $ cstPrettyPrint n

instance Diffable ParseError where
  toLines err = ["At " ++ show (errorSpan err), "  " ++ getErrorMessage err]
