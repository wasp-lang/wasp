{-# OPTIONS_GHC -Wno-orphans #-}

module Wasp.Backend.LexerTest where

import Control.DeepSeq (deepseq)
import Data.List (isInfixOf)
import Test.Tasty.Hspec
import Test.Tasty.QuickCheck
import TestUtil
import qualified Wasp.Backend.Lexer as L
import Wasp.Backend.Token

token :: TokenKind -> String -> Token
token kind text = Token {tokenKind = kind, tokenWidth = length text}

spec_Lexer :: Spec
spec_Lexer = do
  it "works for a simple example" $ do
    let source = "app Test { name: \"hello world\" }\n\n\n  // this is a comment\nimport  from"
    let expected =
          [ token Identifier "app",
            token White " ",
            token Identifier "Test",
            token White " ",
            token LCurly "{",
            token White " ",
            token Identifier "name",
            token Colon ":",
            token White " ",
            token String "\"hello world\"",
            token White " ",
            token RCurly "}",
            token Newline "\n",
            token Newline "\n",
            token Newline "\n",
            token White "  ",
            token Comment "// this is a comment",
            token Newline "\n",
            token KwImport "import",
            token White "  ",
            token KwFrom "from"
          ]
    L.lex source `shouldBeWithDiff` expected

  it "only makes Quoted tokens after an LQuote and before an RQuote" $ do
    property $ \inner ->
      if "txt=}" `isInfixOf` inner
        then discard
        else case L.lex $ "{=txt " ++ inner ++ " txt=}" of
          [] -> False
          open : rest
            | tokenKind open /= LQuote -> False
            | otherwise ->
              all ((== Quoted) . tokenKind) (init rest)
                && (tokenKind (last rest) == RQuote)

  it "never fails to lex" $ do
    -- @deepseq@ is used to force evaluation of the tokens, making sure an
    -- error never occurs (there are calls to @error@ in the lexer that we
    -- need to make sure never actually happen)
    property $ \source -> L.lex source `deepseq` True

instance Diffable Token where
  toLines tok =
    [ show (tokenKind tok) ++ "[" ++ show (tokenWidth tok) ++ "]"
    ]
