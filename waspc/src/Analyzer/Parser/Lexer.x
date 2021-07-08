{
-- This file is processed by Alex (https://www.haskell.org/alex/) and generates
-- the module `Analyzer.Parser.Lexer`

{-# LANGUAGE NamedFieldPuns #-}

module Analyzer.Parser.Lexer
  ( lexer
  ) where

import Analyzer.Parser.Util (ParserInput, Parser, ParserState (..), updatePosition, putInput, setStartCode)
import Analyzer.Parser.Token (Token (..), TokenClass (..))
import Analyzer.Parser.ParseError (ParseError (..))
import Control.Monad.Trans.State.Lazy (get)
import Control.Monad.Trans.Except (throwE)
import Control.Monad.Trans.Class (lift)
import Data.Word (Word8)
import Codec.Binary.UTF8.String (encodeChar)

import Debug.Trace
}

-- Character set aliases
$digit = 0-9
$alpha = [a-zA-Z]
$identstart = [_$alpha]
$ident = [_$alpha$digit]
$any = [.$white]

-- Regular expression aliases
@string = \"([^\\\"]|\\.)*\" -- matches string-literal on a single line, from https://stackoverflow.com/a/9260547/3902376
@double = "-"? $digit+ "." $digit+
@integer = "-"? $digit+
@ident = $identstart $ident* "'"*

tokens :-

-- Skips whitespace
<0>       $white+ ;

-- Quoter rules
<0>       "{=" @ident { beginQuoter }
<quoter>  @ident "=}" { endQuoter }
<quoter>  $any { createValueToken TQuoted }

-- Simple tokens
<0>       "{" { createConstToken TLCurly }
<0>       "}" { createConstToken TRCurly }
<0>       "," { createConstToken TComma }
<0>       ":" { createConstToken TColon }
<0>       "[" { createConstToken TLSquare }
<0>       "]" { createConstToken TRSquare }
<0>       "import" { createConstToken TImport }
<0>       "from" { createConstToken TFrom }
<0>       "true" { createConstToken TTrue }
<0>       "false" { createConstToken TFalse }

-- Strings, numbers, identifiers
<0>       @string { createValueToken $ \s -> TString $ read s }
<0>       @double { createValueToken $ \s -> TDouble $ read s }
<0>       @integer { createValueToken $ \s -> TInt $ read s }
<0>       @ident { createValueToken $ \s -> TIdentifier s }

{

-- Alex needs the input type to be called "AlexInput"
type AlexInput = ParserInput

-- | Required for Alex to produce correct code.
--
--   This function is taken from the Alex basic wrapper.
alexGetByte :: AlexInput -> Maybe (Word8, AlexInput)
alexGetByte (c, (b:bs), s) = Just (b, (c, bs, s))
alexGetByte (_, [], []) = Nothing
alexGetByte (_, [], (c:s)) = case encodeChar c of
                               (b:bs) -> Just (b, (c, bs, s))
                               [] -> Nothing

-- | Required for Alex to produce correct code.
--
--   This function is taken from the Alex basic wrapper.
alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar (c, _, _) = c

-- | Lexes a single token from the input.
--
--   This function is designed for use with the Happy threaded lexer, which
--   requests a single token at a time from the lexer, so the Parser monad is
--   also used here.
lexer :: (Token -> Parser a) -> Parser a
lexer parseToken = do
  input@(c, _, str) <- parserRemainingInput <$> get
  startCode <- parserStartCode <$> get
  case alexScan input startCode of
    AlexEOF -> do
      createConstToken TEOF "" >>= parseToken
    AlexError _ -> do
      pos <- parserSourcePosition <$> get
      trace (show startCode) $ pure ()
      lift $ throwE $ UnexpectedChar c pos
    AlexSkip input' len -> do
      updatePosition $ take len str
      putInput input'
      lexer parseToken
    AlexToken input' len action -> do
      -- Token is made before `updatePosition` so that its `tokenPosition` points to
      -- the start of the token's lexeme.
      tok <- action $ take len str
      updatePosition $ take len str
      putInput input'
      parseToken tok

-- | Takes a lexeme like "{=json" and sets the quoter start code
beginQuoter :: String -> Parser Token
beginQuoter leftQuoteTag = do
  setStartCode quoter
  let tag = drop 2 leftQuoteTag
  createConstToken (TLQuote tag) leftQuoteTag

-- | Takes a lexeme like "json=}" and returns to start code 0
endQuoter :: String -> Parser Token
endQuoter rightQuoteTag = do
  setStartCode 0
  let tag = take (length rightQuoteTag - 2) rightQuoteTag
  createConstToken (TRQuote tag) rightQuoteTag

-- | Makes an action that creates a token from a constant TokenClass.
createConstToken :: TokenClass -> (String -> Parser Token)
createConstToken tc str = do
  position <- parserSourcePosition <$> get
  return $ Token { tokenClass = tc
                 , tokenPosition = position
                 , tokenLexeme = str
                 }

-- | Makes an action that creates a token using the input lexeme.
createValueToken :: (String -> TokenClass) -> (String -> Parser Token)
createValueToken f str = createConstToken (f str) str
}
