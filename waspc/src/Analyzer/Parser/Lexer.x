{
-- This file is processed by Alex (https://www.haskell.org/alex/) and generates
-- the module `Analyzer.Parser.Lexer`

{-# LANGUAGE NamedFieldPuns #-}

module Analyzer.Parser.Lexer
  ( lexer
  ) where

import Analyzer.Parser.Util (ParserInput, Parser, ParserState (..), updatePosition, putInput)
import Analyzer.Parser.Token (Token (..), TokenClass (..))
import Analyzer.Parser.ParseError (ParseError (..))
import Control.Monad.Trans.State.Lazy (get)
import Control.Monad.Trans.Except (throwE)
import Control.Monad.Trans.Class (lift)
import Data.Word (Word8)
import Codec.Binary.UTF8.String (encodeChar)
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
  $white+ ;

-- Quoters — For now a dirty method that hardcodes json and psl in lexer
-- TODO: figure out how to make this better
  "{=json" $any* "json=}" { createValueToken $ \s -> TQuoter ("json", (unquote 6 s)) }
  "{=psl" $any* "psl=}" { createValueToken $ \s -> TQuoter ("psl", (unquote 5 s)) }

-- Simple tokens
  "{" { createConstToken TLCurly }
  "}" { createConstToken TRCurly }
  "," { createConstToken TComma }
  ":" { createConstToken TColon }
  "[" { createConstToken TLSquare }
  "]" { createConstToken TRSquare }
  "import" { createConstToken TImport }
  "from" { createConstToken TFrom }
  "true" { createConstToken TTrue }
  "false" { createConstToken TFalse }

-- Strings, numbers, identifiers
  @string { createValueToken $ \s -> TString $ read s }
  @double { createValueToken $ \s -> TDouble $ read s }
  @integer { createValueToken $ \s -> TInt $ read s }
  @ident { createValueToken $ \s -> TIdentifier s }

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
  case alexScan input 0 of
    AlexEOF -> do
      createConstToken TEOF "" >>= parseToken
    AlexError _ -> do
      pos <- parserSourcePosition <$> get
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

-- | Removes wasp quoter beginning and ending block of length `len`
unquote :: Int -> String -> String
unquote len s = let takeLen = length s - len * 2
                in  if takeLen < 0
                      then ""
                      else take takeLen $ drop len s

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
