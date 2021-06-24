{
{-# LANGUAGE NamedFieldPuns #-}

module Analyzer.Parser.Lexer
  ( lexer
  ) where

import Analyzer.Parser.Util (AlexInput, Parser, ParseState (..), updatePosn, putInput)
import Analyzer.Parser.Syntax (ParseError (..), Token (..), TokenClass (..))
import Control.Monad.Trans.State.Lazy (get)
import Control.Monad.Trans.Except (throwE)
import Control.Monad.Trans.Class (lift)
import Data.Word (Word8)
import Codec.Binary.UTF8.String (encodeChar)
}

$digit = 0-9
$alpha = [a-zA-Z]
$identstart = [_$alpha]
$ident = [_$alpha$digit]
$any = [.$white]

@string = \" (. # \")* \"
@double = "-"? $digit+ "." $digit+
@integer = "-"? $digit+
@ident = $identstart+ $ident* "'"*

tokens :-

  $white+ ;

-- Quoters — For now a dirty method that hardcodes json and psl in lexer
-- TODO: figure out how to make this better
  "{=json" $any* "json=}" { token $ \s -> TQuoter ("json", (unquote 6 s), "json") }
  "{=psl" $any* "psl=}" { token $ \s -> TQuoter ("psl", (unquote 5 s), "psl") }

-- Simple tokens
  "{" { fromClass TLCurly }
  "}" { fromClass TRCurly }
  "," { fromClass TComma }
  ":" { fromClass TColon }
  "[" { fromClass TLSquare }
  "]" { fromClass TRSquare }
  "import" { fromClass TImport }
  "from" { fromClass TFrom }
  "true" { fromClass TTrue }
  "false" { fromClass TFalse }

-- Strings, numbers, identifiers
  @string { token $ \s -> TString (init $ tail s) }
  @double { token $ \s -> TDouble $ read s }
  @integer { token $ \s -> TInt $ read s }
  @ident { token $ \s -> TIdent s }

{

-- | Get a byte from the input.
--
--   This function is taken from the Alex basic wrapper.
alexGetByte :: AlexInput -> Maybe (Word8, AlexInput)
alexGetByte (c, (b:bs), s) = Just (b, (c, bs, s))
alexGetByte (_, [], []) = Nothing
alexGetByte (_, [], (c:s)) = case encodeChar c of
                               (b:bs) -> Just (b, (c, bs, s))
                               [] -> Nothing

-- | Get the previous character from the input.
--
--   This function is taken from the Alex basic wrapper.
alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar (c, _, _) = c

-- | Lexes a single token from the input.
--
--   This function is designed for use with the Happy threaded lexer, which
--   is why it uses a CPS-style.
lexer :: (Token -> Parser a) -> Parser a
lexer cont = do
  inp@(c, _, str) <- psInput <$> get
  case alexScan inp 0 of
    AlexEOF -> do
      fromClass TEOF "" >>= cont
    AlexError _ -> do
      pos <- psPosn <$> get
      lift $ throwE $ UnexpectedChar c pos
    AlexSkip inp' len -> do
      updatePosn str len
      putInput inp'
      lexer cont
    AlexToken inp' len act -> do
      -- Token is made before `updatePosn` so that its `tokenPosn` points to
      -- the start of the token's lexeme.
      tok <- act $ take len str
      updatePosn str len
      putInput inp'
      cont tok

-- | Removes wasp quoter beginning ending block of length `len`
unquote :: Int -> String -> String
unquote len s = let takeLen = length s - len * 2
                in  if takeLen < 0
                      then ""
                      else take takeLen $ drop len s

-- | Makes an action that creates a token from a constant TokenClass.
fromClass :: TokenClass -> (String -> Parser Token)
fromClass tc str = do
  posn <- psPosn <$> get
  return $ Token { tokenClass = tc
                 , tokenPosn = posn
                 , tokenLexeme = str
                 }

-- | Makes an action that creates a token using the input lexeme.
token :: (String -> TokenClass) -> (String -> Parser Token)
token f str = fromClass (f str) str
}
