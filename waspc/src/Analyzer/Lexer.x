{
{-# LANGUAGE NamedFieldPuns #-}

module Analyzer.Lexer
  ( lexer
  ) where

import Analyzer.ParserUtil (AlexInput, Parser, ParseState (..), updatePosn, putInput)
import Analyzer.Syntax (ParseError (..), Token (..))
import Control.Monad.Trans.State.Lazy (get)
import Control.Monad.Trans.Except (throwE)
import Control.Monad.Trans.Class (lift)
import Data.Word (Word8)
import Data.Char (ord)
import qualified Data.Bits
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
  "{=json" $any* "json=}" { \s -> TQuoter ("json", (unquote 6 s), "json") }
  "{=psl" $any* "psl=}" { \s -> TQuoter ("psl", (unquote 5 s), "psl") }

-- Simple tokens
  "{" { const TLCurly }
  "}" { const TRCurly }
  "," { const TComma }
  ":" { const TColon }
  "[" { const TLSquare }
  "]" { const TRSquare }
  "import" { const TImport }
  "from" { const TFrom }
  "true" { const TTrue }
  "false" { const TFalse }

-- Strings, numbers, identifiers
  @string { \s -> TString (init $ tail s) }
  @double { \s -> TDouble $ read s }
  @integer { \s -> TInt $ read s }
  @ident { \s -> TIdent s }

{

alexGetByte :: AlexInput -> Maybe (Word8, AlexInput)
alexGetByte (c, (b:bs), s) = Just (b, (c, bs, s))
alexGetByte (c, [], []) = Nothing
alexGetByte (_, [], (c:s)) = case encodeChar c of
                               (b:bs) -> Just (b, (c, bs, s))

alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar (c, _, _) = c

lexer :: (Token -> Parser a) -> Parser a
lexer cont = do
  inp@(c, _, str) <- psInput <$> get
  case alexScan inp 0 of
    AlexEOF -> cont TEOF
    AlexError e -> do
      pos <- psPosn <$> get
      lift $ throwE $ UnexpectedChar c pos
    AlexSkip inp' len -> do
      updatePosn str len
      putInput inp'
      lexer cont
    AlexToken inp' len act -> do
      updatePosn str len
      putInput inp'
      cont $ act (take len str)

-- | Removes wasp quoter beginning ending block of length `len`
unquote :: Int -> String -> String
unquote len s = let takeLen = length s - len * 2
                in  if takeLen < 0
                      then ""
                      else take takeLen $ drop len s
}
