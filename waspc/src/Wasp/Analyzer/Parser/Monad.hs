{-# LANGUAGE FlexibleContexts #-}

module Wasp.Analyzer.Parser.Monad
  ( ParserState (..),
    initialState,
    Parser,
    updatePosition,
    putInput,
    setStartCode,
    ParserInput,
    LexerStartCode (..),
  )
where

import Control.Monad.Except (Except)
import Control.Monad.State.Lazy (StateT, get, modify)
import Data.Word (Word8)
import Wasp.Analyzer.Parser.ParseError (ParseError)
import Wasp.Analyzer.Parser.Token (SourcePosition (..))

type Parser a = StateT ParserState (Except ParseError) a

updatePosition :: String -> Parser ()
updatePosition parsedSourcePiece = do
  position <- parserSourcePosition <$> get
  let position' = calcNewPosition parsedSourcePiece position
  modify $ \s -> s {parserSourcePosition = position'}
  where
    -- Scan the string character by character to look for newlines
    calcNewPosition [] position = position
    calcNewPosition ('\n' : cs) (SourcePosition line _) = calcNewPosition cs $ SourcePosition (line + 1) 1
    calcNewPosition (_ : cs) (SourcePosition line col) = calcNewPosition cs $ SourcePosition line (col + 1)

putInput :: ParserInput -> Parser ()
putInput input = modify $ \s -> s {parserRemainingInput = input}

setStartCode :: LexerStartCode -> Parser ()
setStartCode startCode = modify $ \s -> s {parserLexerStartCode = startCode}

data ParserState = ParserState
  { parserSourcePosition :: SourcePosition,
    parserRemainingInput :: ParserInput,
    parserLexerStartCode :: LexerStartCode
  }
  deriving (Show)

-- | A representation of the lexer's start code: https://www.haskell.org/alex/doc/html/alex-files.html#startcodes
data LexerStartCode
  = -- | For a start code @DefaultStartCode@, the lexer is in start code <0>
    DefaultStartCode
  | -- | For a start code @QuoterStartCode tag@, the lexer is in start code <quoter> and the opening tag was @tag@
    QuoterStartCode String
  deriving (Show)

initialState :: String -> ParserState
initialState source =
  ParserState
    { parserSourcePosition = SourcePosition 1 1,
      parserRemainingInput = ('\n', [], source),
      parserLexerStartCode = DefaultStartCode
    }

-- | The type of the input given to the parser/lexer
--
--   An input @(c, bs, str)@ represents
--   - @c@ The previous character consumed by the lexer
--   - @bs@ The UTF8 bytes of the current character being lexed
--   - @str@ The remaining input to be lexed and parsed
type ParserInput = (Char, [Word8], String)
