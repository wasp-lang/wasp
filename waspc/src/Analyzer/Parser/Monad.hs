{-# LANGUAGE FlexibleContexts #-}

module Analyzer.Parser.Monad
  ( ParserState (..),
    initialState,
    Parser,
    updatePosition,
    putInput,
    setStartCode,
    ParserInput,
  )
where

import Analyzer.Parser.ParseError (ParseError)
import Analyzer.Parser.Token (SourcePosition (..))
import Control.Monad.Except (Except)
import Control.Monad.State.Lazy (StateT, get, modify)
import Data.Word (Word8)

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

setStartCode :: Int -> Parser ()
setStartCode startCode = modify $ \s -> s {parserStartCode = startCode}

data ParserState = ParserState
  { parserSourcePosition :: SourcePosition,
    parserRemainingInput :: ParserInput,
    parserStartCode :: Int
  }
  deriving (Show)

initialState :: String -> ParserState
initialState source =
  ParserState
    { parserSourcePosition = SourcePosition 1 1,
      parserRemainingInput = ('\n', [], source),
      parserStartCode = 0
    }

-- | The type of the input given to the parser/lexer
--
--   An input @(c, bs, str)@ represents
--   - @c@ The previous character consumed by the lexer
--   - @bs@ The UTF8 bytes of the current character being lexed
--   - @str@ The remaining input to be lexed and parsed
type ParserInput = (Char, [Word8], String)
