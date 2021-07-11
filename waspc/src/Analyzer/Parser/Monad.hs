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
import Control.Monad.Trans.Except (Except)
import Control.Monad.Trans.State.Lazy (StateT, get, modify)
import Data.Word (Word8)

type Parser a = StateT ParserState (Except ParseError) a

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

-- | Updates the current position of parser in the source based on the
--   latest parsed piece of source.
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

-- | Shorthand to replace the current lexer input in the parser state.
putInput :: ParserInput -> Parser ()
putInput input = modify $ \s -> s {parserRemainingInput = input}

setStartCode :: Int -> Parser ()
setStartCode startCode = do
  modify $ \s -> s {parserStartCode = startCode}

-- | The type of the input given to the parser/lexer
--
--   An input @(c, bs, str)@ represents
--   - @c@ The previous character consumed by the lexer
--   - @bs@ The UTF8 bytes of the current character being lexed
--   - @str@ The remaining input to be lexed and parsed
type ParserInput = (Char, [Word8], String)
