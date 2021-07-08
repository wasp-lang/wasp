module Analyzer.Parser.Util
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

-- | Tracks state of the parser, which is
--   - Current line/column position
--   - Current input to the lexer
data ParserState = ParserState
  { parserSourcePosition :: SourcePosition,
    parserRemainingInput :: ParserInput,
    parserStartCode :: Int
  }
  deriving (Show)

-- | Create an initial state from a source string
initialState :: String -> ParserState
initialState source =
  ParserState
    { parserSourcePosition = SourcePosition 1 1,
      parserRemainingInput = ('\n', [], source),
      parserStartCode = 0
    }

type Parser a = StateT ParserState (Except ParseError) a

-- | @updatePosition str@ updates the parse state position based on the
--   characters in @str@, incrementing line count on newlines only.
updatePosition :: String -> Parser ()
updatePosition str = do
  pos <- parserSourcePosition <$> get
  let pos' = go str pos
  modify $ \s -> s {parserSourcePosition = pos'}
  where
    -- Scan the string character by character to look for newlines
    go [] pos = pos
    go ('\n' : cs) (SourcePosition line _) = go cs $ SourcePosition (line + 1) 1
    go (_ : cs) (SourcePosition line col) = go cs $ SourcePosition line (col + 1)

-- | Shorthand to replace the current lexer input in the parser state.
putInput :: ParserInput -> Parser ()
putInput inp = modify $ \s -> s {parserRemainingInput = inp}

setStartCode :: Int -> Parser ()
setStartCode state = do
  modify $ \s -> s {parserStartCode = state}

-- | The type of the input given to the parser/lexer
--
--   An input @(c, bs, str)@ represents
--   - @c@ The previous character consumed by the lexer
--   - @bs@ The UTF8 bytes of the current character being lexed
--   - @str@ The remaining input to be lexed and parsed
type ParserInput = (Char, [Word8], String)
