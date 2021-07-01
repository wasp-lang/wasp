module Analyzer.Parser.Util
  ( ParseState (..),
    initialState,
    Parser,
    updatePosn,
    putInput,
    AlexInput,
  )
where

import Analyzer.Parser.Syntax (ParseError, Posn (..))
import Control.Monad.Trans.Except (Except)
import Control.Monad.Trans.State.Lazy (StateT, get, modify)
import Data.Word (Word8)

-- | Tracks state of the parser, which is
--   - Current line/column position
--   - Current input to the lexer
data ParseState = ParseState
  { psPosn :: Posn,
    psInput :: AlexInput
  }
  deriving (Show)

-- | Create an initial state from a source string
initialState :: String -> ParseState
initialState source =
  ParseState
    { psPosn = Posn 1 1,
      psInput = ('\n', [], source)
    }

type Parser a = StateT ParseState (Except ParseError) a

-- | @updatePosn str n@ updates the parse state position based on the first
--   @n@ characters in @str@, incrementing line count on newlines only.
updatePosn :: String -> Int -> Parser ()
updatePosn str len = do
  pos <- psPosn <$> get
  let pos' = go (take len str) pos
  modify $ \s -> s {psPosn = pos'}
  where
    -- Scan the string character by character to look for newlines
    go [] pos = pos
    go ('\n' : cs) (Posn line _) = go cs $ Posn (line + 1) 1
    go (_ : cs) (Posn line col) = go cs $ Posn line (col + 1)

-- | Shorthand to replace the current lexer input in the parser state.
putInput :: AlexInput -> Parser ()
putInput inp = modify $ \s -> s {psInput = inp}

-- | The type of the input given to the lexer
type AlexInput = (Char, [Word8], String)
