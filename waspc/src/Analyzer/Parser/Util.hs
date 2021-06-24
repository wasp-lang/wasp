{-# LANGUAGE NamedFieldPuns #-}

module Analyzer.Parser.Util
  ( ParseState (..)
  , initialState
  , Parser
  , updatePosn
  , putInput
  , AlexInput
  ) where

import Analyzer.Parser.Syntax (ParseError, Posn (..))
import Data.Word (Word8)
import Control.Monad.Trans.State.Lazy (StateT, get, modify)
import Control.Monad.Trans.Except (Except)

-- | Tracks state of the parser, which is
--   - Current line/column position
--   - Current input to the lexer
data ParseState = ParseState { psPosn :: Posn
                             , psInput :: AlexInput
                             }
                             deriving Show

-- | Create an initial state from a source string
initialState :: String -> ParseState
initialState source = ParseState { psPosn = Posn { line = 1, col = 1 }
                                 , psInput = ('\n', [], source)
                                 }

type Parser a = StateT ParseState (Except ParseError) a

-- | Updates the parse state position based on the first `len` characters in
--   `str`.
updatePosn :: String -> Int -> Parser ()
updatePosn str len = do
  pos <- psPosn <$> get
  let pos' = go (take len str) pos
  modify $ \s -> s { psPosn = pos' }

  -- Scan the string character by character to look for newlines
  where go [] pos = pos
        go (c:cs) Posn { line, col } = case c of
                                         '\n' -> go cs $ Posn { line = line + 1, col = 1 }
                                         _ -> go cs $ Posn { line = line, col = col + 1 }

-- | Shorthand to replace the current lexer input in the parser state.
putInput :: AlexInput -> Parser ()
putInput inp = modify $ \s -> s { psInput = inp }

-- | The type of the input given to the lexer
type AlexInput = (Char, [Word8], String)
