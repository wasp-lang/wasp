{-# LANGUAGE FlexibleContexts #-}

module Wasp.Analyzer.Parser.Monad
  ( ParserState (..),
    initialState,
    Parser,
    updateParserStateWithScannedToken,
    updateParserStateWithSkippedChars,
    setStartCode,
    ParserInput,
    LexerStartCode (..),
  )
where

import Control.Monad.Except (Except)
import Control.Monad.State.Lazy (StateT, get, modify)
import Data.Word (Word8)
import Wasp.Analyzer.Parser.ParseError (ParseError)
import Wasp.Analyzer.Parser.SourcePosition (SourcePosition (..), calcNextPosition)
import Wasp.Analyzer.Parser.Token

type Parser a = StateT ParserState (Except ParseError) a

updateParserStateWithScannedToken :: Token -> Parser ()
updateParserStateWithScannedToken token = do
  updatePositionAndInput (tokenLexeme token)
  modify $ \s ->
    s
      { lastToLastScannedToken = lastScannedToken s,
        lastScannedToken = token
      }

updateParserStateWithSkippedChars :: Int -> Parser ()
updateParserStateWithSkippedChars numChars = do
  (_, _, remainingSource) <- parserRemainingInput <$> get
  let charsSkipped = take numChars remainingSource
  updatePositionAndInput charsSkipped

updatePositionAndInput :: String -> Parser ()
updatePositionAndInput parsedSourcePiece = do
  position <- parserSourcePosition <$> get
  (_, _, remainingSource) <- parserRemainingInput <$> get
  let position' = calcNextPosition parsedSourcePiece position
  let input' =
        let (prevChar : remainingSource') = drop (length parsedSourcePiece - 1) remainingSource
         in (prevChar, [], remainingSource')
  modify $ \s ->
    s
      { parserSourcePosition = position',
        parserRemainingInput = input'
      }

setStartCode :: LexerStartCode -> Parser ()
setStartCode startCode = modify $ \s -> s {parserLexerStartCode = startCode}

data ParserState = ParserState
  { parserSourcePosition :: SourcePosition,
    -- | Last token that was scanned by Alex.
    -- NOTE: Token first gets scanned by Alex, and then it gets parsed by Happy.
    lastScannedToken :: Token,
    -- | Second last token that was scanned by Alex.
    lastToLastScannedToken :: Token,
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
      lastScannedToken = Token TEOF (SourcePosition 1 1) "\n", -- NOTE: Dummy initial value.
      lastToLastScannedToken = Token TEOF (SourcePosition 1 1) "\n", -- NOTE: Dummy initial value.
      parserRemainingInput = ('\n', [], source), -- NOTE: '\n' here is dummy initial value.
      parserLexerStartCode = DefaultStartCode
    }

-- | The type of the input given to the parser/lexer.
--
--   An input @(prevChar, bs, remainingSource)@ represents:
--   - @prevChar@ The previous character, successfully consumed by the lexer.
--   - @bs@ The yet unconsumed UTF8 bytes of the current character being lexed.
--   - @remainingSource@ The remaining source to be lexed and parsed
--           (including the character currently being lexed as the first char in it).
type ParserInput = (Char, [Word8], String)
