{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module Wasp.Backend.Lexer.Internal
  ( -- * Lexer monad code
    Lexer,
    runLexer,
    initialLexState,
    LexState,
    LexerStartCode (..),
    LexInput (..),
    getInput,
    updateInput,
    setStartCode,
    getStartCode,

    -- * Alex utilities
    AlexInput,
    startCodeToInt,
    alexGetByte,
    alexInputPrevChar,
    beginQuoter,
    lexQuoterEndTag,
    createToken,
  )
where

import Codec.Binary.UTF8.String (encodeChar)
import Control.Monad.State.Strict (MonadState, State, evalState, get, modify)
import Data.Word (Word8)
import Wasp.Backend.Token (Token (..), TokenKind)
import qualified Wasp.Backend.Token as T

-- LEXER MONAD CODE

newtype Lexer a = Lexer {unLexer :: State LexState a}
  deriving (Functor, Applicative, Monad, MonadState LexState)

runLexer :: Lexer a -> LexState -> a
runLexer lexer state = evalState (unLexer lexer) state

initialLexState :: String -> LexState
initialLexState source =
  LexState
    { lstateInput = LexInput '\n' [] source,
      lstateStartCode = DefaultStartCode
    }

data LexState = LexState
  { lstateInput :: LexInput,
    lstateStartCode :: LexerStartCode
  }

getInput :: Lexer LexInput
getInput = lstateInput <$> get

updateInput :: Int -> Lexer ()
updateInput !consumed = do
  (LexInput _ _ remaining) <- getInput
  let newInput =
        let (prevChar : remaining') = drop (consumed - 1) remaining
         in LexInput prevChar [] remaining'
  modify $ \s -> s {lstateInput = newInput}

getStartCode :: Lexer LexerStartCode
getStartCode = lstateStartCode <$> get

setStartCode :: LexerStartCode -> Lexer ()
setStartCode startCode = modify $ \s -> s {lstateStartCode = startCode}

-- | A representation of the lexer's start code: https://www.haskell.org/alex/doc/html/alex-files.html#startcodes
data LexerStartCode
  = -- | For a start code @DefaultStartCode@, the lexer is in start code <0>
    DefaultStartCode
  | -- | For a start code @QuoterStartCode tag@, the lexer is in start code <quoter> with opening tag @tag@
    QuoterStartCode String
  deriving (Show)

-- | The type of the input given to the parser/lexer.
--
--   An input @(prevChar, bs, remainingSource)@ represents:
--   - @prevChar@ The previous character, successfully consumed by the lexer.
--   - @bs@ The yet unconsumed UTF8 bytes of the current character being lexed.
--   - @remainingSource@ The remaining source to be lexed and parsed
--           (including the character currently being lexed as the first char in it).
data LexInput = LexInput Char [Word8] String

-- ALEX UTILITIES

type AlexInput = LexInput

-- | Convert the ParserState's start code to an int for Alex to use
startCodeToInt :: Int -> LexerStartCode -> Int
startCodeToInt _ DefaultStartCode = 0
startCodeToInt quoter (QuoterStartCode _) = quoter

-- | Required by Alex.
--
-- This function is taken from the Alex basic wrapper.
alexGetByte :: AlexInput -> Maybe (Word8, AlexInput)
alexGetByte (LexInput prevChar (b : bs) remainingSource) = Just (b, LexInput prevChar bs remainingSource)
alexGetByte (LexInput _ [] []) = Nothing
alexGetByte (LexInput _ [] (currChar : remainingSource)) = case encodeChar currChar of
  (b : bs) -> Just (b, LexInput currChar bs remainingSource)
  [] -> Nothing

-- | Required by Alex.
--
-- This function is taken from the Alex basic wrapper.
alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar (LexInput prevChar _ _) = prevChar

-- | Takes a lexeme like "{=json" and sets the quoter start code
beginQuoter :: String -> Lexer Token
beginQuoter leftQuoteTag = do
  let tag = drop 2 leftQuoteTag
  setStartCode (QuoterStartCode tag)
  createToken T.LQuote leftQuoteTag

-- | Takes a lexeme like "json=}" and either ends a quoter or add quoted text to
-- the quoter
lexQuoterEndTag :: String -> Lexer Token
lexQuoterEndTag rightQuoteTag = do
  startCode <- getStartCode
  case startCode of
    DefaultStartCode -> error "impossible: lexQuoterEndTag with DefaultStartCode"
    QuoterStartCode startTag | startTag == tag -> do
      setStartCode DefaultStartCode
      createToken T.RQuote rightQuoteTag
    _ -> do
      createToken T.Quoted rightQuoteTag
  where
    tag = take (length rightQuoteTag - 2) rightQuoteTag

-- | Makes an action that creates a token from a "TokenKind"
createToken :: TokenKind -> (String -> Lexer Token)
createToken kind lexeme =
  return $
    Token
      { tokenKind = kind,
        tokenWidth = length lexeme
      }
