module Wasp.Analyzer.Parser.LexerUtils
  ( AlexInput
  , startCodeToInt
  , alexGetByte
  , alexInputPrevChar
  , beginQuoter
  , lexQuoterEndTag
  , createConstToken
  , createValueToken
  ) where

import Wasp.Analyzer.Parser.Monad
import Wasp.Analyzer.Parser.Token (Token (..), TokenType (..))
import Control.Monad.State.Lazy (gets)
import Data.Word (Word8)
import Codec.Binary.UTF8.String (encodeChar)

type AlexInput = ParserInput

-- Convert the ParserState's start code to an int for Alex to use
startCodeToInt :: Int -> LexerStartCode -> Int
startCodeToInt _ DefaultStartCode = 0
startCodeToInt quoter (QuoterStartCode _) = quoter

-- | Required by Alex.
--
--   This function is taken from the Alex basic wrapper.
alexGetByte :: AlexInput -> Maybe (Word8, AlexInput)
alexGetByte (prevChar, (b:bs), remainingSource) = Just (b, (prevChar, bs, remainingSource))
alexGetByte (_, [], []) = Nothing
alexGetByte (_, [], (currChar:remainingSource)) = case encodeChar currChar of
                                                    (b:bs) -> Just (b, (currChar, bs, remainingSource))
                                                    [] -> Nothing
-- | Required by Alex.
--
--   This function is taken from the Alex basic wrapper.
alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar (prevChar, _, _) = prevChar

-- | Takes a lexeme like "{=json" and sets the quoter start code
beginQuoter :: String -> Parser Token
beginQuoter leftQuoteTag = do
  let tag = drop 2 leftQuoteTag
  setStartCode $ QuoterStartCode tag
  createConstToken (TLQuote tag) leftQuoteTag

-- | Takes a lexeme like "json=}" and either ends a quoter or add quoted text to the quoter
lexQuoterEndTag :: String -> Parser Token
lexQuoterEndTag rightQuoteTag = gets parserLexerStartCode >>= \startCode -> case startCode of
  DefaultStartCode -> error "impossible: lexQuoterEndTag with DefaultStartCode"
  QuoterStartCode startTag | startTag == tag -> do
    setStartCode DefaultStartCode
    createConstToken (TRQuote tag) rightQuoteTag
  _ -> do
    createValueToken TQuoted rightQuoteTag
  where
    tag = take (length rightQuoteTag - 2) rightQuoteTag

-- | Makes an action that creates a token from a constant TokenType.
createConstToken :: TokenType -> (String -> Parser Token)
createConstToken tokType lexeme = do
  position <- gets parserSourcePosition
  return $ Token { tokenType = tokType
                 , tokenStartPosition = position
                 , tokenLexeme = lexeme
                 }

-- | Makes an action that creates a token using the input lexeme.
createValueToken :: (String -> TokenType) -> (String -> Parser Token)
createValueToken getTokenType lexeme = createConstToken (getTokenType lexeme) lexeme

