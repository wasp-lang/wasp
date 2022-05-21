module Wasp.Analyzer.Parser.LexerUtils
  ( AlexInput
  , beginQuoter
  , lexQuoterEndTag
  , createConstToken
  , createValueToken
  ) where

import Wasp.Analyzer.Parser.Monad
import Wasp.Analyzer.Parser.Token (Token (..), TokenType (..))
import Control.Monad.State.Lazy (gets)

type AlexInput = ParserInput

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

