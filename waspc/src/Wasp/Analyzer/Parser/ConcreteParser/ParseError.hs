{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}

module Wasp.Analyzer.Parser.ConcreteParser.ParseError
  ( -- * Parse error
    ParseError (..),

    -- * Source positions
    errorSpan,
    getErrorMessage,
    getErrorMessageAndCtx,
  )
where

import Control.DeepSeq (NFData)
import Data.List (intercalate)
import GHC.Generics (Generic)
import Wasp.Analyzer.Parser.Ctx (Ctx (Ctx))
import Wasp.Analyzer.Parser.SourceOffset (SourceOffset)
import Wasp.Analyzer.Parser.SourceRegion (sourceSpanToRegion)
import Wasp.Analyzer.Parser.SourceSpan (SourceSpan (..))
import Wasp.Analyzer.Parser.Token (TokenKind, showTokenKind)
import Wasp.Analyzer.Parser.TokenSet (TokenSet)
import qualified Wasp.Analyzer.Parser.TokenSet as TokenSet

data ParseError
  = UnexpectedToken !SourceSpan !TokenKind TokenSet
  | UnexpectedEOF !SourceOffset TokenSet
  deriving (Eq, Ord, Show, Generic)

instance NFData ParseError

errorSpan :: ParseError -> SourceSpan
errorSpan (UnexpectedEOF srcOffset _) = SourceSpan srcOffset srcOffset
errorSpan (UnexpectedToken srcSpan _ _) = srcSpan

getErrorMessageAndCtx :: String -> ParseError -> (String, Ctx)
getErrorMessageAndCtx source err = (getErrorMessage err, Ctx $ sourceSpanToRegion source $ errorSpan err)

getErrorMessage :: ParseError -> String
getErrorMessage = \case
  UnexpectedEOF _ expectedTokens ->
    "Unexpected end of file, " ++ expectedTokensErrorMessage expectedTokens
  UnexpectedToken _ actual expectedTokens ->
    "Unexpected token " ++ showTokenKind actual ++ ", " ++ expectedTokensErrorMessage expectedTokens
  where
    expectedTokensErrorMessage :: TokenSet -> String
    expectedTokensErrorMessage tokens =
      let kindStrs = map showTokenKind $ TokenSet.toList tokens
          eofStrs = if TokenSet.eofMember tokens then ["<eof>"] else []
       in "expected one of the following tokens instead: " ++ intercalate "," (kindStrs ++ eofStrs)
