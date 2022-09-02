{-# LANGUAGE DeriveGeneric #-}

module Wasp.Analyzer.Parser.ConcreteParser.ParseError
  ( -- * Parse error
    ParseError (..),

    -- * Source positions
    errorSpan,
    showError,
    showErrorMessage,
  )
where

import Control.DeepSeq (NFData)
import Data.List (intercalate)
import GHC.Generics (Generic)
import Wasp.Analyzer.Parser.SourceOffset (SourceOffset)
import Wasp.Analyzer.Parser.SourceRegion (sourceSpanToRegion)
import Wasp.Analyzer.Parser.SourceSpan (SourceSpan (..))
import Wasp.Analyzer.Parser.Token (TokenKind)
import qualified Wasp.Analyzer.Parser.Token as T
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

-- TODO: I believe this is showing error for the purposes of LSP diagnostics? Maybe move it there?
showError :: String -> ParseError -> String
showError source msg =
  let sourceRegion = sourceSpanToRegion source $ errorSpan msg
   in "Parse error at " ++ show sourceRegion ++ ":\n  " ++ showErrorMessage msg

showErrorMessage :: ParseError -> String
showErrorMessage (UnexpectedEOF _ expecteds) =
  "Unexpected end of file, " ++ showExpected expecteds
showErrorMessage (UnexpectedToken _ actual expecteds) =
  "Unexpected token " ++ showTokenKind actual ++ ", " ++ showExpected expecteds

showExpected :: TokenSet -> String
showExpected expecteds = "expected one of " ++ showExpecteds expecteds

showExpecteds :: TokenSet -> String
showExpecteds expecteds =
  let kindStrs = map showTokenKind $ TokenSet.toList expecteds
      eofStrs = if TokenSet.eofMember expecteds then ["<eof>"] else []
   in intercalate "," (kindStrs ++ eofStrs)

-- TODO: This should probably go under TokenKind? Or is it specific for error reporting here?
showTokenKind :: TokenKind -> String
showTokenKind T.White = "<whitespace>"
showTokenKind T.Newline = "\\n"
showTokenKind T.Comment = "<comment>"
showTokenKind T.LParen = "'('"
showTokenKind T.RParen = "')'"
showTokenKind T.LSquare = "'['"
showTokenKind T.RSquare = "']'"
showTokenKind T.LCurly = "'{'"
showTokenKind T.RCurly = "'}'"
showTokenKind T.Comma = "','"
showTokenKind T.Colon = "':'"
showTokenKind T.KwImport = "'import'"
showTokenKind T.KwFrom = "'from'"
showTokenKind T.KwTrue = "'true'"
showTokenKind T.KwFalse = "'false'"
showTokenKind T.String = "<string>"
showTokenKind T.Int = "<number>"
showTokenKind T.Double = "<number>"
showTokenKind T.LQuote = "'{='"
showTokenKind T.RQuote = "'=}'"
showTokenKind T.Quoted = "<any>" -- Should be impossible, hard to prove though
showTokenKind T.Identifier = "<identifier>"
showTokenKind T.Error = "<error>"
