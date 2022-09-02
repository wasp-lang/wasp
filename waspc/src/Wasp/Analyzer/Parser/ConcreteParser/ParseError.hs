{-# LANGUAGE DeriveGeneric #-}

module Wasp.Analyzer.Parser.ConcreteParser.ParseError
  ( -- * Parse error
    ParseError (..),

    -- * Source positions
    Region (..),
    errorRegion,
    showError,
    showErrorMessage,
  )
where

import Control.DeepSeq (NFData)
import Data.List (intercalate)
import GHC.Generics (Generic)
import Wasp.Analyzer.Parser.SourcePosition (SourcePosition (..), offsetToPosition)
import Wasp.Analyzer.Parser.Token (TokenKind)
import qualified Wasp.Analyzer.Parser.Token as T
import Wasp.Analyzer.Parser.TokenSet (TokenSet)
import qualified Wasp.Analyzer.Parser.TokenSet as TokenSet

data ParseError
  = UnexpectedToken !Region !TokenKind TokenSet
  | UnexpectedEOF !Int TokenSet
  deriving (Eq, Ord, Show, Generic)

instance NFData ParseError

-- TODO: This file is a bit of a mess due to moving it quickly from LSP here.
-- Figure out which logic should remain here, and which not:
-- some logic might belong better in LSP, close to Diagnostics,
-- and some might be moved to Token in waspc, or smth like that.
-- Also, what about liberal usage of term "offset", can we encode that somehow better?
-- name `Region` is also not great, should probably be called OffsetRegion or smth like that.

-- TODO: It is weird to have Region here, why would it be here? It should be somewhere
-- at the same level as SourcePosition and SourceRegion. Check TODO in SourcePosition for more inspiration.

-- | @Region start end@ where @start@ is the offset of the first character in
-- the region and @end@ is the offset of the first character after the region.
-- In other words, its the region of characters with offsets from @start@ to
-- @end@, including @start@ but not including @end@.
data Region = Region !Int !Int deriving (Eq, Ord, Show, Generic)

instance NFData Region

-- TODO: Make this receive a proper SourceRegion instead of two SourcePositions.
-- Also, move it to SourceRegion? Is it specific for it, or is it specific for diagnostics?
showRegion :: SourcePosition -> SourcePosition -> String
showRegion start@(SourcePosition sl sc) end@(SourcePosition el ec)
  | start == end = show sl ++ ":" ++ show sc
  | sl == el = show sl ++ ":" ++ show sc ++ "-" ++ show ec
  | otherwise = show sl ++ ":" ++ show sc ++ "-" ++ show el ++ ":" ++ show ec

errorRegion :: ParseError -> Region
errorRegion (UnexpectedEOF o _) = Region o o
errorRegion (UnexpectedToken rgn _ _) = rgn

-- TODO: I believe this is showing error for the purposes of LSP diagnostics? Maybe move it there?
showError :: String -> ParseError -> String
showError source msg =
  let (Region so eo) = errorRegion msg
      start = offsetToPosition source so
      end = offsetToPosition source eo
   in "Parse error at " ++ showRegion start end ++ " (" ++ show so ++ ".." ++ show eo ++ ")\n  " ++ showErrorMessage msg

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
