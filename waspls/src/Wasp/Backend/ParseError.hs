{-# LANGUAGE DeriveGeneric #-}

module Wasp.Backend.ParseError
  ( -- * Parse error
    ParseError (..),

    -- * Display functions
    showError,
    showErrorMessage,
    errorSpan,

    -- * Source positions
    Span (..),
    SourcePos (..),
    offsetToSourcePos,

    -- * Expected set

    -- | A type for saying what token kinds were expected
    ExpectedSet,
    esetEmpty,
    esetMember,
    esetInsert,
    esetInsertKind,
    esetUnion,
    esetSingleton,
    esetFromList,
  )
where

import Control.DeepSeq (NFData)
import Data.List (intercalate)
import Data.Set (Set)
import qualified Data.Set as Set
import GHC.Generics (Generic)
import Wasp.Backend.Token (TokenKind)
import qualified Wasp.Backend.Token as T

data ParseError
  = Unexpected !Span !TokenKind ExpectedSet
  | UnexpectedEOF !Int ExpectedSet
  deriving (Eq, Ord, Show, Generic)

instance NFData ParseError

data Span = Span !Int !Int deriving (Eq, Ord, Show, Generic)

instance NFData Span

data SourcePos = SourcePos !Int !Int deriving (Eq, Ord, Show, Generic)

instance NFData SourcePos

offsetToSourcePos :: String -> Int -> SourcePos
offsetToSourcePos source targetOffset = reach 0 (SourcePos 1 1) source
  where
    reach :: Int -> SourcePos -> String -> SourcePos
    reach o (SourcePos l c) remaining
      | o == targetOffset = SourcePos l c
      | [] <- remaining = SourcePos l c
      | ('\n' : remaining') <- remaining =
        let sp' = SourcePos (l + 1) 1
         in reach (o + 1) sp' remaining'
      | (_ : remaining') <- remaining =
        let sp' = SourcePos l (c + 1)
         in reach (o + 1) sp' remaining'

data ExpectedSet = ExpectedSet {esetAllowEof :: Bool, esetKinds :: Set TokenKind}
  deriving (Eq, Ord, Show, Generic)

instance NFData ExpectedSet

esetEmpty :: ExpectedSet
esetEmpty = ExpectedSet {esetAllowEof = False, esetKinds = Set.empty}

esetMember :: Maybe TokenKind -> ExpectedSet -> Bool
esetMember Nothing eset = esetAllowEof eset
esetMember (Just kind) eset = kind `Set.member` esetKinds eset

esetInsertKind :: TokenKind -> ExpectedSet -> ExpectedSet
esetInsertKind kind eset = Just kind `esetInsert` eset

esetInsert :: Maybe TokenKind -> ExpectedSet -> ExpectedSet
esetInsert Nothing eset = eset {esetAllowEof = True}
esetInsert (Just kind) eset = eset {esetKinds = kind `Set.insert` esetKinds eset}

esetUnion :: ExpectedSet -> ExpectedSet -> ExpectedSet
esetUnion x y =
  ExpectedSet
    { esetAllowEof = esetAllowEof x || esetAllowEof y,
      esetKinds = esetKinds x `Set.union` esetKinds y
    }

esetSingleton :: Maybe TokenKind -> ExpectedSet
esetSingleton k = k `esetInsert` esetEmpty

esetFromList :: [TokenKind] -> ExpectedSet
esetFromList ks = ExpectedSet {esetAllowEof = False, esetKinds = Set.fromList ks}

showError :: String -> ParseError -> String
showError source msg =
  let (Span so eo) = errorSpan msg
      start = offsetToSourcePos source so
      end = offsetToSourcePos source eo
   in "Parse error at " ++ showRegion start end ++ " (" ++ show so ++ ".." ++ show eo ++ ")\n  " ++ showErrorMessage msg

errorSpan :: ParseError -> Span
errorSpan (UnexpectedEOF o _) = Span o o
errorSpan (Unexpected spn _ _) = spn

showErrorMessage :: ParseError -> String
showErrorMessage (UnexpectedEOF _ expecteds) =
  "Unexpected end of file, " ++ showExpected expecteds
showErrorMessage (Unexpected _ actual expecteds) =
  "Unexpected token " ++ showTokenKind actual ++ ", " ++ showExpected expecteds

showExpected :: ExpectedSet -> String
showExpected expecteds = "expected one of " ++ showExpecteds expecteds

showExpecteds :: ExpectedSet -> String
showExpecteds expecteds =
  let kindStrs = map showTokenKind $ Set.toList $ esetKinds expecteds
      eofStrs = if esetAllowEof expecteds then ["<eof>"] else []
   in intercalate "," (kindStrs ++ eofStrs)

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

showRegion :: SourcePos -> SourcePos -> String
showRegion start@(SourcePos sl sc) end@(SourcePos el ec)
  | start == end = show sl ++ ":" ++ show sc
  | sl == el = show sl ++ ":" ++ show sc ++ "-" ++ show ec
  | otherwise = show sl ++ ":" ++ show sc ++ "-" ++ show el ++ ":" ++ show ec
