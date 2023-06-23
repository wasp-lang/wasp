module Wasp.Analyzer.Parser.SourcePosition
  ( SourcePosition (..),
    calcNextPosition,
    sourceOffsetToPosition,
  )
where

import Data.Aeson (FromJSON (parseJSON), withObject, (.:))
import Wasp.Analyzer.Parser.SourceOffset (SourceOffset)

-- | The first character on the first line is at position @Position 1 1@
-- @SourcePosition <line> <column>@
data SourcePosition = SourcePosition Int Int deriving (Eq)

instance FromJSON SourcePosition where
  parseJSON = withObject "SourcePosition" $ \v ->
    SourcePosition <$> v .: "line" <*> v .: "column"

instance Show SourcePosition where
  show (SourcePosition line column) = show line ++ ":" ++ show column

-- | Scan the source fragment character by character and update position based on it.
-- Important thing that this function does is ensure that newlines are correctly handled.
calcNextPosition ::
  SourceFragment ->
  -- | Start position of source fragment (first char).
  SourcePosition ->
  -- | Source position right after the source fragment (position after the last char).
  SourcePosition
calcNextPosition [] position = position
calcNextPosition ('\n' : cs) (SourcePosition line _) = calcNextPosition cs $ SourcePosition (line + 1) 1
calcNextPosition (_ : cs) (SourcePosition line col) = calcNextPosition cs $ SourcePosition line (col + 1)

type SourceFragment = String

sourceOffsetToPosition :: String -> SourceOffset -> SourcePosition
sourceOffsetToPosition source targetOffset =
  calcNextPosition (take targetOffset source) (SourcePosition 1 1)
