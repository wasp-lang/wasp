module Wasp.Analyzer.Parser.SourcePosition
  ( SourcePosition (..),
  )
where

-- | The first character on the first line is at position @Position 1 1@
-- @SourcePosition <line> <column>@
data SourcePosition = SourcePosition Int Int deriving (Eq, Show)
