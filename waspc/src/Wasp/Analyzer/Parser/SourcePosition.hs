module Wasp.Analyzer.Parser.SourcePosition
  ( SourcePosition (..),
    calcNextPosition,
    -- | TODO: Add types for source offsets and regions that use offsets. Name
    -- ideas are @SourceLinearPosition@/@SourceLinearRegion@ and @SourceOffset@/
    -- @SourceOffsetRegion@.
    offsetToPosition,
  )
where

-- | The first character on the first line is at position @Position 1 1@
-- @SourcePosition <line> <column>@
data SourcePosition = SourcePosition Int Int deriving (Eq, Show)

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

offsetToPosition :: String -> Int -> SourcePosition
offsetToPosition source targetOffset =
  calcNextPosition (take targetOffset source) (SourcePosition 1 1)
