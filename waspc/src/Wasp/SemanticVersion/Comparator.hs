{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use <$>" #-}
module Wasp.SemanticVersion.Comparator
  ( Comparator (..),
    PrimitiveOperator (..),
    simpleComparatorParser,
    hyphenRangeComparatorParser,
  )
where

import Text.Parsec (Parsec)
import qualified Text.Parsec as P
import Wasp.SemanticVersion.PartialVersion
  ( PartialVersion (..),
    partialVersionParser,
  )
import Wasp.SemanticVersion.Version (Version (..))
import Wasp.SemanticVersion.VersionBound
  ( HasVersionBounds (..),
    VersionBound (..),
  )

-- | A comparator is composed of an operator and a partial version.
-- It represents a single version constraint in a range.
-- See: https://github.com/npm/node-semver#ranges
data Comparator
  = PrimitiveComparator PrimitiveOperator PartialVersion
  | BackwardsCompatibleWith PartialVersion
  | ApproximatelyEquvivalentTo PartialVersion
  | XRange PartialVersion
  | HyphenRange PartialVersion PartialVersion
  deriving (Eq)

-- | We rely on this 'show' implementation to produce valid `node-semver` comparator.
instance Show Comparator where
  show (PrimitiveComparator op pv) = show op ++ show pv
  show (BackwardsCompatibleWith pv) = "^" ++ show pv
  show (ApproximatelyEquvivalentTo pv) = "~" ++ show pv
  show (XRange pv) = show pv
  show (HyphenRange pv1 pv2) = show pv1 ++ " - " ++ show pv2

instance HasVersionBounds Comparator where
  versionBounds (PrimitiveComparator primOp pv) = case primOp of
    Equal -> (toXRangeLowerBound pv, toXRangeUpperBound pv)
    LessThan -> (Inf, toLessThanUpperBound pv)
      where
        toLessThanUpperBound :: PartialVersion -> VersionBound
        toLessThanUpperBound Any = Exclusive $ Version 0 0 0
        toLessThanUpperBound (Major mjr) = Exclusive $ Version mjr 0 0
        toLessThanUpperBound (MajorMinor mjr mnr) = Exclusive $ Version mjr mnr 0
        toLessThanUpperBound (Full mjr mnr ptc) = Exclusive $ Version mjr mnr ptc
    LessThanOrEqual -> (Inf, toXRangeUpperBound pv)
    GreaterThan -> (toGreaterThanLowerBound pv, Inf)
      where
        toGreaterThanLowerBound :: PartialVersion -> VersionBound
        toGreaterThanLowerBound Any = Inf
        toGreaterThanLowerBound (Major mjr) = Inclusive $ Version (mjr + 1) 0 0
        toGreaterThanLowerBound (MajorMinor mjr mnr) = Inclusive $ Version mjr (mnr + 1) 0
        toGreaterThanLowerBound (Full mjr mnr ptc) = Exclusive $ Version mjr mnr ptc
    GreaterThanOrEqual -> (toXRangeLowerBound pv, Inf)
  versionBounds (XRange pv) =
    (toXRangeLowerBound pv, toXRangeUpperBound pv)
  versionBounds (HyphenRange lower upper) =
    (toXRangeLowerBound lower, toXRangeUpperBound upper)
  versionBounds (BackwardsCompatibleWith pv) =
    (toXRangeLowerBound pv, toCaretUpperBound pv)
    where
      -- Caret allows changes that don't modify the leftmost non-zero digit.
      toCaretUpperBound :: PartialVersion -> VersionBound
      toCaretUpperBound Any = Inf
      toCaretUpperBound (Major 0) = Exclusive (Version 1 0 0)
      toCaretUpperBound (Major mjr) = Exclusive (Version (mjr + 1) 0 0)
      toCaretUpperBound (MajorMinor 0 0) = Exclusive (Version 0 1 0)
      toCaretUpperBound (MajorMinor 0 mnr) = Exclusive (Version 0 (mnr + 1) 0)
      toCaretUpperBound (MajorMinor mjr _) = Exclusive (Version (mjr + 1) 0 0)
      toCaretUpperBound (Full 0 0 ptc) = Exclusive (Version 0 0 (ptc + 1))
      toCaretUpperBound (Full 0 mnr _) = Exclusive (Version 0 (mnr + 1) 0)
      toCaretUpperBound (Full mjr _ _) = Exclusive (Version (mjr + 1) 0 0)
  versionBounds (ApproximatelyEquvivalentTo pv) =
    (toXRangeLowerBound pv, toTildeUpperBound pv)
    where
      -- Tilde allows patch-level changes if minor is specified.
      toTildeUpperBound :: PartialVersion -> VersionBound
      toTildeUpperBound Any = Inf
      toTildeUpperBound (Major mjr) = Exclusive (Version (mjr + 1) 0 0)
      toTildeUpperBound (MajorMinor mjr mnr) = Exclusive (Version mjr (mnr + 1) 0)
      toTildeUpperBound (Full mjr mnr _) = Exclusive (Version mjr (mnr + 1) 0)

toXRangeUpperBound :: PartialVersion -> VersionBound
toXRangeUpperBound Any = Inf
toXRangeUpperBound (Major mjr) = Exclusive (Version (mjr + 1) 0 0)
toXRangeUpperBound (MajorMinor mjr mnr) = Exclusive (Version mjr (mnr + 1) 0)
toXRangeUpperBound (Full mjr mnr ptc) = Inclusive (Version mjr mnr ptc)

toXRangeLowerBound :: PartialVersion -> VersionBound
toXRangeLowerBound Any = Inclusive $ Version 0 0 0
toXRangeLowerBound (Major mjr) = Inclusive $ Version mjr 0 0
toXRangeLowerBound (MajorMinor mjr mnr) = Inclusive $ Version mjr mnr 0
toXRangeLowerBound (Full mjr mnr ptc) = Inclusive $ Version mjr mnr ptc

data PrimitiveOperator
  = Equal
  | LessThan
  | LessThanOrEqual
  | GreaterThan
  | GreaterThanOrEqual
  deriving (Eq)

-- | We rely on this 'show' implementation to produce valid `node-semver` comparator.
instance Show PrimitiveOperator where
  -- Equal shows as "" because both "=1.2.3" and "1.2.3" are valid,
  -- and the canonical form omits the "=".
  show Equal = ""
  show LessThan = "<"
  show LessThanOrEqual = "<="
  show GreaterThan = ">"
  show GreaterThanOrEqual = ">="

-- | Parses a single hyphen range comparator.
-- Separated from 'simpleComparatorParser' because hyphen ranges cannot be
-- combined with other comparators in a comparator set.
-- See `hyphen` definition here: https://github.com/npm/node-semver#range-grammar
hyphenRangeComparatorParser :: Parsec String () Comparator
hyphenRangeComparatorParser = do
  lowerVersion <- partialVersionParser
  _ <- hyphenParser
  upperVersion <- partialVersionParser
  pure $ HyphenRange lowerVersion upperVersion
  where
    hyphenParser :: Parsec String () Char
    hyphenParser = P.spaces *> P.char '-' <* P.spaces

-- | Parses a single non-hyphen comparator (primitive, tilde, caret, or x-range).
-- Separated from 'hyphenRangeComparatorParser' because hyphen ranges cannot be
-- combined with other comparators in a comparator set.
-- See `simple` definition here: https://github.com/npm/node-semver#range-grammar
simpleComparatorParser :: Parsec String () Comparator
simpleComparatorParser =
  P.choice
    [ emptyInputIsAnyParser,
      tildeComparatorParser,
      caretComparatorParser,
      xRangeComparatorParser,
      primitiveComparatorParser
    ]
  where
    -- `node-semver` allows parsing of a empty string into the x-range any comparator (*).
    emptyInputIsAnyParser :: Parsec String () Comparator
    emptyInputIsAnyParser = XRange Any <$ P.eof

    tildeComparatorParser :: Parsec String () Comparator
    tildeComparatorParser = ApproximatelyEquvivalentTo <$> (P.char '~' *> partialVersionParser)

    caretComparatorParser :: Parsec String () Comparator
    caretComparatorParser = BackwardsCompatibleWith <$> (P.char '^' *> partialVersionParser)

    xRangeComparatorParser :: Parsec String () Comparator
    xRangeComparatorParser = XRange <$> partialVersionParser

    primitiveComparatorParser :: Parsec String () Comparator
    primitiveComparatorParser = PrimitiveComparator <$> primitiveOperatorParser <*> partialVersionParser

    primitiveOperatorParser :: Parsec String () PrimitiveOperator
    primitiveOperatorParser =
      P.choice
        [ LessThanOrEqual <$ P.try (P.string "<="),
          GreaterThanOrEqual <$ P.try (P.string ">="),
          LessThan <$ P.char '<',
          GreaterThan <$ P.char '>',
          Equal <$ P.char '='
        ]
