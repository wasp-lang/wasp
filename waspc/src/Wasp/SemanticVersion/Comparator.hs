{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use <$>" #-}
module Wasp.SemanticVersion.Comparator
  ( Comparator (..),
    PrimitiveOperator (..),
    primitiveComparatorParser,
    toXRangeUpperBound,
    toXRangeLowerBound,
  )
where

import qualified Text.Parsec as P
import Wasp.SemanticVersion.PartialVersion
  ( PartialVersion (..),
    partialVersionParser,
  )
import Wasp.SemanticVersion.Version (Version (..))
import Wasp.SemanticVersion.VersionBound
  ( HasVersionBounds (..),
    VersionBound (..),
    noVersionInterval,
  )

-- | A comparator composed of an operator and a partial version.
-- It represents a single version constraint in a range.
data Comparator
  = Comparator PrimitiveOperator PartialVersion
  deriving (Eq)

data PrimitiveOperator
  = Equal
  | LessThan
  | LessThanOrEqual
  | GreaterThan
  | GreaterThanOrEqual
  deriving (Eq)

-- | We rely on this 'show' implementation to produce valid `node-semver` comparator.
instance Show Comparator where
  show (Comparator op pv) = show op ++ show pv

-- | We rely on this 'show' implementation to produce valid `node-semver` comparator.
instance Show PrimitiveOperator where
  -- Equal shows as "" because both "=1.2.3" and "1.2.3" are valid,
  -- and the canonical form omits the "=".
  show Equal = ""
  show LessThan = "<"
  show LessThanOrEqual = "<="
  show GreaterThan = ">"
  show GreaterThanOrEqual = ">="

instance HasVersionBounds Comparator where
  versionBounds (Comparator primOp pv) = case primOp of
    Equal -> (toXRangeLowerBound pv, toXRangeUpperBound pv)
    LessThan -> case pv of
      Any -> noVersionInterval
      (Major mjr) -> (Inclusive $ Version 0 0 0, Exclusive $ Version mjr 0 0)
      (MajorMinor mjr mnr) -> (Inclusive $ Version 0 0 0, Exclusive $ Version mjr mnr 0)
      (MajorMinorPatch mjr mnr ptc) -> (Inclusive $ Version 0 0 0, Exclusive $ Version mjr mnr ptc)
    LessThanOrEqual -> (Inclusive $ Version 0 0 0, toXRangeUpperBound pv)
    GreaterThan -> case pv of
      Any -> noVersionInterval
      (Major mjr) -> (Inclusive $ Version (mjr + 1) 0 0, Inf)
      (MajorMinor mjr mnr) -> (Inclusive $ Version mjr (mnr + 1) 0, Inf)
      (MajorMinorPatch mjr mnr ptc) -> (Exclusive $ Version mjr mnr ptc, Inf)
    GreaterThanOrEqual -> (toXRangeLowerBound pv, Inf)

toXRangeUpperBound :: PartialVersion -> VersionBound
toXRangeUpperBound Any = Inf
toXRangeUpperBound (Major mjr) = Exclusive (Version (mjr + 1) 0 0)
toXRangeUpperBound (MajorMinor mjr mnr) = Exclusive (Version mjr (mnr + 1) 0)
toXRangeUpperBound (MajorMinorPatch mjr mnr ptc) = Inclusive (Version mjr mnr ptc)

toXRangeLowerBound :: PartialVersion -> VersionBound
toXRangeLowerBound Any = Inclusive $ Version 0 0 0
toXRangeLowerBound (Major mjr) = Inclusive $ Version mjr 0 0
toXRangeLowerBound (MajorMinor mjr mnr) = Inclusive $ Version mjr mnr 0
toXRangeLowerBound (MajorMinorPatch mjr mnr ptc) = Inclusive $ Version mjr mnr ptc

-- | Parses a single primitive comparator.
-- See `primitive` definition here: https://github.com/npm/node-semver#range-grammar
primitiveComparatorParser :: P.Parsec String () Comparator
primitiveComparatorParser = Comparator <$> primitiveOperatorParser <* P.spaces <*> partialVersionParser
  where
    primitiveOperatorParser :: P.Parsec String () PrimitiveOperator
    primitiveOperatorParser =
      P.choice
        [ LessThanOrEqual <$ P.try (P.string "<="),
          GreaterThanOrEqual <$ P.try (P.string ">="),
          LessThan <$ P.char '<',
          GreaterThan <$ P.char '>',
          Equal <$ P.char '=',
          Equal <$ P.string ""
        ]
