{-# LANGUAGE DeriveLift #-}
{-# HLINT ignore "Use <$>" #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Wasp.SemanticVersion.ComparatorSet
  ( ComparatorSet (..),
    SimpleRangeExpression (..),
    comparatorSetParser,
    simpleRangeExpressionParser,
    hyphenRangeParser,
  )
where

import qualified Data.List.NonEmpty as NE
import qualified Language.Haskell.TH.Syntax as TH
import qualified Text.Parsec as P
import Wasp.SemanticVersion.Comparator
  ( Comparator (..),
    comparatorParser,
    toXRangeLowerBound,
    toXRangeUpperBound,
  )
import Wasp.SemanticVersion.PartialVersion (PartialVersion (..), partialVersionParser)
import Wasp.SemanticVersion.Version (Version (..))
import Wasp.SemanticVersion.VersionBound
  ( HasVersionBounds (versionBounds),
    VersionBound (..),
    intervalIntersection,
  )

-- | A comparator set is either:
--  - a sequence of simple range expressions (AND logic)
--  - a hyphen range
--
-- The name comparator set comes from the fact that all items of comparator set
-- can be desguared into comparators.
data ComparatorSet
  = SimpleComparatorSet (NE.NonEmpty SimpleRangeExpression)
  | HyphenRange PartialVersion PartialVersion
  deriving (Eq, TH.Lift)

-- | Anything that is either a comparator, or can be desugared into comparators.
-- Simple because all operators here require only a single partial version.
--
-- X-Range is already supported on all operators through the 'Partial Version' implementation.
data SimpleRangeExpression
  = -- | 1.2.3 (=1.2.3), >1.2.3, <1.2.3, >=1.2.3, <=1.2.3
    Primitive Comparator
  | -- | ~1.2.3
    TildeRange PartialVersion
  | -- | ^1.2.3
    CaretRange PartialVersion
  deriving (Eq, TH.Lift)

-- | We rely on this 'show' implementation to produce valid `node-semver` comparator set.
instance Show ComparatorSet where
  show (SimpleComparatorSet simpleRangeExpressions) = unwords $ show <$> NE.toList simpleRangeExpressions
  show (HyphenRange lower upper) = show lower ++ " - " ++ show upper

-- | We rely on this 'show' implementation to produce valid `node-semver` simple comparator.
instance Show SimpleRangeExpression where
  show (Primitive comparator) = show comparator
  show (TildeRange pv) = "~" ++ show pv
  show (CaretRange pv) = "^" ++ show pv

-- | We define concatenation of two comparator sets as a union of their range expressions.
-- Hyphen Ranges can't be combined with other comparator sets.
instance Semigroup ComparatorSet where
  (SimpleComparatorSet left) <> (SimpleComparatorSet right) = SimpleComparatorSet $ NE.nub $ left <> right
  (HyphenRange _ _) <> _ = error "Cannot combine Hyphen Range with other comparator sets"
  _ <> (HyphenRange _ _) = error "Cannot combine Hyphen Range with other comparator sets"

instance HasVersionBounds ComparatorSet where
  versionBounds (SimpleComparatorSet simpleRangeExpressions) =
    foldr1 intervalIntersection $ versionBounds <$> simpleRangeExpressions
  versionBounds (HyphenRange lower upper) = (toXRangeLowerBound lower, toXRangeUpperBound upper)

instance HasVersionBounds SimpleRangeExpression where
  versionBounds (Primitive comp) = versionBounds comp
  versionBounds (TildeRange pv) = (toXRangeLowerBound pv, toTildeRangeUpperBound pv)
  versionBounds (CaretRange pv) = (toXRangeLowerBound pv, toCareRangetUpperBound pv)

-- | Tilde range allows patch-level changes if minor is specified.
toTildeRangeUpperBound :: PartialVersion -> VersionBound
toTildeRangeUpperBound Any = Inf
toTildeRangeUpperBound (Major mjr) = Exclusive (Version (mjr + 1) 0 0)
toTildeRangeUpperBound (MajorMinor mjr mnr) = Exclusive (Version mjr (mnr + 1) 0)
toTildeRangeUpperBound (MajorMinorPatch mjr mnr _) = Exclusive (Version mjr (mnr + 1) 0)

-- | Caret range allows changes that don't modify the leftmost non-zero digit.
toCareRangetUpperBound :: PartialVersion -> VersionBound
toCareRangetUpperBound Any = Inf
toCareRangetUpperBound (Major 0) = Exclusive (Version 1 0 0)
toCareRangetUpperBound (Major mjr) = Exclusive (Version (mjr + 1) 0 0)
toCareRangetUpperBound (MajorMinor 0 0) = Exclusive (Version 0 1 0)
toCareRangetUpperBound (MajorMinor 0 mnr) = Exclusive (Version 0 (mnr + 1) 0)
toCareRangetUpperBound (MajorMinor mjr _) = Exclusive (Version (mjr + 1) 0 0)
toCareRangetUpperBound (MajorMinorPatch 0 0 ptc) = Exclusive (Version 0 0 (ptc + 1))
toCareRangetUpperBound (MajorMinorPatch 0 mnr _) = Exclusive (Version 0 (mnr + 1) 0)
toCareRangetUpperBound (MajorMinorPatch mjr _ _) = Exclusive (Version (mjr + 1) 0 0)

-- | Parses a comparator set: either a single hyphen range or
-- one or more simple comparators separated by spaces.
-- See `range` definition here: https://github.com/npm/node-semver#range-grammar
comparatorSetParser :: P.Parsec String () ComparatorSet
comparatorSetParser =
  P.choice
    [ P.try hyphenRangeParser,
      P.try simpleComparatorSetParser
    ]
  where
    simpleComparatorSetParser :: P.Parsec String () ComparatorSet
    simpleComparatorSetParser = do
      first <- simpleRangeExpressionParser
      rest <- P.many $ P.try (P.many1 P.space *> simpleRangeExpressionParser)
      pure $ SimpleComparatorSet (NE.fromList (first : rest))

-- | Parses a single non-hyphen comparator (primitive, tilde or caret).
-- See `simple` definition here: https://github.com/npm/node-semver#range-grammar
simpleRangeExpressionParser :: P.Parsec String () SimpleRangeExpression
simpleRangeExpressionParser =
  P.choice
    [ tildeRangeParser,
      caretRangeParser,
      primitiveParser
    ]
  where
    tildeRangeParser :: P.Parsec String () SimpleRangeExpression
    tildeRangeParser = TildeRange <$> (P.char '~' *> P.spaces *> partialVersionParser)

    caretRangeParser :: P.Parsec String () SimpleRangeExpression
    caretRangeParser = CaretRange <$> (P.char '^' *> P.spaces *> partialVersionParser)

    primitiveParser :: P.Parsec String () SimpleRangeExpression
    primitiveParser = Primitive <$> comparatorParser

-- | Parses a hyphen range: two partial versions separated by " - ".
-- See `hyphen` definition here: https://github.com/npm/node-semver#range-grammar
hyphenRangeParser :: P.Parsec String () ComparatorSet
hyphenRangeParser = do
  lower <- partialVersionParser
  _ <- hyphenRangeSeparatorParser
  upper <- partialVersionParser
  pure $ HyphenRange lower upper
  where
    -- Must must exactly 1 white space character around the hyphen.
    hyphenRangeSeparatorParser :: P.Parsec String () Char
    hyphenRangeSeparatorParser = P.space *> P.char '-' <* P.space
