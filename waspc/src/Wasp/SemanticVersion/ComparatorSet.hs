{-# HLINT ignore "Use <$>" #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Wasp.SemanticVersion.ComparatorSet
  ( ComparatorSet (..),
    Simple (..),
    comparatorSetParser,
    simpleParser,
    hyphenRangeParser,
    lt,
    lte,
    gt,
    gte,
    eq,
    backwardsCompatibleWith,
    approximatelyEquivalentTo,
    hyphenRange,
  )
where

import qualified Data.List.NonEmpty as NE
import qualified Text.Parsec as P
import Wasp.SemanticVersion.Comparator
  ( Comparator (..),
    PrimitiveOperator (..),
    primitiveComparatorParser,
    toXRangeLowerBound,
    toXRangeUpperBound,
  )
import Wasp.SemanticVersion.PartialVersion (PartialVersion (..), fromVersion, partialVersionParser)
import Wasp.SemanticVersion.Version (Version (..))
import Wasp.SemanticVersion.VersionBound
  ( HasVersionBounds (versionBounds),
    VersionBound (..),
    intervalIntersection,
  )

-- | A comparator set is either a sequence of simple comparators (AND logic)
-- or a hyphen range.
-- See `range` definition here: https://github.com/npm/node-semver#range-grammar
data ComparatorSet
  = SimpleComparatorSet (NE.NonEmpty Simple)
  | HyphenRange PartialVersion PartialVersion
  deriving (Eq)

-- | A simple: primitive, tilde, caret, or x-range.
-- See `simple` definition here: https://github.com/npm/node-semver#range-grammar
data Simple
  = -- | 1.2.3 (=1.2.3), >1.2.3, <1.2.3, >=1.2.3, <=1.2.3
    Primitive Comparator
  | -- | ~1.2.3
    Tilde PartialVersion
  | -- | ^1.2.3
    Caret PartialVersion
  deriving (Eq)

-- | We rely on this 'show' implementation to produce valid `node-semver` comparator set.
instance Show ComparatorSet where
  show (SimpleComparatorSet simples) = unwords $ show <$> NE.toList simples
  show (HyphenRange pv1 pv2) = show pv1 ++ " - " ++ show pv2

-- | We rely on this 'show' implementation to produce valid `node-semver` simple comparator.
instance Show Simple where
  show (Primitive comp) = show comp
  show (Tilde pv) = "~" ++ show pv
  show (Caret pv) = "^" ++ show pv

-- | We define concatenation of two comparator sets as a union of their simples.
-- Only valid for SimpleComparatorSets.
instance Semigroup ComparatorSet where
  (SimpleComparatorSet left) <> (SimpleComparatorSet right) = SimpleComparatorSet $ NE.nub $ left <> right
  _ <> _ = error "Cannot combine HyphenRange with other comparator sets"

instance HasVersionBounds ComparatorSet where
  versionBounds (SimpleComparatorSet simples) = foldr1 intervalIntersection $ versionBounds <$> simples
  versionBounds (HyphenRange lower upper) = (toXRangeLowerBound lower, toXRangeUpperBound upper)

instance HasVersionBounds Simple where
  versionBounds (Primitive comp) = versionBounds comp
  versionBounds (Caret pv) =
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
      toCaretUpperBound (MajorMinorPatch 0 0 ptc) = Exclusive (Version 0 0 (ptc + 1))
      toCaretUpperBound (MajorMinorPatch 0 mnr _) = Exclusive (Version 0 (mnr + 1) 0)
      toCaretUpperBound (MajorMinorPatch mjr _ _) = Exclusive (Version (mjr + 1) 0 0)
  versionBounds (Tilde pv) =
    (toXRangeLowerBound pv, toTildeUpperBound pv)
    where
      -- Tilde allows patch-level changes if minor is specified.
      toTildeUpperBound :: PartialVersion -> VersionBound
      toTildeUpperBound Any = Inf
      toTildeUpperBound (Major mjr) = Exclusive (Version (mjr + 1) 0 0)
      toTildeUpperBound (MajorMinor mjr mnr) = Exclusive (Version mjr (mnr + 1) 0)
      toTildeUpperBound (MajorMinorPatch mjr mnr _) = Exclusive (Version mjr (mnr + 1) 0)

-- Helper methods for constructing a 'ComparatorSet'.
-- While 'Comparator' works with 'PartialVersion' internally, we only use it through 'Version' in our code.
-- To create 'PartialVersion' comparator sets, please use the 'ComparatorSet' constructors directly.

lt :: Version -> ComparatorSet
lt = mkPrimCompSet LessThan

lte :: Version -> ComparatorSet
lte = mkPrimCompSet LessThanOrEqual

gt :: Version -> ComparatorSet
gt = mkPrimCompSet GreaterThan

gte :: Version -> ComparatorSet
gte = mkPrimCompSet GreaterThanOrEqual

eq :: Version -> ComparatorSet
eq = mkPrimCompSet Equal

backwardsCompatibleWith :: Version -> ComparatorSet
backwardsCompatibleWith = SimpleComparatorSet . pure . Caret . fromVersion

approximatelyEquivalentTo :: Version -> ComparatorSet
approximatelyEquivalentTo = SimpleComparatorSet . pure . Tilde . fromVersion

hyphenRange :: Version -> Version -> ComparatorSet
hyphenRange v1 v2 = HyphenRange (fromVersion v1) (fromVersion v2)

mkPrimCompSet :: PrimitiveOperator -> Version -> ComparatorSet
mkPrimCompSet op = SimpleComparatorSet . pure . Primitive . Comparator op . fromVersion

-- | Parses a single non-hyphen comparator (primitive, tilde, caret, or x-range).
-- See `simple` definition here: https://github.com/npm/node-semver#range-grammar
simpleParser :: P.Parsec String () Simple
simpleParser =
  P.choice
    [ tildeParser,
      caretParser,
      primitiveParser
    ]
  where
    tildeParser :: P.Parsec String () Simple
    tildeParser = Tilde <$> (P.char '~' *> partialVersionParser)

    caretParser :: P.Parsec String () Simple
    caretParser = Caret <$> (P.char '^' *> partialVersionParser)

    primitiveParser :: P.Parsec String () Simple
    primitiveParser = Primitive <$> primitiveComparatorParser

-- | Parses a hyphen range: two partial versions separated by " - ".
-- See `hyphen` definition here: https://github.com/npm/node-semver#range-grammar
hyphenRangeParser :: P.Parsec String () ComparatorSet
hyphenRangeParser = do
  lowerVersion <- partialVersionParser
  _ <- P.space *> P.char '-' <* P.space
  upperVersion <- partialVersionParser
  pure $ HyphenRange lowerVersion upperVersion

-- | Parses a comparator set: either a single hyphen range or
-- one or more simple comparators separated by spaces.
-- See `range` definition here: https://github.com/npm/node-semver#range-grammar
-- NOTE: Grammar's `range` is our comparator set. And grammar's `range-set` is our range.
comparatorSetParser :: P.Parsec String () ComparatorSet
comparatorSetParser =
  P.choice
    [ P.try hyphenRangeParser,
      simpleComparatorSetParser
    ]
  where
    simpleComparatorSetParser :: P.Parsec String () ComparatorSet
    simpleComparatorSetParser = do
      first <- simpleParser
      rest <- P.many $ P.try (spacesBetweenComparatorsParser *> simpleParser)
      pure $ SimpleComparatorSet (NE.fromList (first : rest))

    -- Consumes whitespace only when it separates comparators.
    spacesBetweenComparatorsParser :: P.Parsec String () ()
    spacesBetweenComparatorsParser = do
      _ <- P.many1 P.space
      P.notFollowedBy (P.string "||")
      P.notFollowedBy P.eof
