module Wasp.SemanticVersion.ComparatorSet
  ( ComparatorSet (..),
    comparatorSetParser,
    lt,
    lte,
    gt,
    gte,
    eq,
    backwardsCompatibleWith,
    approximatelyEquvivalentTo,
    xRange,
    hyphenRange,
  )
where

import qualified Data.List.NonEmpty as NE
import Text.Parsec (Parsec)
import qualified Text.Parsec as P
import Wasp.SemanticVersion.Comparator
  ( Comparator (..),
    PrimitiveOperator (..),
    hyphenRangeComparatorParser,
    simpleComparatorParser,
  )
import Wasp.SemanticVersion.PartialVersion (fromVersion)
import Wasp.SemanticVersion.Version (Version)
import Wasp.SemanticVersion.VersionBound
  ( HasVersionBounds (versionBounds),
    intervalIntersection,
  )

-- | Comparators can be joined together to form a comparator set,
-- A comparator set is satisfied by the intersection of all comparators it includes.
-- See: https://github.com/npm/node-semver#ranges
data ComparatorSet = ComparatorSet (NE.NonEmpty Comparator)
  deriving (Eq)

-- | We rely on this 'show' implementation to produce valid `node-semver` comparator set.
instance Show ComparatorSet where
  show (ComparatorSet comps) = unwords $ show <$> NE.toList comps

-- | We define concatenation of two comparator sets as a union of their comparators.
instance Semigroup ComparatorSet where
  (ComparatorSet leftComps) <> (ComparatorSet rightComps) = ComparatorSet $ NE.nub $ leftComps <> rightComps

instance HasVersionBounds ComparatorSet where
  versionBounds (ComparatorSet comps) = foldr1 intervalIntersection $ versionBounds <$> comps

-- Helper methods for constructing a 'ComparatorSet'.
-- While 'Comparator' works with 'PartialVersion' internally, we only use it through 'Version' in our code.
-- To create 'PartialVersion' comparator sets, please use the 'ComparatorSet' constructor directly.

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
backwardsCompatibleWith = ComparatorSet . pure . BackwardsCompatibleWith . fromVersion

approximatelyEquvivalentTo :: Version -> ComparatorSet
approximatelyEquvivalentTo = ComparatorSet . pure . ApproximatelyEquivalentTo . fromVersion

xRange :: Version -> ComparatorSet
xRange = ComparatorSet . pure . XRange . fromVersion

hyphenRange :: Version -> Version -> ComparatorSet
hyphenRange rv1 rv2 = ComparatorSet . pure $ HyphenRange (fromVersion rv1) (fromVersion rv2)

mkPrimCompSet :: PrimitiveOperator -> Version -> ComparatorSet
mkPrimCompSet op = ComparatorSet . pure . PrimitiveComparator op . fromVersion

-- | Parses a comparator set: either a single hyphen range comparator or
-- one or more simple comparators separated by spaces.
-- See `range` definition here: https://github.com/npm/node-semver#range-grammar
-- NOTE: Grammar's `range` is our comparator set. And grammar's `range-set` is our range.
comparatorSetParser :: Parsec String () ComparatorSet
comparatorSetParser =
  P.choice
    [ hyphenRangeComparatorSetParser,
      simpleComparatorSetParser
    ]
  where
    hyphenRangeComparatorSetParser :: Parsec String () ComparatorSet
    hyphenRangeComparatorSetParser = ComparatorSet . pure <$> P.try hyphenRangeComparatorParser

    simpleComparatorSetParser :: Parsec String () ComparatorSet
    simpleComparatorSetParser = do
      first <- simpleComparatorParser
      rest <- P.many $ P.try (spacesBetweenComparatorsParser *> simpleComparatorParser)
      pure $ ComparatorSet (NE.fromList (first : rest))

    -- Consumes whitespace only when it separates comparators.
    spacesBetweenComparatorsParser :: Parsec String () ()
    spacesBetweenComparatorsParser = do
      _ <- P.many1 P.space
      P.notFollowedBy (P.string "||")
      P.notFollowedBy P.eof
