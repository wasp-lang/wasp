module Wasp.SemanticVersion.ComparatorSet
  ( ComparatorSet (..),
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
import Wasp.SemanticVersion.Comparator
  ( Comparator (..),
    PrimitiveOperator (..),
  )
import Wasp.SemanticVersion.PartialVersion (fromVersion)
import Wasp.SemanticVersion.Version (Version)
import Wasp.SemanticVersion.VersionBound
  ( HasVersionBounds (versionBounds),
    intervalIntersection,
  )

-- | 'Comparator' can be joined by whitespace to form a 'ComparatorSet',
-- which is satisfied by the intersection of all of the 'Comparator' it includes.
data ComparatorSet = ComparatorSet (NE.NonEmpty Comparator)
  deriving (Eq)

-- | We rely on this `show` implementation to produce valid semver representation of comparator set.
instance Show ComparatorSet where
  show (ComparatorSet comps) = unwords $ show <$> NE.toList comps

-- | We define concatenation of two 'ComparatorSet' as a union of their comparators.
instance Semigroup ComparatorSet where
  (ComparatorSet leftComps) <> (ComparatorSet rightComps) = ComparatorSet $ NE.nub $ leftComps <> rightComps

instance HasVersionBounds ComparatorSet where
  versionBounds (ComparatorSet comps) = foldr1 intervalIntersection $ versionBounds <$> comps

-- Helper methods for constructing 'ComparatorSet'.
-- While 'Comparator' works with 'PartialVersion' internally, we only ever use 'Version' in our code.
-- For 'PartialVersion' comparator sets, pelease use the 'ComparatorSet' constructor directly.

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
approximatelyEquvivalentTo = ComparatorSet . pure . ApproximatelyEquvivalentTo . fromVersion

xRange :: Version -> ComparatorSet
xRange = ComparatorSet . pure . XRange . fromVersion

hyphenRange :: Version -> Version -> ComparatorSet
hyphenRange rv1 rv2 = ComparatorSet . pure $ HyphenRange (fromVersion rv1) (fromVersion rv2)

mkPrimCompSet :: PrimitiveOperator -> Version -> ComparatorSet
mkPrimCompSet op = ComparatorSet . pure . PrimitiveComparator op . fromVersion
