module Wasp.SemanticVersion
  ( Version (..),
    Operator (..),
    Comparator (..),
    ComparatorSet (..),
    Range (..),
    isVersionInRange,
    rangeFromVersion,
    rangeFromVersionsIntersection,
  )
where

import Data.List (intercalate)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Numeric.Natural
import Text.Printf (printf)

-- Implements SemVer (semantic versioning) by following spec from https://github.com/npm/node-semver .

data Version = Version
  { major :: Natural,
    minor :: Natural,
    patch :: Natural
  }
  deriving (Eq, Ord)

-- | We rely on `show` here to produce valid semver representation of version.
instance Show Version where
  show (Version mjr mnr ptc) = printf "%d.%d.%d" mjr mnr ptc

data Operator
  = Equal
  | LessThanOrEqual
  | -- | TODO: BackwardsCompatibleWith (^) is actually, by semver spec, a special "range" which desugarizes into basic
    -- comparators.
    -- Same goes for other special ranges like Hyphen, v.x.x, ~, ... . They are all sugars that can be expressed with basic
    -- comparators.
    -- Although they call them special "ranges", since they all desugarize into comparator sets (no ||), we can also
    -- more strictly treat them as special "comparator sets".
    -- Therefore, we should remove ^ as an operator from here and instead extend ComparatorSet as:
    -- data ComparatorSet = Regular (NonEmpty Comparator) | Special ComparatorSetSpecial
    -- data ComparatorSetSpecial = BackwardsCompatibleWith Version | Hyphen Version Version | ...
    BackwardsCompatibleWith
  deriving (Eq)

-- | We rely on `show` here to produce valid semver representation of operator.
instance Show Operator where
  show Equal = "="
  show LessThanOrEqual = "<="
  show BackwardsCompatibleWith = "^"

data Comparator = Comparator Operator Version
  deriving (Eq)

-- | We rely on `show` here to produce valid semver representation of comparator.
instance Show Comparator where
  show (Comparator op v) = show op ++ show v

data ComparatorSet = ComparatorSet (NonEmpty Comparator)
  deriving (Eq)

-- | We rely on `show` here to produce valid semver representation of comparator set.
instance Show ComparatorSet where
  show (ComparatorSet comps) = unwords $ show <$> NE.toList comps

data Range = Range [ComparatorSet]
  deriving (Eq)

-- | We rely on `show` here to produce valid semver representation of version range.
instance Show Range where
  show (Range compSets) = intercalate " || " $ show <$> compSets

-- | We define concatenation of two version ranges as union of their comparator sets.
instance Semigroup Range where
  (Range csets1) <> (Range csets2) = Range $ csets1 <> csets2

instance Monoid Range where
  mempty = Range []

nextBreakingChangeVersion :: Version -> Version
nextBreakingChangeVersion version = case version of
  (Version 0 0 x) -> Version 0 0 (succ x)
  (Version 0 x _) -> Version 0 (succ x) 0
  (Version x _ _) -> Version (succ x) 0 0

doesVersionSatisfyComparator :: Version -> Comparator -> Bool
doesVersionSatisfyComparator version (Comparator operator compVersion) = case operator of
  Equal -> version == compVersion
  LessThanOrEqual -> version <= compVersion
  BackwardsCompatibleWith -> version >= compVersion && version < nextBreakingChangeVersion compVersion

doesVersionSatisfyComparatorSet :: Version -> ComparatorSet -> Bool
doesVersionSatisfyComparatorSet version (ComparatorSet comps) = all (doesVersionSatisfyComparator version) comps

isVersionInRange :: Version -> Range -> Bool
isVersionInRange version (Range compSets) = any (doesVersionSatisfyComparatorSet version) compSets

rangeFromVersion :: (Operator, Version) -> Range
rangeFromVersion = Range . pure . ComparatorSet . pure . uncurry Comparator

rangeFromVersionsIntersection :: [(Operator, Version)] -> Range
rangeFromVersionsIntersection [] = Range []
rangeFromVersionsIntersection compPairs = Range $ pure $ ComparatorSet $ uncurry Comparator <$> NE.fromList compPairs
