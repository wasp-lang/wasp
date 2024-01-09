module Wasp.SemanticVersion
  ( Version (..),
    Range (..),
    ComparatorSet,
    isVersionInRange,
    lt,
    lte,
    gt,
    gte,
    eq,
    backwardsCompatibleWith,
    doesVersionRangeAllowMajorChanges,
  )
where

import Control.Monad (guard)
import Data.List (intercalate, nub)
import qualified Data.List.NonEmpty as NE
import Data.Maybe (isJust)
import Wasp.SemanticVersion.Version (Version (..), nextBreakingChangeVersion)
import Wasp.SemanticVersion.VersionBound
  ( HasVersionBounds (versionBounds),
    VersionBound (Exclusive, Inclusive, Inf),
    intervalIntersection,
    intervalUnion,
    isSubintervalOf,
    isVersionInInterval,
    versionFromBound,
  )

-- Implements SemVer (semantic versioning) by following spec from https://github.com/npm/node-semver .

data Operator
  = Equal
  | LessThan
  | LessThanOrEqual
  | GreaterThan
  | GreaterThanOrEqual
  deriving (Eq)

-- | We rely on this `show` implementation to produce valid semver representation of version.
instance Show Operator where
  show Equal = "="
  show LessThan = "<"
  show LessThanOrEqual = "<="
  show GreaterThan = ">"
  show GreaterThanOrEqual = ">="

data Comparator
  = PrimitiveComparator Operator Version
  | BackwardsCompatibleWith Version
  deriving (Eq)

-- | We rely on this `show` implementation to produce valid semver representation of comparator.
instance Show Comparator where
  show (PrimitiveComparator op v) = show op ++ show v
  show (BackwardsCompatibleWith v) = "^" ++ show v

data ComparatorSet = ComparatorSet (NE.NonEmpty Comparator)
  deriving (Eq)

-- | We rely on this `show` implementation to produce valid semver representation of comparator set.
instance Show ComparatorSet where
  show (ComparatorSet comps) = unwords $ show <$> NE.toList comps

-- | We define concatenation of two comparator sets as a union of their comparators.
instance Semigroup ComparatorSet where
  (ComparatorSet compsl) <> (ComparatorSet compsr) = ComparatorSet $ NE.nub $ compsl <> compsr

data Range = Range [ComparatorSet]
  deriving (Eq)

-- | We rely on this `show` implementation to produce valid semver representation of version range.
instance Show Range where
  show (Range compSets) = intercalate " || " $ show <$> compSets

-- | We define concatenation of two version ranges as a union of their comparator sets.
instance Semigroup Range where
  (Range csets1) <> (Range csets2) = Range $ nub $ csets1 <> csets2

instance Monoid Range where
  mempty = Range []

instance HasVersionBounds Comparator where
  versionBounds (PrimitiveComparator operator version) =
    case operator of
      Equal -> (Inclusive version, Inclusive version)
      LessThan -> (Inf, Exclusive version)
      LessThanOrEqual -> (Inf, Inclusive version)
      GreaterThan -> (Exclusive version, Inf)
      GreaterThanOrEqual -> (Inclusive version, Inf)
  versionBounds (BackwardsCompatibleWith version) =
    (Inclusive version, Exclusive $ nextBreakingChangeVersion version)

instance HasVersionBounds ComparatorSet where
  versionBounds (ComparatorSet comps) = foldr1 intervalIntersection $ versionBounds <$> comps

instance HasVersionBounds Range where
  versionBounds (Range []) = (Inf, Inf)
  versionBounds (Range compSets) = foldr1 intervalUnion $ versionBounds <$> compSets

isVersionInRange :: Version -> Range -> Bool
isVersionInRange version (Range compSets) = any (doesVersionSatisfyComparatorSet version) compSets

doesVersionSatisfyComparatorSet :: Version -> ComparatorSet -> Bool
doesVersionSatisfyComparatorSet version (ComparatorSet comps) =
  all (doesVersionSatisfyComparator version) comps

doesVersionSatisfyComparator :: Version -> Comparator -> Bool
doesVersionSatisfyComparator version comparator =
  isVersionInInterval (versionBounds comparator) version

-- Helper methods.

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
backwardsCompatibleWith = ComparatorSet . pure . BackwardsCompatibleWith

mkPrimCompSet :: Operator -> Version -> ComparatorSet
mkPrimCompSet op = ComparatorSet . pure . PrimitiveComparator op

doesVersionRangeAllowMajorChanges :: Range -> Bool
doesVersionRangeAllowMajorChanges = not . doesVersionRangeAllowOnlyMinorChanges
  where
    doesVersionRangeAllowOnlyMinorChanges versionRange = isJust $ do
      let versionInterval = versionBounds versionRange
      let lowerBound = fst versionInterval
      lowerBoundVersion <- versionFromBound lowerBound
      let noMajorChangesInterval =
            (lowerBound, Exclusive $ nextBreakingChangeVersion lowerBoundVersion)
      guard $ versionInterval `isSubintervalOf` noMajorChangesInterval
