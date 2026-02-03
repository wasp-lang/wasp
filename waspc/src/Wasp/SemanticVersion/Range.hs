module Wasp.SemanticVersion.Range
  ( Range (..),
    isVersionInRange,
    doesVersionRangeAllowMajorChanges,
  )
where

import Control.Monad (guard)
import Data.List (intercalate, nub)
import Data.Maybe (isJust)
import Wasp.SemanticVersion.ComparatorSet (ComparatorSet (..))
import Wasp.SemanticVersion.Version (Version, nextBreakingChangeVersion)
import Wasp.SemanticVersion.VersionBound
  ( HasVersionBounds (versionBounds),
    VersionBound (Exclusive, Inf),
    intervalUnion,
    isSubintervalOf,
    isVersionInInterval,
    versionFromBound,
  )

-- | A 'Range' is composed of one or more 'ComparatorSet', joined by "||".
-- A 'Version' matches a 'Range' if and only if every 'Comparator' in at least one
-- of the "||"-separated 'ComparatorSet' is satisfied by the 'Version'.
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

instance HasVersionBounds Range where
  versionBounds (Range []) = (Inf, Inf)
  versionBounds (Range compSets) = foldr1 intervalUnion $ versionBounds <$> compSets

isVersionInRange :: Version -> Range -> Bool
isVersionInRange version (Range compSets) = any (doesVersionSatisfyComparatorSet version) compSets

doesVersionSatisfyComparatorSet :: Version -> ComparatorSet -> Bool
doesVersionSatisfyComparatorSet version compSet =
  isVersionInInterval (versionBounds compSet) version

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
