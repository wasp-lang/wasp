module Wasp.SemanticVersion.Range
  ( Range (..),
    parseRange,
    rangeParser,
    isVersionInRange,
    doesVersionRangeAllowMajorChanges,
  )
where

import Control.Monad (guard)
import Data.List (intercalate, nub)
import Data.Maybe (isJust)
import qualified Text.Parsec as P
import Wasp.SemanticVersion.ComparatorSet (ComparatorSet (..), Simple (..), comparatorSetParser)
import Wasp.SemanticVersion.PartialVersion (PartialVersion (..))
import Wasp.SemanticVersion.Version (Version (..), nextBreakingChangeVersion)
import Wasp.SemanticVersion.VersionBound
  ( HasVersionBounds (versionBounds),
    VersionBound (..),
    allVersionsInterval,
    intervalUnion,
    isSubintervalOf,
    isVersionInInterval,
    versionFromBound,
  )

-- | Comparator sets can be joined by "||" to form a range,
-- which is satisfied by satisfying any of the comparator sets it includes.
-- See: https://github.com/npm/node-semver#ranges
data Range = Range [ComparatorSet]
  deriving (Eq)

-- | We rely on this 'show' implementation to produce valid `node-semver` range.
instance Show Range where
  show (Range compSets) = intercalate " || " $ show <$> compSets

-- | We define concatenation of two version ranges as a union of their comparator sets.
instance Semigroup Range where
  (Range csets1) <> (Range csets2) = Range $ nub $ csets1 <> csets2

instance Monoid Range where
  mempty = Range []

instance HasVersionBounds Range where
  versionBounds (Range []) = allVersionsInterval
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

parseRange :: String -> Either P.ParseError Range
parseRange = P.parse rangeParser ""

-- See `range-set` definition here: https://github.com/npm/node-semver#range-grammar
rangeParser :: P.Parsec String () Range
rangeParser =
  P.choice
    [ P.try emptyRangeParser,
      Range <$> (comparatorSetParser `P.sepBy1` P.try logicalOrParser)
    ]
  where
    -- `node-semver` allows parsing of an empty string into the x-range any comparator (*).
    emptyRangeParser :: P.Parsec String () Range
    emptyRangeParser = Range [SimpleComparatorSet (pure (XRange Any))] <$ P.eof

    logicalOrParser :: P.Parsec String () ()
    logicalOrParser = P.spaces *> P.string "||" *> P.spaces
