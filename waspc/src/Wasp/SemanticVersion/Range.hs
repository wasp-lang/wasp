{-# LANGUAGE DeriveLift #-}

module Wasp.SemanticVersion.Range where

import Control.Monad (guard, void)
import Data.List (intercalate)
import qualified Data.List.NonEmpty as NE
import Data.Maybe (isJust)
import qualified Language.Haskell.TH.Quote as TH
import qualified Language.Haskell.TH.Syntax as TH
import qualified Text.Parsec as P
import Wasp.SemanticVersion.PartialVersion (versionToPartialVersion)
import Wasp.SemanticVersion.RangeExpression (PrimitiveOperator (..), RangeExpression (..), SimpleRangeExpression (..), rangeParser)
import Wasp.SemanticVersion.Version (Version (..), nextBreakingChangeVersion)
import Wasp.SemanticVersion.VersionBound
  ( HasVersionBounds (versionBounds),
    VersionBound (..),
    intervalUnion,
    isSubintervalOf,
    isVersionInInterval,
    versionFromBound,
  )
import Wasp.Util.TH (quasiQuoterFromParser)

-- | Comparator sets can be joined by "||" to form a range,
-- which is satisfied by satisfying any of the comparator sets it includes.
data Range = Range (NE.NonEmpty RangeExpression)
  deriving (Eq, TH.Lift)

-- | We rely on this 'show' implementation to produce valid a `node-semver` output.
instance Show Range where
  show (Range rangeExpressions) = intercalate " || " (show <$> NE.toList rangeExpressions)

-- | We define concatenation of two version ranges as a union of their comparator sets.
instance Semigroup Range where
  (Range csets1) <> (Range csets2) = Range $ NE.nub $ csets1 <> csets2

instance HasVersionBounds Range where
  versionBounds (Range compSets) = foldr1 intervalUnion $ versionBounds <$> compSets

isVersionInRange :: Version -> Range -> Bool
isVersionInRange version (Range compSets) = any (doesVersionSatisfyComparatorSet version) compSets

doesVersionSatisfyComparatorSet :: Version -> RangeExpression -> Bool
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

-- Helper methods for constructing a 'Range'.

caretRange :: Version -> Range
caretRange = Range . pure . Simple . NE.fromList . pure . CaretRange . versionToPartialVersion

backwardsCompatibleWith :: Version -> Range
backwardsCompatibleWith = caretRange

tildeRange :: Version -> Range
tildeRange = Range . pure . Simple . NE.fromList . pure . TildeRange . versionToPartialVersion

approximatelyEquivalentTo :: Version -> Range
approximatelyEquivalentTo = tildeRange

hyphenRange :: Version -> Version -> Range
hyphenRange v1 v2 = Range $ NE.fromList [HyphenRange (versionToPartialVersion v1) (versionToPartialVersion v2)]

lt :: Version -> Range
lt = mkComparatorRange LessThan

lte :: Version -> Range
lte = mkComparatorRange LessThanOrEqual

gt :: Version -> Range
gt = mkComparatorRange GreaterThan

gte :: Version -> Range
gte = mkComparatorRange GreaterThanOrEqual

eq :: Version -> Range
eq = mkComparatorRange Equal

mkComparatorRange :: PrimitiveOperator -> Version -> Range
mkComparatorRange op = Range . pure . Simple . NE.fromList . pure . Primitive op . versionToPartialVersion

r :: TH.QuasiQuoter
r = quasiQuoterFromParser parseRange

parseRange :: String -> Either P.ParseError Range
parseRange = P.parse (rangeSetParser <* P.eof) ""

-- | Parses a version range.
-- See `range-set` definition here: https://github.com/npm/node-semver#range-grammar
rangeSetParser :: P.Parsec String () Range
rangeSetParser = do
  first <- rangeSetItemParser
  rest <- P.many $ P.try (logicalOrParser *> rangeSetItemParser)
  pure $ Range $ NE.fromList (first : rest)
  where
    rangeSetItemParser :: P.Parsec String () RangeExpression
    rangeSetItemParser = P.spaces *> rangeParser <* P.spaces

    logicalOrParser :: P.Parsec String () ()
    logicalOrParser = void (P.spaces *> P.string "||" <* P.spaces)
