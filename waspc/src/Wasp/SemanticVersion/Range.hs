{-# LANGUAGE DeriveLift #-}

module Wasp.SemanticVersion.Range
  ( Range (..),
    parseRange,
    rangeParser,
    isVersionInRange,
    doesVersionRangeAllowMajorChanges,
    lt,
    lte,
    gt,
    gte,
    eq,
    caretRange,
    tildeRange,
    backwardsCompatibleWith,
    approximatelyEquivalentTo,
    hyphenRange,
    r,
  )
where

import Control.Monad (guard, void)
import Data.List (intercalate, nub)
import qualified Data.List.NonEmpty as NE
import Data.Maybe (isJust)
import qualified Language.Haskell.TH.Quote as TH
import qualified Language.Haskell.TH.Syntax as TH
import qualified Text.Parsec as P
import Wasp.SemanticVersion.Comparator (Comparator (..), PrimitiveOperator (..))
import Wasp.SemanticVersion.ComparatorSet (ComparatorSet (..), SimpleRangeExpression (..), comparatorSetParser)
import Wasp.SemanticVersion.PartialVersion (PartialVersion (..), fromVersion)
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
import Wasp.Util.TH (quasiQuoterFromParser)

-- | Comparator sets can be joined by "||" to form a range,
-- which is satisfied by satisfying any of the comparator sets it includes.
data Range = Range [ComparatorSet]
  deriving (Eq, TH.Lift)

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

-- Helper methods for constructing a 'Range'.

caretRange :: Version -> Range
caretRange = Range . pure . SimpleComparatorSet . NE.fromList . pure . CaretRange . fromVersion

backwardsCompatibleWith :: Version -> Range
backwardsCompatibleWith = caretRange

tildeRange :: Version -> Range
tildeRange = Range . pure . SimpleComparatorSet . NE.fromList . pure . TildeRange . fromVersion

approximatelyEquivalentTo :: Version -> Range
approximatelyEquivalentTo = tildeRange

hyphenRange :: Version -> Version -> Range
hyphenRange v1 v2 = Range [HyphenRange (fromVersion v1) (fromVersion v2)]

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
mkComparatorRange op = Range . pure . SimpleComparatorSet . NE.fromList . pure . Primitive . Comparator op . fromVersion

r :: TH.QuasiQuoter
r = quasiQuoterFromParser parseRange

parseRange :: String -> Either P.ParseError Range
parseRange = P.parse (rangeParser <* P.eof) ""

-- | Parses a version range.
-- See `range-set` definition here: https://github.com/npm/node-semver#range-grammar
rangeParser :: P.Parsec String () Range
rangeParser = Range <$> rangeSetParser
  where
    rangeSetParser :: P.Parsec String () [ComparatorSet]
    rangeSetParser = do
      first <- rangeExpressionParser
      rest <- P.many $ P.try (logicalOrParser *> rangeExpressionParser)
      pure (first : rest)

    rangeExpressionParser :: P.Parsec String () ComparatorSet
    rangeExpressionParser = P.spaces *> (comparatorSetParser P.<|> emptyRangeParser) <* P.spaces

    -- `node-semver` parses empty input as the equals any comparator (*).
    emptyRangeParser :: P.Parsec String () ComparatorSet
    emptyRangeParser = (SimpleComparatorSet . pure . Primitive $ Comparator Equal Any) <$ P.eof

    logicalOrParser :: P.Parsec String () ()
    logicalOrParser = void (P.spaces *> P.string "||" <* P.spaces)
