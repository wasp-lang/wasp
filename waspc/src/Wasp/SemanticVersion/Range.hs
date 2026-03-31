{-# LANGUAGE DeriveLift #-}

module Wasp.SemanticVersion.Range
  ( Range (..),
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
    parseRange,
    rangeSetParser,
  )
where

import Control.Monad (guard, void)
import Data.List (intercalate)
import qualified Data.List.NonEmpty as NE
import Data.Maybe (isJust)
import qualified Language.Haskell.TH.Quote as TH
import qualified Language.Haskell.TH.Syntax as TH
import qualified Text.Parsec as P
import Wasp.SemanticVersion.PartialVersion (versionToPartialVersion)
import Wasp.SemanticVersion.RangeExpression (PrimitiveOperator (..), RangeExpression (..), SimpleRangeExpression (..), rangeExpressionParser)
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

-- | A range is composed of one or more range expressions.
data Range = Range (NE.NonEmpty RangeExpression)
  deriving (Eq, TH.Lift)

-- | We rely on this 'show' implementation to produce valid a `node-semver` output.
instance Show Range where
  show (Range rangeExpressions) = intercalate " || " (show <$> NE.toList rangeExpressions)

-- | We define concatenation of two version ranges as a union of their range expressions.
instance Semigroup Range where
  (Range left) <> (Range right) = Range $ NE.nub $ left <> right

instance HasVersionBounds Range where
  versionBounds (Range rangeExpressions) = foldr1 intervalUnion $ versionBounds <$> rangeExpressions

isVersionInRange :: Version -> Range -> Bool
isVersionInRange version (Range rangeExpressions) = any isVersionInRangeExpression rangeExpressions
  where
    isVersionInRangeExpression :: RangeExpression -> Bool
    isVersionInRangeExpression rangeExpression =
      isVersionInInterval (versionBounds rangeExpression) version

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
caretRange = Range . pure . Simple . pure . CaretRange . versionToPartialVersion

backwardsCompatibleWith :: Version -> Range
backwardsCompatibleWith = caretRange

tildeRange :: Version -> Range
tildeRange = Range . pure . Simple . pure . TildeRange . versionToPartialVersion

approximatelyEquivalentTo :: Version -> Range
approximatelyEquivalentTo = tildeRange

hyphenRange :: Version -> Version -> Range
hyphenRange v1 v2 = Range $ pure $ HyphenRange (versionToPartialVersion v1) (versionToPartialVersion v2)

lt :: Version -> Range
lt = mkPrimitiveRange LessThan

lte :: Version -> Range
lte = mkPrimitiveRange LessThanOrEqual

gt :: Version -> Range
gt = mkPrimitiveRange GreaterThan

gte :: Version -> Range
gte = mkPrimitiveRange GreaterThanOrEqual

eq :: Version -> Range
eq = mkPrimitiveRange Equal

mkPrimitiveRange :: PrimitiveOperator -> Version -> Range
mkPrimitiveRange op = Range . pure . Simple . pure . Primitive op . versionToPartialVersion

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
    rangeSetItemParser = P.spaces *> rangeExpressionParser <* P.spaces

    logicalOrParser :: P.Parsec String () ()
    logicalOrParser = void (P.spaces *> P.string "||" <* P.spaces)
