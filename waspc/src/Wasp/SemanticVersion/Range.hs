{-# LANGUAGE DeriveLift #-}

module Wasp.SemanticVersion.Range
  ( Range (..),
    parseRange,
    rangeParser,
    isVersionInRange,
    doesVersionRangeAllowMajorChanges,
    r,
  )
where

import Control.Monad (guard)
import Data.List (intercalate, nub)
import Data.Maybe (isJust)
import qualified Language.Haskell.TH.Quote as TH
import qualified Language.Haskell.TH.Syntax as TH
import qualified Text.Parsec as P
import Wasp.SemanticVersion.Comparator (Comparator (..), PrimitiveOperator (Equal))
import Wasp.SemanticVersion.ComparatorSet (ComparatorSet (..), SimpleRangeExpression (..), comparatorSetParser)
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

r :: TH.QuasiQuoter
r = quasiQuoterFromParser parseRange

parseRange :: String -> Either P.ParseError Range
parseRange = P.parse rangeParser ""

-- See `range-set` definition here: https://github.com/npm/node-semver#range-grammar
rangeParser :: P.Parsec String () Range
rangeParser =
  P.choice
    [ nonEmptyRangeParser,
      emptyRangeParser
    ]
  where
    -- `node-semver` allows parsing of an empty string into the any comparator (*).
    emptyRangeParser :: P.Parsec String () Range
    emptyRangeParser = Range [SimpleComparatorSet (pure (Primitive $ Comparator Equal Any))] <$ P.eof

    nonEmptyRangeParser :: P.Parsec String () Range
    nonEmptyRangeParser = Range <$> (comparatorSetParser `P.sepBy1` P.try logicalOrParser)

    logicalOrParser :: P.Parsec String () ()
    logicalOrParser = P.spaces *> P.string "||" *> P.spaces
