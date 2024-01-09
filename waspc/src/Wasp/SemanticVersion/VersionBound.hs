{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE FlexibleInstances #-}

module Wasp.SemanticVersion.VersionBound
  ( HasVersionBounds (..),
    VersionBound (..),
    VersionInterval,
    versionFromBound,
    intervalIntersection,
    intervalUnion,
    isSubintervalOf,
    isVersionInInterval,
    showInterval,
    parseInterval,
    intervalParser,
    vi,
  )
where

import qualified Language.Haskell.TH.Quote as TH
import qualified Language.Haskell.TH.Syntax as TH
import qualified Text.Parsec as P
import Wasp.SemanticVersion.Version (Version, versionParser)
import Wasp.Util.TH (quasiQuoterFromParser)

data VersionBound = Inclusive !Version | Exclusive !Version | Inf
  deriving (Eq, Show, TH.Lift)

versionFromBound :: VersionBound -> Maybe Version
versionFromBound Inf = Nothing
versionFromBound (Inclusive v) = Just v
versionFromBound (Exclusive v) = Just v

-- | Interval of versions represented by the lower and upper bound.
-- Interval might be continuous or discontinuous, we don't know,
-- we know only its lower and upper bound.
type VersionInterval = (VersionBound, VersionBound)

showInterval :: VersionInterval -> String
showInterval (lowerBound, upperBound) =
  let lbStr = case lowerBound of
        Inf -> "(inf"
        Inclusive v -> '[' : show v
        Exclusive v -> '(' : show v
      ubStr = case upperBound of
        Inf -> "inf)"
        Inclusive v -> show v <> "]"
        Exclusive v -> show v <> ")"
   in lbStr <> ", " <> ubStr

parseInterval :: String -> Either P.ParseError VersionInterval
parseInterval = P.parse intervalParser ""

intervalParser :: P.Parsec String () VersionInterval
intervalParser = do
  _ <- P.spaces
  lowerBound <-
    P.choice $
      P.try
        <$> [ P.char '(' >> P.spaces >> P.string "inf" >> return Inf,
              P.char '(' >> P.spaces >> Exclusive <$> versionParser,
              P.char '[' >> P.spaces >> Inclusive <$> versionParser
            ]
  _ <- P.spaces >> P.char ',' >> P.spaces
  upperBound <-
    P.choice $
      P.try
        <$> [ P.string "inf" >> P.spaces >> P.char ')' >> return Inf,
              versionParser >>= \v -> P.spaces >> P.char ')' >> return (Exclusive v),
              versionParser >>= \v -> P.spaces >> P.char ']' >> return (Inclusive v)
            ]
  return (lowerBound, upperBound)

vi :: TH.QuasiQuoter
vi = quasiQuoterFromParser parseInterval

class HasVersionBounds a where
  -- | Returns lower and upper version bounds of the interval of valid versions.
  -- Interval of valid versions defined by these bounds may or may not be discontinous.
  versionBounds :: a -> VersionInterval

-- | NOTE: Returned interval might be "empty", if lower bound is higher than the upper bound.
-- TODO: Maybe return Nothing in such case? Right now we leave it to consumer of this function
-- to check if interval is non-empty, if they even care about that.
intervalIntersection :: VersionInterval -> VersionInterval -> VersionInterval
intervalIntersection (lb1, ub1) (lb2, ub2) = (maxLowerBound lb1 lb2, minUpperBound ub1 ub2)

intervalUnion :: VersionInterval -> VersionInterval -> VersionInterval
intervalUnion (lb1, ub1) (lb2, ub2) = (minLowerBound lb1 lb2, maxUpperBound ub1 ub2)

isSubintervalOf :: VersionInterval -> VersionInterval -> Bool
isSubintervalOf vi1 vi2 = intervalIntersection vi1 vi2 == vi1

isVersionInInterval :: VersionInterval -> Version -> Bool
isVersionInInterval interval version = interval == intervalUnion interval (Inclusive version, Inclusive version)

-- Takes two lower bounds and returns the bigger one.
maxLowerBound :: VersionBound -> VersionBound -> VersionBound
maxLowerBound = stricterBound (>)

-- Takes two upper bounds and returns the smaller one.
minUpperBound :: VersionBound -> VersionBound -> VersionBound
minUpperBound = stricterBound (<)

-- Takes two lower bounds and returns the smaller one.
minLowerBound :: VersionBound -> VersionBound -> VersionBound
minLowerBound = looserBound (>)

-- Takes two upper bounds and returns the bigger one.
maxUpperBound :: VersionBound -> VersionBound -> VersionBound
maxUpperBound = looserBound (<)

-- | Takes two bounds (either both upper or both lower) and returns the stricter one.
-- Examples:
--  - `stricterBound (3.0 (5.0 == (5.0`
--  - `stricterBound 3.0) 5.0] == 3.0)`
--  - `stricterBound 0.1] Inf == 0.1]`
stricterBound :: (Version -> Version -> Bool) -> VersionBound -> VersionBound -> VersionBound
stricterBound isStricterThan vb1 vb2 = fst $ orderBounds isStricterThan vb1 vb2

-- | Same like `stricterBound` but returns the other, looser bound.
looserBound :: (Version -> Version -> Bool) -> VersionBound -> VersionBound -> VersionBound
looserBound isStricterThan vb1 vb2 = snd $ orderBounds isStricterThan vb1 vb2

-- | Given two bounds (either both upper or both lower), returns these two bounds ordered so
-- that the first one is stricter and the second one is looser.
orderBounds :: (Version -> Version -> Bool) -> VersionBound -> VersionBound -> (VersionBound, VersionBound)
orderBounds isStricterThan vb1 vb2 = go vb1 vb2
  where
    go lb1@(Inclusive v1) lb2@(Inclusive v2) = if v1 `isStricterThan` v2 then (lb1, lb2) else (lb2, lb1)
    go lb1@(Exclusive v1) lb2@(Exclusive v2) = if v1 `isStricterThan` v2 then (lb1, lb2) else (lb2, lb1)
    go lb1@(Exclusive v1) lb2@(Inclusive v2) = if v2 `isStricterThan` v1 then (lb2, lb1) else (lb1, lb2)
    go lb1@(Inclusive _) lb2@(Exclusive _) = go lb2 lb1
    go lb1 Inf = (lb1, Inf)
    go Inf lb2 = (lb2, Inf)
