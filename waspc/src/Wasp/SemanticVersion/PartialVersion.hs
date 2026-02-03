{-# LANGUAGE DeriveLift #-}

module Wasp.SemanticVersion.PartialVersion
  ( PartialVersion (..),
    fromVersion,
    parsePartialVersion,
    partialVersionParser,
    toLowerBound,
    toUpperBound,
    toTildeUpperBound,
    toCaretUpperBound,
    pv,
  )
where

import qualified Language.Haskell.TH.Quote as TH
import qualified Language.Haskell.TH.Syntax as TH
import Numeric.Natural (Natural)
import Text.Parsec (ParseError, Parsec, char, choice, digit, many1, oneOf, optional, parse, try)
import Text.Printf (printf)
import Wasp.SemanticVersion.Version (Version (..))
import Wasp.SemanticVersion.VersionBound (VersionBound (..))
import Wasp.Util.TH (quasiQuoterFromParser)

-- | Version representation used in range expressions.
-- Unlike 'Version' which always has all 3 components,
-- 'PartialVersion' can represent partial/wildcard versions as they appear in ranges.
data PartialVersion
  = -- | Major, minor and patch (1.2.3).
    PVFull !Natural !Natural !Natural
  | -- | Major and minor only (1.2 meaning 1.2.x).
    PVMajorMinor !Natural !Natural
  | -- | Major only (1 meaning 1.x.x).
    PVMajorOnly !Natural
  | -- | No version value (*, x or X, meaning x.x.x).
    PVAny
  deriving (Eq, TH.Lift)

fromVersion :: Version -> PartialVersion
fromVersion (Version m n p) = PVFull m n p

instance Show PartialVersion where
  show (PVFull m n p) = printf "%d.%d.%d" m n p
  show (PVMajorMinor m n) = printf "%d.%d" m n
  show (PVMajorOnly m) = printf "%d" m
  show PVAny = "*"

parsePartialVersion :: String -> Either ParseError PartialVersion
parsePartialVersion = parse partialVersionParser ""

-- | Parser for PartialVersion. Does NOT consume trailing whitespace.
partialVersionParser :: Parsec String () PartialVersion
partialVersionParser = choice [try fullParser, try majorMinorParser, try majorOnlyParser, anyParser]
  where
    fullParser = do
      m <- naturalP
      _ <- char '.'
      n <- naturalP
      _ <- char '.'
      p <- naturalP
      return $ PVFull m n p

    majorMinorParser = do
      m <- naturalP
      _ <- char '.'
      -- Could be a number or x/*
      n <-
        choice
          [ try (xParser >> return Nothing),
            Just <$> naturalP
          ]
      -- Check for optional trailing .x or .*
      optional (try $ char '.' >> xParser)
      case n of
        Nothing -> return $ PVMajorOnly m -- 1.x or 1.*
        Just mnr -> return $ PVMajorMinor m mnr

    majorOnlyParser = do
      m <- naturalP
      -- Check for optional trailing .x.x or .*.* patterns
      optional (try $ char '.' >> xParser >> optional (try $ char '.' >> xParser))
      return $ PVMajorOnly m

    anyParser = do
      _ <- xParser
      return PVAny

    xParser = oneOf "xX*" >> return ()

    naturalP :: Parsec String () Natural
    naturalP = read <$> many1 digit

pv :: TH.QuasiQuoter
pv = quasiQuoterFromParser parsePartialVersion

-- | Convert a 'PartialVersion' to its lower bound 'Version'.
-- For partial versions, missing components default to 0.
toLowerBound :: PartialVersion -> Version
toLowerBound PVAny = Version 0 0 0
toLowerBound (PVMajorOnly m) = Version m 0 0
toLowerBound (PVMajorMinor m n) = Version m n 0
toLowerBound (PVFull m n p) = Version m n p

-- | Convert a PartialVersion to its exclusive upper bound for X-ranges.
-- For full versions, this returns an Inclusive bound (exact match).
-- For partial versions, returns Exclusive bound of next increment.
toUpperBound :: PartialVersion -> VersionBound
toUpperBound PVAny = Inf
toUpperBound (PVMajorOnly m) = Exclusive (Version (m + 1) 0 0)
toUpperBound (PVMajorMinor m n) = Exclusive (Version m (n + 1) 0)
toUpperBound (PVFull m n p) = Inclusive (Version m n p)

-- | Convert a PartialVersion to its exclusive upper bound for approximatelyEquvivalentTo ranges (~).
-- Tilde allows patch-level changes if minor is specified.
-- ~1.2.3 -> <1.3.0, ~1.2 -> <1.3.0, ~1 -> <2.0.0
toTildeUpperBound :: PartialVersion -> VersionBound
toTildeUpperBound PVAny = Inf
toTildeUpperBound (PVMajorOnly m) = Exclusive (Version (m + 1) 0 0)
toTildeUpperBound (PVMajorMinor m n) = Exclusive (Version m (n + 1) 0)
toTildeUpperBound (PVFull m n _) = Exclusive (Version m (n + 1) 0)

-- | Convert a PartialVersion to its exclusive upper bound for caret ranges (^).
-- Caret allows changes that don't modify the leftmost non-zero digit.
-- ^1.2.3 -> <2.0.0, ^0.2.3 -> <0.3.0, ^0.0.3 -> <0.0.4
-- ^1.2 -> <2.0.0, ^0.2 -> <0.3.0, ^0.0 -> <0.1.0
-- ^1 -> <2.0.0, ^0 -> <1.0.0
toCaretUpperBound :: PartialVersion -> VersionBound
toCaretUpperBound PVAny = Inf
toCaretUpperBound (PVMajorOnly 0) = Exclusive (Version 1 0 0)
toCaretUpperBound (PVMajorOnly m) = Exclusive (Version (m + 1) 0 0)
toCaretUpperBound (PVMajorMinor 0 0) = Exclusive (Version 0 1 0)
toCaretUpperBound (PVMajorMinor 0 n) = Exclusive (Version 0 (n + 1) 0)
toCaretUpperBound (PVMajorMinor m _) = Exclusive (Version (m + 1) 0 0)
toCaretUpperBound (PVFull 0 0 p) = Exclusive (Version 0 0 (p + 1))
toCaretUpperBound (PVFull 0 n _) = Exclusive (Version 0 (n + 1) 0)
toCaretUpperBound (PVFull m _ _) = Exclusive (Version (m + 1) 0 0)
