{-# LANGUAGE DeriveLift #-}

module Wasp.SemanticVersion.PartialVersion
  ( PartialVersion (..),
    fromVersion,
    parsePartialVersion,
    partialVersionParser,
    toLowerBoundVersion,
    toUpperBound,
    toTildeUpperBound,
    toCaretUpperBound,
    pv,
  )
where

import Control.Applicative ((<|>))
import Control.Monad (void)
import Data.Maybe (catMaybes)
import qualified Language.Haskell.TH.Quote as TH
import qualified Language.Haskell.TH.Syntax as TH
import Numeric.Natural (Natural)
import Text.Parsec (ParseError, Parsec, char, oneOf, optionMaybe, parse, try)
import Text.Printf (printf)
import Wasp.SemanticVersion.Parsers (noLeadingZeroNaturalP)
import Wasp.SemanticVersion.Version (Version (..))
import Wasp.SemanticVersion.VersionBound (VersionBound (..))
import Wasp.Util.TH (quasiQuoterFromParser)

-- | Version representation used in `node-semver` range expressions.
-- Unlike 'Version' which always has all 3 components,
-- 'PartialVersion' can represent partial/wildcard versions as they appear in range.
data PartialVersion
  = -- | Major, minor and patch version (e.g. 1.2.3).
    Full !Natural !Natural !Natural
  | -- | Major and minor version only (e.g. 1.2).
    MajorMinor !Natural !Natural
  | -- | Major version only (e.g. 1).
    Major !Natural
  | -- | No version value (*, x or X).
    Any
  deriving (Eq, TH.Lift)

-- | We rely on this 'show' implementation to produce valid node-semver partial version.
instance Show PartialVersion where
  show (Full mjr mnr ptc) = printf "%d.%d.%d" mjr mnr ptc
  show (MajorMinor mjr mnr) = printf "%d.%d" mjr mnr
  show (Major mjr) = printf "%d" mjr
  show Any = "*"

fromVersion :: Version -> PartialVersion
fromVersion (Version mjr mnr ptc) = Full mjr mnr ptc

parsePartialVersion :: String -> Either ParseError PartialVersion
parsePartialVersion = parse partialVersionParser ""

-- See `partial` defintion here: https://github.com/npm/node-semver#range-grammar
partialVersionParser :: Parsec String () PartialVersion
partialVersionParser = do
  maybeMajor <- componentP
  maybeMinor <- optionMaybe $ try (char '.' *> componentP)
  maybePatch <- optionMaybe $ try (char '.' *> componentP)
  let components = maybeMajor : catMaybes [maybeMinor, maybePatch]
  case components of
    [Nothing] -> pure Any -- "*" / "x" / "X"
    [Just mjr] -> pure (Major mjr) -- "1"
    [Just mjr, Nothing] -> pure (Major mjr) -- "1.x"
    [Just mjr, Nothing, Nothing] -> pure (Major mjr) -- "1.x.x"
    [Just mjr, Just mnr] -> pure (MajorMinor mjr mnr) -- "1.2"
    [Just mjr, Just mnr, Nothing] -> pure (MajorMinor mjr mnr) -- "1.2.x"
    [Just mjr, Just mnr, Just ptc] -> pure (Full mjr mnr ptc) -- "1.2.3"
    [Nothing, _, _] -> fail "wildcard must be the only component"
    [_, Nothing, Just _] -> fail "patch cannot be specified if minor is wildcard"
    _ -> fail "invalid version form"
  where
    componentP = (Nothing <$ wildcardP) <|> (Just <$> noLeadingZeroNaturalP)
    wildcardP = void (oneOf "xX*")

pv :: TH.QuasiQuoter
pv = quasiQuoterFromParser parsePartialVersion

-- | Converts a 'PartialVersion' to its lower bound 'Version'.
-- For partial versions, missing components default to 0.
toLowerBoundVersion :: PartialVersion -> Version
toLowerBoundVersion Any = Version 0 0 0
toLowerBoundVersion (Major mjr) = Version mjr 0 0
toLowerBoundVersion (MajorMinor mjr mnr) = Version mjr mnr 0
toLowerBoundVersion (Full mjr mnr ptc) = Version mjr mnr ptc

-- | Converts a 'PartialVersion' to its exclusive upper bound for X-ranges.
-- For full versions, this returns an Inclusive bound (exact match).
-- For partial versions, returns Exclusive bound of next increment.
toUpperBound :: PartialVersion -> VersionBound
toUpperBound Any = Inf
toUpperBound (Major mjr) = Exclusive (Version (mjr + 1) 0 0)
toUpperBound (MajorMinor mjr mnr) = Exclusive (Version mjr (mnr + 1) 0)
toUpperBound (Full mjr mnr ptc) = Inclusive (Version mjr mnr ptc)

-- | Tilde allows patch-level changes if minor is specified.
toTildeUpperBound :: PartialVersion -> VersionBound
toTildeUpperBound Any = Inf
toTildeUpperBound (Major mjr) = Exclusive (Version (mjr + 1) 0 0)
toTildeUpperBound (MajorMinor mjr mnr) = Exclusive (Version mjr (mnr + 1) 0)
toTildeUpperBound (Full mjr mnr _) = Exclusive (Version mjr (mnr + 1) 0)

-- | Caret allows changes that don't modify the leftmost non-zero digit.
toCaretUpperBound :: PartialVersion -> VersionBound
toCaretUpperBound Any = Inf
toCaretUpperBound (Major 0) = Exclusive (Version 1 0 0)
toCaretUpperBound (Major mjr) = Exclusive (Version (mjr + 1) 0 0)
toCaretUpperBound (MajorMinor 0 0) = Exclusive (Version 0 1 0)
toCaretUpperBound (MajorMinor 0 mnr) = Exclusive (Version 0 (mnr + 1) 0)
toCaretUpperBound (MajorMinor mjr _) = Exclusive (Version (mjr + 1) 0 0)
toCaretUpperBound (Full 0 0 ptc) = Exclusive (Version 0 0 (ptc + 1))
toCaretUpperBound (Full 0 mnr _) = Exclusive (Version 0 (mnr + 1) 0)
toCaretUpperBound (Full mjr _ _) = Exclusive (Version (mjr + 1) 0 0)
