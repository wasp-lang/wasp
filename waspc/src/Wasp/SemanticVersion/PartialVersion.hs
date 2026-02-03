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

-- | Version representation used in range expressions.
-- Unlike 'Version' which always has all 3 components,
-- 'PartialVersion' can represent partial/wildcard versions as they appear in ranges.
data PartialVersion
  = -- | Major, minor and patch (1.2.3).
    Full !Natural !Natural !Natural
  | -- | Major and minor only (1.2 meaning 1.2.x).
    MajorMinor !Natural !Natural
  | -- | Major only (1 meaning 1.x.x).
    Major !Natural
  | -- | No version value (*, x or X, meaning x.x.x).
    Any
  deriving (Eq, TH.Lift)

fromVersion :: Version -> PartialVersion
fromVersion (Version m n p) = Full m n p

instance Show PartialVersion where
  show (Full m n p) = printf "%d.%d.%d" m n p
  show (MajorMinor m n) = printf "%d.%d" m n
  show (Major m) = printf "%d" m
  show Any = "*"

parsePartialVersion :: String -> Either ParseError PartialVersion
parsePartialVersion = parse partialVersionParser ""

partialVersionParser :: Parsec String () PartialVersion
partialVersionParser = do
  first <- componentP
  maybeSecond <- optionMaybe (try (char '.' *> componentP))
  maybeThird <- optionMaybe (try (char '.' *> componentP))
  let components = first : catMaybes [maybeSecond, maybeThird]
  case components of
    [Nothing] -> pure Any -- "*" / "x" / "X"
    [Just m] -> pure (Major m) -- "1"
    [Just m, Nothing] -> pure (Major m) -- "1.x"
    [Just m, Nothing, Nothing] -> pure (Major m) -- "1.x.x"
    [Just m, Just n] -> pure (MajorMinor m n) -- "1.2"
    [Just m, Just n, Nothing] -> pure (MajorMinor m n) -- "1.2.x"
    [Just m, Just n, Just p] -> pure (Full m n p) -- "1.2.3"
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
toLowerBound :: PartialVersion -> Version
toLowerBound Any = Version 0 0 0
toLowerBound (Major m) = Version m 0 0
toLowerBound (MajorMinor m n) = Version m n 0
toLowerBound (Full m n p) = Version m n p

-- | Converts a 'PartialVersion' to its exclusive upper bound for X-ranges.
-- For full versions, this returns an Inclusive bound (exact match).
-- For partial versions, returns Exclusive bound of next increment.
toUpperBound :: PartialVersion -> VersionBound
toUpperBound Any = Inf
toUpperBound (Major m) = Exclusive (Version (m + 1) 0 0)
toUpperBound (MajorMinor m n) = Exclusive (Version m (n + 1) 0)
toUpperBound (Full m n p) = Inclusive (Version m n p)

-- | Tilde allows patch-level changes if minor is specified.
toTildeUpperBound :: PartialVersion -> VersionBound
toTildeUpperBound Any = Inf
toTildeUpperBound (Major m) = Exclusive (Version (m + 1) 0 0)
toTildeUpperBound (MajorMinor m n) = Exclusive (Version m (n + 1) 0)
toTildeUpperBound (Full m n _) = Exclusive (Version m (n + 1) 0)

-- | Caret allows changes that don't modify the leftmost non-zero digit.
toCaretUpperBound :: PartialVersion -> VersionBound
toCaretUpperBound Any = Inf
toCaretUpperBound (Major 0) = Exclusive (Version 1 0 0)
toCaretUpperBound (Major m) = Exclusive (Version (m + 1) 0 0)
toCaretUpperBound (MajorMinor 0 0) = Exclusive (Version 0 1 0)
toCaretUpperBound (MajorMinor 0 n) = Exclusive (Version 0 (n + 1) 0)
toCaretUpperBound (MajorMinor m _) = Exclusive (Version (m + 1) 0 0)
toCaretUpperBound (Full 0 0 p) = Exclusive (Version 0 0 (p + 1))
toCaretUpperBound (Full 0 n _) = Exclusive (Version 0 (n + 1) 0)
toCaretUpperBound (Full m _ _) = Exclusive (Version (m + 1) 0 0)
