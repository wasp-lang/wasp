{-# LANGUAGE DeriveLift #-}

module Wasp.SemanticVersion.PartialVersion
  ( PartialVersion (..),
    fromVersion,
    parsePartialVersion,
    partialVersionParser,
    pv,
  )
where

import Control.Applicative ((<|>))
import Control.Monad (void)
import Data.Maybe (catMaybes)
import qualified Language.Haskell.TH.Quote as TH
import qualified Language.Haskell.TH.Syntax as TH
import Numeric.Natural (Natural)
import qualified Text.Parsec as P
import Text.Printf (printf)
import Wasp.SemanticVersion.Parsers (naturalNumberParser)
import Wasp.SemanticVersion.Version (Version (..))
import Wasp.Util.TH (quasiQuoterFromParser)

-- | Version representation used in `node-semver` range expressions.
-- Unlike 'Version' which always has all 3 version components,
-- 'PartialVersion' can represent partial/wildcard versions as they appear in a range.
data PartialVersion
  = -- | Major, minor and patch version (e.g. 1.2.3).
    MajorMinorPatch !Natural !Natural !Natural
  | -- | Major and minor version only (e.g. 1.2).
    MajorMinor !Natural !Natural
  | -- | Major version only (e.g. 1).
    Major !Natural
  | -- | No version value (*, x or X).
    Any
  deriving (Eq, TH.Lift)

-- | We rely on this 'show' implementation to produce valid `node-semver` partial version.
instance Show PartialVersion where
  show (MajorMinorPatch mjr mnr ptc) = printf "%d.%d.%d" mjr mnr ptc
  show (MajorMinor mjr mnr) = printf "%d.%d" mjr mnr
  show (Major mjr) = printf "%d" mjr
  show Any = "*"

fromVersion :: Version -> PartialVersion
fromVersion (Version mjr mnr ptc) = MajorMinorPatch mjr mnr ptc

pv :: TH.QuasiQuoter
pv = quasiQuoterFromParser parsePartialVersion

parsePartialVersion :: String -> Either P.ParseError PartialVersion
parsePartialVersion = P.parse partialVersionParser ""

-- See `partial` definition here: https://github.com/npm/node-semver#range-grammar
partialVersionParser :: P.Parsec String () PartialVersion
partialVersionParser = do
  maybeMajor <- versionComponentParser
  maybeMaybeMinor <- P.optionMaybe $ P.try (P.char '.' *> versionComponentParser)
  maybeMaybePatch <- P.optionMaybe $ P.try (P.char '.' *> versionComponentParser)
  let versionComponents = maybeMajor : catMaybes [maybeMaybeMinor, maybeMaybePatch]
  case versionComponents of
    [Wildcard] -> pure Any -- "*" / "x" / "X"
    [Number mjr] -> pure (Major mjr) -- "1"
    [Number mjr, Wildcard] -> pure (Major mjr) -- "1.x"
    [Number mjr, Wildcard, Wildcard] -> pure (Major mjr) -- "1.x.x"
    [Number mjr, Number mnr] -> pure (MajorMinor mjr mnr) -- "1.2"
    [Number mjr, Number mnr, Wildcard] -> pure (MajorMinor mjr mnr) -- "1.2.x"
    [Number mjr, Number mnr, Number ptc] -> pure (MajorMinorPatch mjr mnr ptc) -- "1.2.3"
    [Wildcard, _, _] -> fail "wildcard must be the only component"
    [_, Wildcard, Number _] -> fail "patch cannot be specified if minor is wildcard"
    _ -> fail "invalid version form"
  where
    versionComponentParser :: P.Parsec String () VersionComponent
    versionComponentParser = (Wildcard <$ wildcardParser) <|> (Number <$> naturalNumberParser)

    wildcardParser :: P.Parsec String () ()
    wildcardParser = void (P.oneOf "xX*")

data VersionComponent = Wildcard | Number Natural