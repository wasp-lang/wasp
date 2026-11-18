{-# LANGUAGE DeriveLift #-}

module Wasp.SemanticVersion.PartialVersion
  ( PartialVersion (..),
    versionToPartialVersion,
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
import Wasp.SemanticVersion.Version (Version, versionWithCoreParser)
import Wasp.Util.TH (quasiQuoterFromParser)

-- | Version representation used in `node-semver` range expressions.
-- Unlike 'Version' which always has all 3 version components,
-- 'PartialVersion' can represent partial/wildcard versions as they appear in a range.
data PartialVersion
  = -- | Full version, optionally with pre-release and build (e.g. 1.2.3, 1.2.3-alpha.1+build).
    FullVersion !Version
  | -- | Major and minor version only (e.g. 1.2).
    MajorMinor !Natural !Natural
  | -- | Major version only (e.g. 1).
    Major !Natural
  | -- | No version value (*, x or X).
    Any
  deriving (Eq, TH.Lift)

-- | We rely on this 'show' implementation to produce a valid `node-semver` output.
instance Show PartialVersion where
  show (FullVersion version) = show version
  show (MajorMinor mjr mnr) = printf "%d.%d" mjr mnr
  show (Major mjr) = printf "%d" mjr
  show Any = "*"

versionToPartialVersion :: Version -> PartialVersion
versionToPartialVersion = FullVersion

pv :: TH.QuasiQuoter
pv = quasiQuoterFromParser parsePartialVersion

parsePartialVersion :: String -> Either P.ParseError PartialVersion
parsePartialVersion = P.parse (partialVersionParser <* P.eof) ""

-- See `partial` definition here: https://github.com/npm/node-semver#range-grammar
-- NOTE: Unlike the grammar, we accept the pre-release and build metadata only on versions without wildcards.
-- `node-semver` discards them on versions with wildcards anyway (e.g. 1.2.x-alpha).
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
    [Number mjr, Number mnr, Number ptc] -> FullVersion <$> versionWithCoreParser mjr mnr ptc -- "1.2.3", "1.2.3-alpha+build"
    [Wildcard, _, _] -> fail "wildcard must be the only component"
    [_, Wildcard, Number _] -> fail "patch cannot be specified if minor is wildcard"
    _ -> fail "invalid version form"
  where
    versionComponentParser :: P.Parsec String () VersionComponent
    versionComponentParser = (Wildcard <$ wildcardParser) <|> (Number <$> naturalNumberParser)

    wildcardParser :: P.Parsec String () ()
    wildcardParser = void (P.oneOf "xX*")

data VersionComponent = Wildcard | Number Natural
