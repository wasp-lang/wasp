{-# LANGUAGE DeriveLift #-}

module Wasp.SemanticVersion.Version
  ( Version (..),
    nextBreakingChangeVersion,
    parseVersion,
    versionParser,
    v,
  )
where

import qualified Language.Haskell.TH.Quote as TH
import qualified Language.Haskell.TH.Syntax as TH
import Numeric.Natural (Natural)
import Text.Parsec (ParseError, Parsec, char, parse, sepBy)
import Text.Printf (printf)
import Wasp.SemanticVersion.Parsers (noLeadingZeroNaturalP)
import Wasp.Util.TH (quasiQuoterFromParser)

data Version = Version
  { major :: !Natural,
    minor :: !Natural,
    patch :: !Natural
  }
  deriving (Eq, Ord, TH.Lift)

-- | We rely on this `show` implementation to produce valid semver representation of version.
instance Show Version where
  show (Version mjr mnr ptc) = printf "%d.%d.%d" mjr mnr ptc

parseVersion :: String -> Either ParseError Version
parseVersion = parse versionParser ""

-- | A 'Version' number must take the form X.Y.Z where:
-- - X, Y, and Z are non-negative integers,
-- - X, Y, and Z must not contain leading zeroes.
versionParser :: Parsec String () Version
versionParser = do
  noLeadingZeroNaturalP `sepBy` char '.' >>= \case
    [a, b, c] -> return $ Version a b c
    _invalidFormat -> fail "Invalid version format"

v :: TH.QuasiQuoter
v = quasiQuoterFromParser parseVersion

nextBreakingChangeVersion :: Version -> Version
nextBreakingChangeVersion = \case
  (Version 0 0 x) -> Version 0 0 (succ x)
  (Version 0 x _) -> Version 0 (succ x) 0
  (Version x _ _) -> Version (succ x) 0 0
