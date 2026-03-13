{-# LANGUAGE DeriveLift #-}

module Wasp.SemanticVersion.Version
  ( Version (..),
    parseVersion,
    strictParseVersion,
    versionParser,
    v,
    nextBreakingChangeVersion,
  )
where

import qualified Language.Haskell.TH.Quote as TH
import qualified Language.Haskell.TH.Syntax as TH
import Numeric.Natural (Natural)
import qualified Text.Parsec as P
import Text.Printf (printf)
import Wasp.SemanticVersion.Parsers (naturalNumberParser)
import Wasp.Util.TH (quasiQuoterFromParser)

-- | Follows SemVer specification.
-- See: https://semver.org/
-- TODO: Add pre-release (-) and build (+) support.
data Version = Version
  { major :: !Natural,
    minor :: !Natural,
    patch :: !Natural
  }
  deriving (Eq, Ord, TH.Lift)

-- | We rely on this 'show' implementation to produce a valid SemVer version.
instance Show Version where
  show (Version mjr mnr ptc) = printf "%d.%d.%d" mjr mnr ptc

v :: TH.QuasiQuoter
v = quasiQuoterFromParser strictParseVersion

strictParseVersion :: String -> Either P.ParseError Version
strictParseVersion = P.parse (versionParser <* P.eof) ""

parseVersion :: String -> Either P.ParseError Version
parseVersion = P.parse versionParser ""

-- | Follows SemVer specification.
-- See: https://semver.org/#backusnaur-form-grammar-for-valid-semver-versions
-- TODO: Add pre-release (-) and build (+) support.
versionParser :: P.Parsec String () Version
versionParser = do
  (mjr, mnr, ptc) <- versionCoreParser
  pure (Version mjr mnr ptc)
  where
    versionCoreParser :: P.Parsec String () (Natural, Natural, Natural)
    versionCoreParser = do
      mjr <- naturalNumberParser
      _ <- P.char '.'
      mnr <- naturalNumberParser
      _ <- P.char '.'
      ptc <- naturalNumberParser
      pure (mjr, mnr, ptc)

nextBreakingChangeVersion :: Version -> Version
nextBreakingChangeVersion = \case
  (Version 0 0 x) -> Version 0 0 (succ x)
  (Version 0 x _) -> Version 0 (succ x) 0
  (Version x _ _) -> Version (succ x) 0 0
