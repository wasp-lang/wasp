{-# LANGUAGE DeriveLift #-}

module Wasp.SemanticVersion.Version
  ( Version (..),
    parseVersion,
    versionParser,
    v,
    nextBreakingChangeVersion,
  )
where

import qualified Language.Haskell.TH.Quote as TH
import qualified Language.Haskell.TH.Syntax as TH
import Numeric.Natural (Natural)
import Text.Parsec (ParseError, Parsec, char, parse)
import Text.Printf (printf)
import Wasp.SemanticVersion.Parsers (noLeadingZeroNaturalP)
import Wasp.Util.TH (quasiQuoterFromParser)

data Version = Version
  { major :: !Natural,
    minor :: !Natural,
    patch :: !Natural
  }
  deriving (Eq, Ord, TH.Lift)

-- | We rely on this 'show' implementation to produce a valid SemVer version.
instance Show Version where
  show (Version mjr mnr ptc) = printf "%d.%d.%d" mjr mnr ptc

parseVersion :: String -> Either ParseError Version
parseVersion = parse versionParser ""

-- | Follows SemVer specification.
-- See: https://semver.org/#backusnaur-form-grammar-for-valid-semver-versions
-- TODO: Add pre-release (-) and build (+) support.
versionParser :: Parsec String () Version
versionParser = do
  (mjr, mnr, ptc) <- versionCoreP

  pure (Version mjr mnr ptc)
  where
    versionCoreP = do
      mjr <- noLeadingZeroNaturalP
      _ <- char '.'
      mnr <- noLeadingZeroNaturalP
      _ <- char '.'
      ptc <- noLeadingZeroNaturalP
      pure (mjr, mnr, ptc)

v :: TH.QuasiQuoter
v = quasiQuoterFromParser parseVersion

nextBreakingChangeVersion :: Version -> Version
nextBreakingChangeVersion = \case
  (Version 0 0 x) -> Version 0 0 (succ x)
  (Version 0 x _) -> Version 0 (succ x) 0
  (Version x _ _) -> Version (succ x) 0 0
