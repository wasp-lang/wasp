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
import Text.Parsec.Language (emptyDef)
import Text.Parsec.Token (makeTokenParser, natural)
import Text.Printf (printf)
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

versionParser :: Parsec String () Version
versionParser = do
  naturalP `sepBy` char '.' >>= \case
    [a] -> return $ Version a 0 0
    [a, b] -> return $ Version a b 0
    [a, b, c] -> return $ Version a b c
    _invalidFormat -> fail "Invalid version format"
  where
    naturalP = fromIntegral <$> natural lexer
    lexer = makeTokenParser emptyDef

v :: TH.QuasiQuoter
v = quasiQuoterFromParser parseVersion

nextBreakingChangeVersion :: Version -> Version
nextBreakingChangeVersion = \case
  (Version 0 0 x) -> Version 0 0 (succ x)
  (Version 0 x _) -> Version 0 (succ x) 0
  (Version x _ _) -> Version (succ x) 0 0
