{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE PatternSynonyms #-}

module Wasp.SemanticVersion.Version
  ( Version (..),
    pattern FullVersion,
    nextBreakingChangeVersion,
    parseVersion,
    versionParser,
    v,
  )
where

import Data.Maybe (fromMaybe)
import qualified Language.Haskell.TH.Quote as TH
import qualified Language.Haskell.TH.Syntax as TH
import Numeric.Natural (Natural)
import Text.Parsec (ParseError, Parsec, char, parse, sepBy)
import Text.Parsec.Language (emptyDef)
import Text.Parsec.Token (makeTokenParser, natural)
import Wasp.Util.TH (quasiQuoterFromParser)

data Version = Version
  { major :: !Natural,
    minor :: !(Maybe Natural),
    patch :: !(Maybe Natural)
  }
  deriving (TH.Lift)

-- | Eq instance that treats missing components as 0.
-- So "1" == "1.0" == "1.0.0".
instance Eq Version where
  Version maj1 mnr1 ptc1 == Version maj2 mnr2 ptc2 =
    maj1 == maj2
      && fromMaybe 0 mnr1 == fromMaybe 0 mnr2
      && fromMaybe 0 ptc1 == fromMaybe 0 ptc2

-- | Ord instance that treats missing components as 0.
-- So "1" == "1.0" == "1.0.0".
instance Ord Version where
  compare (Version maj1 mnr1 ptc1) (Version maj2 mnr2 ptc2) =
    compare maj1 maj2
      <> compare (fromMaybe 0 mnr1) (fromMaybe 0 mnr2)
      <> compare (fromMaybe 0 ptc1) (fromMaybe 0 ptc2)

-- | Pattern synonym for the common case where all three version components are present.
pattern FullVersion :: Natural -> Natural -> Natural -> Version
pattern FullVersion maj mnr ptc = Version maj (Just mnr) (Just ptc)

-- | We rely on this `show` implementation to produce valid semver representation of version.
instance Show Version where
  show (Version mjr mnr ptc) = show mjr ++ showMaybe mnr ++ showMaybe ptc
    where
      showMaybe = maybe "" (("." ++) . show)

parseVersion :: String -> Either ParseError Version
parseVersion = parse versionParser ""

versionParser :: Parsec String () Version
versionParser = do
  naturalP `sepBy` char '.' >>= \case
    [a] -> return $ Version a Nothing Nothing
    [a, b] -> return $ Version a (Just b) Nothing
    [a, b, c] -> return $ Version a (Just b) (Just c)
    _invalidFormat -> fail "Invalid version format"
  where
    naturalP = fromIntegral <$> natural lexer
    lexer = makeTokenParser emptyDef

v :: TH.QuasiQuoter
v = quasiQuoterFromParser parseVersion

-- | A missing minor and patch values will desugar to zero.
nextBreakingChangeVersion :: Version -> Version
nextBreakingChangeVersion = \case
  -- 0.0.x -> 0.0.(x+1)
  FullVersion 0 0 ptc -> FullVersion 0 0 (succ ptc)
  -- 0.x.y -> 0.(x+1).0
  Version 0 (Just mnr) _ -> FullVersion 0 (succ mnr) 0
  -- x.y.z -> (x+1).0.0
  Version maj _ _ -> FullVersion (succ maj) 0 0
