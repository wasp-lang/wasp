module Wasp.Version
  ( waspVersion,
    parseWaspVersion,
  )
where

import qualified Data.Version as DV
import qualified Paths_waspc
import Text.Parsec (ParseError, Parsec, char, choice, parse)
import qualified Wasp.SemanticVersion as SV
import Wasp.SemanticVersion.Version (versionParser)

waspVersion :: SV.Version
waspVersion = case Paths_waspc.version of
  DV.Version [major, minor, patch] _ -> SV.Version (toEnum major) (toEnum minor) (toEnum patch)
  _ -> error "This should never happen. Wasp binary version must have exactly three digits, but it doesn't."

parseWaspVersion :: String -> Either ParseError SV.Range
parseWaspVersion = parse waspVersionParser ""

waspVersionParser :: Parsec String () SV.Range
waspVersionParser =
  SV.Range . return <$> (choice prefixParsers <*> versionParser)
  where
    prefixParsers =
      [ char '^' *> return SV.backwardsCompatibleWith,
        char '~' *> return SV.approximatelyEquivalentWith,
        return SV.eq
      ]
