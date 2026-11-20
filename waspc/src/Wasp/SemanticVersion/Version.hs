{-# LANGUAGE DeriveLift #-}

module Wasp.SemanticVersion.Version
  ( Version (..),
    PreReleaseIdentifier (..),
    releaseVersion,
    lowestVersion,
    lowestPreReleaseOf,
    isPreRelease,
    hasSameCoreAs,
    parseVersion,
    versionParser,
    versionWithCoreParser,
    v,
    nextBreakingChangeVersion,
  )
where

import Data.Char (isAsciiLower, isAsciiUpper, isDigit)
import Data.Function (on)
import Data.List (intercalate)
import Data.Ord (comparing)
import qualified Language.Haskell.TH.Quote as TH
import qualified Language.Haskell.TH.Syntax as TH
import Numeric.Natural (Natural)
import qualified Text.Parsec as P
import Text.Printf (printf)
import Wasp.SemanticVersion.Parsers (naturalNumberParser)
import Wasp.Util.TH (quasiQuoterFromParser)

-- | Follows SemVer specification.
-- See: https://semver.org/
data Version = Version
  { major :: !Natural,
    minor :: !Natural,
    patch :: !Natural,
    -- | Empty for release versions (e.g. 1.2.3), non-empty for pre-release versions (e.g. 1.2.3-alpha.1).
    preRelease :: ![PreReleaseIdentifier],
    -- | Build metadata (e.g. 1.2.3+build.5). It doesn't affect version precedence.
    buildMetadata :: ![String]
  }
  deriving (TH.Lift)

-- | The derived 'Ord' follows SemVer precedence:
-- numeric identifiers are compared numerically, alphanumeric identifiers lexically in ASCII order,
-- and numeric identifiers always have lower precedence than alphanumeric identifiers.
data PreReleaseIdentifier
  = NumericIdentifier !Natural
  | AlphanumericIdentifier !String
  deriving (Eq, Ord, TH.Lift)

-- | Versions are equal when they have the same precedence, build metadata is ignored.
-- Same as `node-semver`'s `eq`.
instance Eq Version where
  version1 == version2 = compare version1 version2 == EQ

-- | Follows SemVer precedence, build metadata is ignored.
-- See: https://semver.org/#spec-item-11
instance Ord Version where
  compare =
    comparing (.major)
      <> comparing (.minor)
      <> comparing (.patch)
      <> (comparePreRelease `on` (.preRelease))
    where
      -- A release version has higher precedence than any of its pre-release versions.
      -- Otherwise, identifiers are compared from left to right,
      -- and a shorter list of identifiers has lower precedence if all preceding identifiers are equal.
      comparePreRelease :: [PreReleaseIdentifier] -> [PreReleaseIdentifier] -> Ordering
      comparePreRelease [] [] = EQ
      comparePreRelease [] _ = GT
      comparePreRelease _ [] = LT
      comparePreRelease identifiers1 identifiers2 = compare identifiers1 identifiers2

-- | We rely on this 'show' implementation to produce a valid SemVer version.
instance Show Version where
  show (Version mjr mnr ptc preReleaseIdentifiers buildIdentifiers) =
    printf "%d.%d.%d" mjr mnr ptc
      ++ showIdentifiers '-' (show <$> preReleaseIdentifiers)
      ++ showIdentifiers '+' buildIdentifiers
    where
      showIdentifiers :: Char -> [String] -> String
      showIdentifiers _ [] = ""
      showIdentifiers prefix identifiers = prefix : intercalate "." identifiers

instance Show PreReleaseIdentifier where
  show (NumericIdentifier number) = show number
  show (AlphanumericIdentifier identifier) = identifier

releaseVersion :: Natural -> Natural -> Natural -> Version
releaseVersion mjr mnr ptc = Version mjr mnr ptc [] []

-- | The version with the lowest precedence: 0.0.0-0.
lowestVersion :: Version
lowestVersion = lowestPreReleaseOf 0 0 0

-- | The version with the lowest precedence among all versions with the given major, minor and patch.
-- E.g. 2.0.0-0 for 2.0.0.
-- `node-semver` uses it as the exclusive upper bound of desugared ranges (e.g. ^1.2.3 is >=1.2.3 <2.0.0-0),
-- so that the range doesn't include pre-release versions of the upper bound.
lowestPreReleaseOf :: Natural -> Natural -> Natural -> Version
lowestPreReleaseOf mjr mnr ptc = Version mjr mnr ptc [NumericIdentifier 0] []

isPreRelease :: Version -> Bool
isPreRelease version = not $ null version.preRelease

-- | Checks if both versions have the same major, minor and patch.
hasSameCoreAs :: Version -> Version -> Bool
hasSameCoreAs version1 version2 =
  (version1.major, version1.minor, version1.patch) == (version2.major, version2.minor, version2.patch)

v :: TH.QuasiQuoter
v = quasiQuoterFromParser parseVersion

parseVersion :: String -> Either P.ParseError Version
parseVersion = P.parse (versionParser <* P.eof) ""

-- | Follows SemVer specification.
-- See: https://semver.org/#backusnaur-form-grammar-for-valid-semver-versions
versionParser :: P.Parsec String () Version
versionParser = do
  (mjr, mnr, ptc) <- versionCoreParser
  versionWithCoreParser mjr mnr ptc
  where
    versionCoreParser :: P.Parsec String () (Natural, Natural, Natural)
    versionCoreParser = do
      mjr <- naturalNumberParser
      _ <- P.char '.'
      mnr <- naturalNumberParser
      _ <- P.char '.'
      ptc <- naturalNumberParser
      pure (mjr, mnr, ptc)

-- | Parses the optional pre-release and build metadata following the given version core.
versionWithCoreParser :: Natural -> Natural -> Natural -> P.Parsec String () Version
versionWithCoreParser mjr mnr ptc = Version mjr mnr ptc <$> preReleaseParser <*> buildMetadataParser

-- | Parses the optional pre-release part (e.g. "-alpha.1").
-- Returns an empty list if there is no valid pre-release part.
-- See `<pre-release>` definition here: https://semver.org/#backusnaur-form-grammar-for-valid-semver-versions
preReleaseParser :: P.Parsec String () [PreReleaseIdentifier]
preReleaseParser = P.option [] $ P.try $ P.char '-' *> dotSeparatedIdentifiersParser preReleaseIdentifierParser
  where
    preReleaseIdentifierParser :: P.Parsec String () PreReleaseIdentifier
    preReleaseIdentifierParser = identifierParser >>= toPreReleaseIdentifier

    toPreReleaseIdentifier :: String -> P.Parsec String () PreReleaseIdentifier
    toPreReleaseIdentifier identifier
      | not (all isDigit identifier) = pure $ AlphanumericIdentifier identifier
      | hasLeadingZero identifier = fail "numeric identifier must not include leading zeroes"
      | otherwise = pure $ NumericIdentifier $ read identifier

    hasLeadingZero :: String -> Bool
    hasLeadingZero ('0' : _ : _) = True
    hasLeadingZero _ = False

-- | Parses the optional build metadata part (e.g. "+build.01").
-- Returns an empty list if there is no valid build metadata part.
-- See `<build>` definition here: https://semver.org/#backusnaur-form-grammar-for-valid-semver-versions
buildMetadataParser :: P.Parsec String () [String]
buildMetadataParser = P.option [] $ P.try $ P.char '+' *> dotSeparatedIdentifiersParser identifierParser

dotSeparatedIdentifiersParser :: P.Parsec String () a -> P.Parsec String () [a]
dotSeparatedIdentifiersParser itemParser =
  (:) <$> itemParser <*> P.many (P.try $ P.char '.' *> itemParser)

-- | Parses a non-empty string of ASCII letters, digits and hyphens.
-- We don't use 'P.alphaNum' because it also accepts non-ASCII letters.
identifierParser :: P.Parsec String () String
identifierParser = P.many1 $ P.satisfy isIdentifierCharacter
  where
    isIdentifierCharacter :: Char -> Bool
    isIdentifierCharacter c = isAsciiLower c || isAsciiUpper c || isDigit c || c == '-'

-- | Returns the lowest version which is a breaking change compared to the given version.
nextBreakingChangeVersion :: Version -> Version
nextBreakingChangeVersion = \case
  (Version 0 0 x _ _) -> releaseVersion 0 0 (succ x)
  (Version 0 x _ _ _) -> releaseVersion 0 (succ x) 0
  (Version x _ _ _ _) -> releaseVersion (succ x) 0 0
