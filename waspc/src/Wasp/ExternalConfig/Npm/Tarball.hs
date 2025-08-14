module Wasp.ExternalConfig.Npm.Tarball
  ( makeNpmTarball,
    sanitizePackageNameForTarballName,
    makeTarballFilename,
    SanitizedTarballName (..),
    NpmTarball (..),
  )
where

import Data.Maybe (fromJust)
import StrongPath (File', Path', Rel, parseRelFile)

{--
Tarballs are a convention used by npm to package libraries. For example,
a tarball for a library named "my-lib" with version "1.0.0" would
be named "my-lib-1.0.0.tgz".

Wasp packages Wasp internal libraries in tarballs, so we need to generate
tarball filenames in the same way npm does.
--}

data NpmTarball dir = NpmTarball
  { filename :: Path' (Rel dir) File'
  }
  deriving (Eq)

makeNpmTarball :: SanitizedTarballName -> String -> NpmTarball dir
makeNpmTarball name version =
  NpmTarball
    { filename = makeTarballFilename name version
    }

newtype SanitizedTarballName = SanitizedTarballName String
  deriving (Eq)

instance Show SanitizedTarballName where
  show (SanitizedTarballName name) = name

makeTarballFilename :: SanitizedTarballName -> String -> Path' (Rel dir) File'
makeTarballFilename (SanitizedTarballName name) version =
  fromJust $
    parseRelFile $
      concat
        [ name,
          "-",
          version,
          ".tgz"
        ]

-- | Sanitizes the package name in the same way `npm pack` does.
sanitizePackageNameForTarballName :: String -> SanitizedTarballName
sanitizePackageNameForTarballName = SanitizedTarballName . sanitize
  where
    sanitize :: String -> String
    sanitize = removeStartingAtSymbol . slashesToDashes

    removeStartingAtSymbol :: String -> String
    removeStartingAtSymbol ('@' : xs) = xs
    removeStartingAtSymbol xs = xs

    slashesToDashes :: String -> String
    slashesToDashes = map $ \c -> if c == '/' then '-' else c
