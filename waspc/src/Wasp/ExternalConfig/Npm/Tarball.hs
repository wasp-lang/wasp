module Wasp.ExternalConfig.Npm.Tarball
  ( makeTarballFilePath,
    sanitizeForTarballFilename,
    SanitizedTarballName (..),
  )
where

import Data.Maybe (fromJust)
import StrongPath (File', Path', Rel', parseRelFile)

newtype SanitizedTarballName = SanitizedTarballName String
  deriving (Eq)

instance Show SanitizedTarballName where
  show (SanitizedTarballName name) = name

makeTarballFilePath :: SanitizedTarballName -> String -> Path' Rel' File'
makeTarballFilePath name version =
  fromJust $
    parseRelFile $
      concat
        [ show name,
          "-",
          version,
          ".tgz"
        ]

-- | Sanitizes the package name in the same way `npm pack` does.
sanitizeForTarballFilename :: String -> SanitizedTarballName
sanitizeForTarballFilename packageName =
  let withoutAt = if head packageName == '@' then tail packageName else packageName
      sanitizedName = map sanitizeWithDashes withoutAt
   in SanitizedTarballName sanitizedName
  where
    sanitizeWithDashes :: Char -> Char
    sanitizeWithDashes c
      | c == '/' = '-'
      | otherwise = c
