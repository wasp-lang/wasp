module Wasp.ExternalConfig.Npm.Tarball
  ( packageNameToTarballPrefix,
    makeTarballFilename,
    tarballFilenameAsRelFile,
    TarballFilename (..),
  )
where

import Data.Maybe (fromJust)
import StrongPath (File', Path', Rel, parseRelFile)

-- |
-- Tarballs are a convention used by npm to package libraries. For example,
-- a tarball for a library named "my-lib" with version "1.0.0" would
-- be named "my-lib-1.0.0.tgz".
--
-- Wasp packages Wasp internal libraries in tarballs, so we need to generate
-- tarball filenames in the same way npm does.
newtype TarballFilename = TarballFilename String
  deriving (Show, Eq)

makeTarballFilename :: String -> String -> TarballFilename
makeTarballFilename packageName version =
  TarballFilename $
    concat
      [ packageNameToTarballPrefix packageName,
        "-",
        version,
        ".tgz"
      ]

packageNameToTarballPrefix :: String -> String
packageNameToTarballPrefix = removeStartingAtSymbol . slashesToDashes
  where
    removeStartingAtSymbol :: String -> String
    removeStartingAtSymbol ('@' : xs) = xs
    removeStartingAtSymbol xs = xs

    slashesToDashes :: String -> String
    slashesToDashes = map $ \c -> if c == '/' then '-' else c

tarballFilenameAsRelFile :: TarballFilename -> Path' (Rel dir) File'
tarballFilenameAsRelFile (TarballFilename filename) = fromJust $ parseRelFile filename
