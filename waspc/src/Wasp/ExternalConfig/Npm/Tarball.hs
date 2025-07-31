module Wasp.ExternalConfig.Npm.Tarball
  ( makeTarballFilePath,
    sanitizeForTarballFilename,
  )
where

import Data.Maybe (fromJust)
import StrongPath (File', Path', Rel', parseRelFile)

makeTarballFilePath :: String -> String -> Path' Rel' File'
makeTarballFilePath name version =
  fromJust $
    parseRelFile $
      concat
        [ sanitizeForTarballFilename name,
          "-",
          version,
          ".tgz"
        ]

sanitizeForTarballFilename :: String -> String
sanitizeForTarballFilename packageName =
  let withoutAt = if head packageName == '@' then tail packageName else packageName
      withDashes = map sanitizeWithDashes withoutAt
   in withDashes
  where
    sanitizeWithDashes :: Char -> Char
    sanitizeWithDashes c
      | c == '/' = '-'
      | otherwise = c
