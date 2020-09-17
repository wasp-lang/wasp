module Parser.ExternalCode
    ( extCodeFilePathString
    ) where

import Text.Parsec (unexpected)
import Text.Parsec.String (Parser)
import qualified Path.Posix as PPosix

import qualified Parser.Common


-- Parses string literal that is file path to file in source external code dir.
-- Returns file path relative to the external code dir.
-- Example of input: "@ext/some/file.txt". Output would be: "some/file.txt".
extCodeFilePathString :: Parser (PPosix.Path PPosix.Rel PPosix.File)
extCodeFilePathString = do
    path <- Parser.Common.relPosixFilePathString
    maybe (unexpected $ "string \"" ++ (show path) ++ "\": External code file path should start with \"@ext/\".")
          return
          (PPosix.stripProperPrefix [PPosix.reldir|@ext|] path)
