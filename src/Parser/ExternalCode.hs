module Parser.ExternalCode
    ( extCodeFilePathString
    ) where

import Text.Parsec (unexpected)
import Text.Parsec.String (Parser)
import Path (reldir)
import qualified Path
import qualified Path.Aliases as Path

import qualified Parser.Common

-- Parses string literal that is file path to file in external code dir.
-- Returns file path relative to the external code dir.
-- Example of input: "@ext/some/file.txt". Output would be: "some/file.txt".
extCodeFilePathString :: Parser Path.RelFile
extCodeFilePathString = do
    path <- Parser.Common.relFilePathString
    maybe (unexpected $ "string \"" ++ (show path) ++ "\": External code file path should start with \"@ext/\".")
          return
          (Path.stripProperPrefix [reldir|@ext|] path)
