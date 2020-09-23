module Parser.ExternalCode
    ( extCodeFilePathString
    ) where

import qualified Path.Posix         as PPosix
import           Text.Parsec        (unexpected)
import           Text.Parsec.String (Parser)

import           ExternalCode       (SourceExternalCodeDir)
import qualified Parser.Common
import           StrongPath         (File, Path', Posix, Rel)
import qualified StrongPath         as SP


-- Parses string literal that is file path to file in source external code dir.
-- Returns file path relative to the external code dir.
-- Example of input: "@ext/some/file.txt". Output would be: "some/file.txt".
extCodeFilePathString :: Parser (Path' Posix (Rel SourceExternalCodeDir) File)
extCodeFilePathString = do
    path <- Parser.Common.relPosixFilePathString
    maybe (unexpected $ "string \"" ++ show path ++ "\": External code file path should start with \"@ext/\".")
          (return . SP.fromPathRelFileP)
          (PPosix.stripProperPrefix [PPosix.reldir|@ext|] path)
