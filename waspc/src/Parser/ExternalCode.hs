module Parser.ExternalCode
  ( extCodeFilePathString,
  )
where

import ExternalCode (SourceExternalCodeDir)
import qualified Parser.Common
import qualified Path.Posix as PPosix
import StrongPath (File', Path, Posix, Rel)
import qualified StrongPath.Path as SP.Path
import Text.Parsec (unexpected)
import Text.Parsec.String (Parser)

-- Parses string literal that is file path to file in source external code dir.
-- Returns file path relative to the external code dir.
-- Example of input: "@ext/some/file.txt". Output would be: "some/file.txt".
extCodeFilePathString :: Parser (Path Posix (Rel SourceExternalCodeDir) File')
extCodeFilePathString = do
  path <- Parser.Common.relPosixFilePathString
  maybe
    (unexpected $ "string \"" ++ show path ++ "\": External code file path should start with \"@ext/\".")
    return
    -- TODO: Once StrongPath supports stripProperPrefix method, use that instead of transforming it to Path and back.
    (SP.Path.fromPathRelFileP <$> PPosix.stripProperPrefix [PPosix.reldir|@ext|] (SP.Path.toPathRelFileP path))
