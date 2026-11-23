module Wasp.Project.Db.Dev.Sqlite
  ( defaultDevDbFile,
    devDbFileInDbRootDir,
  )
where

import StrongPath (File', Path', Rel, relfile)
import qualified StrongPath as SP
import Wasp.Generator.Common (DbRootDir)

-- | Prisma resolves SQLite file paths relative to the schema file, which lives
-- in the generated db root dir.
defaultDevDbFile :: String
defaultDevDbFile = "file:./" ++ SP.toFilePath devDbFileInDbRootDir

devDbFileInDbRootDir :: Path' (Rel DbRootDir) File'
devDbFileInDbRootDir = [relfile|dev.db|]
