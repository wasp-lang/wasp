module Wasp.Project.Db.Dev.Sqlite
  ( makeDevConnectionUrl,
  )
where

import StrongPath (Abs, File, Path')
import qualified StrongPath as SP
import System.Directory (createDirectoryIfMissing)
import Wasp.Project.Common
  ( DevDbFile,
  )

makeDevConnectionUrl :: Path' Abs (File DevDbFile) -> IO String
makeDevConnectionUrl devDbFilePath = do
  createDirectoryIfMissing True $ SP.fromAbsDir $ SP.parent devDbFilePath
  return $ "file:" ++ SP.fromAbsFile devDbFilePath
