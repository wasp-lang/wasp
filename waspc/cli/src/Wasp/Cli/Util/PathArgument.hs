module Wasp.Cli.Util.PathArgument
  ( FilePathArgument,
    getFilePath,
    filePathReader,
  )
where

import Options.Applicative (ReadM, str)
import StrongPath (Abs, File', Path')
import StrongPath.FilePath (parseAbsFile)
import System.Directory (makeAbsolute)

-- Paths passed as arguments to a CLI are conventionally either absolute paths,
-- or paths relative to the current working directory. We need IO to resolve
-- which kind of path it is, but we also don't want any unsafe transformations
-- in the meantime; so we make these types opaque until we have access to the IO
-- monad.

newtype FilePathArgument = FilePathArgument FilePath
  deriving (Show, Eq)

filePathReader :: ReadM FilePathArgument
filePathReader = FilePathArgument <$> str

getFilePath :: FilePathArgument -> IO (Path' Abs File')
getFilePath (FilePathArgument filePath) = makeAbsolute filePath >>= parseAbsFile
