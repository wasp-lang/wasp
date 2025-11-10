module Wasp.Cli.Util.PathArgument
  ( PathArgument,
    getPath,
    pathReader,
  )
where

import Options.Applicative (ReadM, str)
import StrongPath (Abs, File, Path')
import StrongPath.FilePath (parseAbsFile)
import System.Directory (makeAbsolute)

-- Paths passed as arguments to a CLI are conventionally either absolute paths,
-- or paths relative to the current working directory. We need IO to resolve
-- which kind of path it is, but we also don't want any unsafe transformations
-- in the meantime; so we make this type opaque until we have access to the IO
-- monad.
newtype PathArgument = PathArgument FilePath
  deriving (Show, Eq)

pathReader :: ReadM PathArgument
pathReader = PathArgument <$> str

getPath :: PathArgument -> IO (Path' Abs (File ()))
getPath (PathArgument filePath) = makeAbsolute filePath >>= parseAbsFile
