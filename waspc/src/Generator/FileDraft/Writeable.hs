module Generator.FileDraft.Writeable
       ( Writeable(..)
       ) where

import StrongPath (Path, Abs, Dir)
import Generator.Common (ProjectRootDir)
import Generator.FileDraft.WriteableMonad


class Writeable w where
    -- | Write file somewhere in the provided project root directory.
    write :: (WriteableMonad m)
          => Path Abs (Dir ProjectRootDir)
          -> w
          -> m ()
