module Generator.FileDraft.Writeable
  ( Writeable (..),
  )
where

import Generator.Common (ProjectRootDir)
import Generator.FileDraft.WriteableMonad
import StrongPath (Abs, Dir, Path)

class Writeable w where
  -- | Write file somewhere in the provided project root directory.
  write ::
    (WriteableMonad m) =>
    Path Abs (Dir ProjectRootDir) ->
    w ->
    m ()
