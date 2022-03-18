module Wasp.Generator.FileDraft.Writeable
  ( Writeable (..),
    Checksum,
    FileOrDirPathRelativeTo,
  )
where

import StrongPath (Abs, Dir, Dir', File', Path', Rel)
import Wasp.Generator.Common (ProjectRootDir)
import Wasp.Generator.FileDraft.WriteableMonad
import Wasp.Util (Checksum)

type FileOrDirPathRelativeTo a = Either (Path' (Rel a) File') (Path' (Rel a) Dir')

class Writeable w where
  -- | Write file somewhere in the provided project root directory.
  write ::
    (WriteableMonad m) =>
    Path' Abs (Dir ProjectRootDir) ->
    w ->
    m ()

  getChecksum :: w -> IO Checksum

  getDstPath :: w -> FileOrDirPathRelativeTo ProjectRootDir
