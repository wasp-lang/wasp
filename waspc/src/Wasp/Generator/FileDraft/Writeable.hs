module Wasp.Generator.FileDraft.Writeable
  ( Writeable (..),
    Checksum,
    FileOrDirPathRelativeTo,
  )
where

import StrongPath (Abs, Dir, Dir', File', Path', Rel)
import Wasp.Generator.Common (GeneratedAppDir)
import Wasp.Generator.FileDraft.WriteableMonad
import Wasp.Util (Checksum)

type FileOrDirPathRelativeTo a = Either (Path' (Rel a) File') (Path' (Rel a) Dir')

class Writeable w where
  -- | Write file somewhere in the provided generated app directory.
  write ::
    (WriteableMonad m) =>
    Path' Abs (Dir GeneratedAppDir) ->
    w ->
    m ()

  getChecksum :: w -> IO Checksum

  getDstPath :: w -> FileOrDirPathRelativeTo GeneratedAppDir
