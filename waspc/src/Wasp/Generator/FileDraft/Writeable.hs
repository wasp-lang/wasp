module Wasp.Generator.FileDraft.Writeable
  ( Writeable (..),
  )
where

import StrongPath (Abs, Dir, Path')
import Wasp.Generator.Common (ProjectRootDir)
import Wasp.Generator.FileDraft.WriteableMonad

class Writeable w where
  -- | Write file somewhere in the provided project root directory.
  write ::
    (WriteableMonad m) =>
    Path' Abs (Dir ProjectRootDir) ->
    w ->
    m ()
