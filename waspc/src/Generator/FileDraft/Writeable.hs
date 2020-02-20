module Generator.FileDraft.Writeable
       ( Writeable(..)
       ) where

import qualified Path.Aliases as Path

import Generator.FileDraft.WriteableMonad

class Writeable w where
    -- | Write file somewhere in the provided dst directory.
    write :: (WriteableMonad m)
          => Path.AbsDir  -- ^ Absolute path of dst directory.
          -> w
          -> m ()
