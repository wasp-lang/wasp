module Generator.FileDraft.Writeable
       ( Writeable(..)
       ) where

import Generator.FileDraft.WriteableMonad

class Writeable w where
    -- | Write file somewhere in the provided dst directory.
    write :: (WriteableMonad m)
          => FilePath  -- ^ Absolute path of dst directory.
          -> w
          -> m ()
