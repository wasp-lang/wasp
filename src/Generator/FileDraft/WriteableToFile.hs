module Generator.FileDraft.WriteableToFile
       ( WriteableToFile(..)
       ) where

import Generator.FileDraft.FileDraftIO

class WriteableToFile w where
    -- | Based on "WriteableToFile" instance, creates file somewhere in the provided dst
    --   directory.
    writeToFile
        :: (FileDraftIO m)
        => FilePath  -- ^ Absolute path of dst directory.
        -> w  -- ^ "WriteableToFile" instance to be written to file.
        -> m ()
