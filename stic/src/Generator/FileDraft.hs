{-# LANGUAGE GADTs #-}
module Generator.FileDraft
       ( FileDraft (..)
       , WriteableToFile (..)
       ) where


class WriteableToFile w where
    writeToFile
        -- | Path to root directory, inside which file to be written has to be positioned.
        :: FilePath
        -> w
        -> IO ()

-- | Existential type / GADT, used to present all types implementing WriteableToFile as same type.
--   This allows us to treat them all in the same way and for example put them all together in a
--   list, achieveing heterogeneous list that way.
data FileDraft where
    FileDraft :: WriteableToFile d => d -> FileDraft

instance WriteableToFile FileDraft where
    writeToFile dstDir (FileDraft writeable) = writeToFile dstDir writeable
