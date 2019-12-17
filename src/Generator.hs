module Generator
       ( writeWebAppCode
       ) where

import CompileOptions (CompileOptions)
import Wasp
import Generator.Generators (generateWebApp)
import Generator.FileDraft (FileDraft, writeToFile)


-- | Generates web app code from given Wasp and writes it to given destination directory.
--   If dstDir does not exist yet, it will be created.
--   NOTE(martin): What if there is already smth in the dstDir? It is probably best
--     if we clean it up first? But we don't want this to end up with us deleting stuff
--     from user's machine. Maybe we just overwrite and we are good?
writeWebAppCode :: Wasp -> FilePath -> CompileOptions -> IO ()
writeWebAppCode wasp dstDir compileOptions = writeFileDrafts dstDir (generateWebApp wasp compileOptions)

-- | Writes file drafts while using given destination dir as root dir.
--   TODO(martin): We could/should parallelize this.
--     We could also skip writing files that are already on the disk with same checksum.
writeFileDrafts :: FilePath -> [FileDraft] -> IO ()
writeFileDrafts dstDir fileDrafts = sequence_ $ map (writeToFile dstDir) fileDrafts
