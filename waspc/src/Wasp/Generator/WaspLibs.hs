module Wasp.Generator.WaspLibs
  ( genWaspLibs,
    ensureWaspLibsAreInGeneratedAppDir,
  )
where

import Control.Monad (forM_)
import StrongPath (Abs, Dir, Path', (</>))
import qualified StrongPath as SP
import System.Directory (createDirectoryIfMissing)
import Wasp.Generator.Common (GeneratedAppDir)
import Wasp.Generator.FileDraft (FileDraft, createCopyLibFileDraft)
import Wasp.Generator.Monad (Generator)
import Wasp.Generator.WaspLibs.AvailableLibs (waspLibs)
import Wasp.Generator.WaspLibs.Common (getAbsLibsSourceDirPath)
import Wasp.Generator.WaspLibs.WaspLib
  ( WaspLib,
    getTarballPathInGeneratedAppDir,
    getTarballPathInLibsSourceDir,
  )
import qualified Wasp.Util.IO as IOUtil

genWaspLibs :: Generator [FileDraft]
genWaspLibs = return $ mkLibCopyDraft <$> waspLibs
  where
    mkLibCopyDraft :: WaspLib -> FileDraft
    mkLibCopyDraft waspLib =
      createCopyLibFileDraft
        (getTarballPathInGeneratedAppDir waspLib)
        (getTarballPathInLibsSourceDir waspLib)

-- | Copies the Wasp lib tarballs into the generated app dir without running
-- the generator. The project's lockfile references the tarballs as @file:@
-- dependencies, so @npm install@ needs them on disk even before the first
-- compilation (e.g. during standalone @wasp install@ on a fresh clone).
ensureWaspLibsAreInGeneratedAppDir :: Path' Abs (Dir GeneratedAppDir) -> IO ()
ensureWaspLibsAreInGeneratedAppDir generatedAppDir = do
  libsSourceDir <- getAbsLibsSourceDirPath
  forM_ waspLibs $ \waspLib -> do
    let srcTarballPath = libsSourceDir </> getTarballPathInLibsSourceDir waspLib
        dstTarballPath = generatedAppDir </> getTarballPathInGeneratedAppDir waspLib
    createDirectoryIfMissing True (SP.fromAbsDir $ SP.parent dstTarballPath)
    IOUtil.copyFile srcTarballPath dstTarballPath
