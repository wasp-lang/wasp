module Wasp.Generator.WaspLibs
  ( genWaspLibs,
  )
where

import StrongPath ((</>))
import Wasp.Generator.FileDraft (FileDraft, createCopyLibFileDraft)
import Wasp.Generator.Monad (Generator)
import Wasp.Generator.WaspLibs.AvailableLibs (waspLibs)
import Wasp.Generator.WaspLibs.Common (libsRootDirInGeneratedCodeDir)
import Wasp.Generator.WaspLibs.WaspLib (WaspLib, getTarballPathInLibsRootDir, getTarballPathInLibsSourceDir)

genWaspLibs :: Generator [FileDraft]
genWaspLibs = return $ mkLibCopyDraft <$> waspLibs
  where
    mkLibCopyDraft :: WaspLib -> FileDraft
    mkLibCopyDraft waspLib =
      createCopyLibFileDraft
        (libsRootDirInGeneratedCodeDir </> getTarballPathInLibsRootDir waspLib)
        (getTarballPathInLibsSourceDir waspLib)
