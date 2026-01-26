module Wasp.Generator.WaspLibs
  ( genWaspLibs,
  )
where

import Wasp.Generator.FileDraft (FileDraft, createCopyLibFileDraft)
import Wasp.Generator.Monad (Generator)
import Wasp.Generator.WaspLibs.AvailableLibs (waspLibs)
import Wasp.Generator.WaspLibs.WaspLib
  ( WaspLib,
    getTarballPathInLibsSourceDir,
    getTarballPathInProjectRootDir,
  )

genWaspLibs :: Generator [FileDraft]
genWaspLibs = return $ mkLibCopyDraft <$> waspLibs
  where
    mkLibCopyDraft :: WaspLib -> FileDraft
    mkLibCopyDraft waspLib =
      createCopyLibFileDraft
        (getTarballPathInProjectRootDir waspLib)
        (getTarballPathInLibsSourceDir waspLib)
