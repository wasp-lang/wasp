module Wasp.Generator.WaspLibs
  ( genWaspLibs,
  )
where

import Wasp.Generator.FileDraft (FileDraft, createCopyLibFileDraft)
import Wasp.Generator.Monad (Generator)
import Wasp.Generator.WaspLibs.AvailableLibs (waspLibs)
import Wasp.Generator.WaspLibs.WaspLib
  ( WaspLib,
    getTarballPathInGeneratedAppDir,
    getTarballPathInLibsSourceDir,
  )

genWaspLibs :: Generator [FileDraft]
genWaspLibs = return $ mkLibCopyDraft <$> waspLibs
  where
    mkLibCopyDraft :: WaspLib -> FileDraft
    mkLibCopyDraft waspLib =
      createCopyLibFileDraft
        (getTarballPathInGeneratedAppDir waspLib)
        (getTarballPathInLibsSourceDir waspLib)
