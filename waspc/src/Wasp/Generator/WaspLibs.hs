module Wasp.Generator.WaspLibs
  ( genWaspLibs,
  )
where

import Wasp.Generator.FileDraft (FileDraft, createCopyLibFileDraft)
import Wasp.Generator.Monad (Generator)
import Wasp.Generator.WaspLibs.AvailableLibs (allWaspLibs)
import Wasp.Generator.WaspLibs.WaspLib
  ( WaspLib,
    getTarballPathInGeneratedAppDir,
    getTarballPathInLibsSourceDir,
  )

genWaspLibs :: Generator [FileDraft]
genWaspLibs = return $ mkLibCopyDraft <$> allWaspLibs
  where
    mkLibCopyDraft :: WaspLib -> FileDraft
    mkLibCopyDraft waspLib =
      createCopyLibFileDraft
        (getTarballPathInGeneratedAppDir waspLib)
        (getTarballPathInLibsSourceDir waspLib)
