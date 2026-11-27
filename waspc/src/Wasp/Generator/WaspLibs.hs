module Wasp.Generator.WaspLibs
  ( genWaspLibs,
  )
where

import Wasp.AppSpec (AppSpec)
import Wasp.Generator.FileDraft (FileDraft, createCopyLibFileDraft)
import Wasp.Generator.Monad (Generator)
import Wasp.Generator.WaspLibs.AvailableLibs (getWaspLibsForApp)
import Wasp.Generator.WaspLibs.WaspLib
  ( WaspLib,
    getTarballPathInGeneratedAppDir,
    getTarballPathInLibsSourceDir,
  )

genWaspLibs :: AppSpec -> Generator [FileDraft]
genWaspLibs spec = return $ mkLibCopyDraft <$> getWaspLibsForApp spec
  where
    mkLibCopyDraft :: WaspLib -> FileDraft
    mkLibCopyDraft waspLib =
      createCopyLibFileDraft
        (getTarballPathInGeneratedAppDir waspLib)
        (getTarballPathInLibsSourceDir waspLib)
