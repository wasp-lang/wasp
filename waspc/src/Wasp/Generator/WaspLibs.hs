module Wasp.Generator.WaspLibs
  ( genWaspLibs,
  )
where

import StrongPath ((</>))
import Wasp.Generator.FileDraft (FileDraft, createCopyFileDraft)
import Wasp.Generator.Monad (Generator, getWaspLibs)
import Wasp.Generator.WaspLibs.Common (libsRootDirInGeneratedCodeDir)
import qualified Wasp.Generator.WaspLibs.WaspLib as WaspLib

genWaspLibs :: Generator [FileDraft]
genWaspLibs = do
  waspLibs <- getWaspLibs
  return [mkLibCopyDraft waspLib | waspLib <- waspLibs]
  where
    mkLibCopyDraft :: WaspLib.WaspLib -> FileDraft
    mkLibCopyDraft waspLib =
      createCopyFileDraft
        (libsRootDirInGeneratedCodeDir </> WaspLib.getTarballPathInLibsRootDir waspLib)
        (WaspLib.waspDataDirTarballAbsPath waspLib)
