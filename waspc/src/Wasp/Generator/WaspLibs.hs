module Wasp.Generator.WaspLibs
  ( genWaspLibs,
  )
where

import StrongPath (Dir, Path', Rel, (</>))
import qualified Wasp.AppSpec as AS
import Wasp.Generator.Common (ProjectRootDir)
import Wasp.Generator.FileDraft (FileDraft, createCopyFileDraft)
import Wasp.Generator.Monad (Generator, getWaspLibs)
import Wasp.Generator.WaspLibs.Common (LibsRootDir, libsRootDirInGeneratedCodeDir, libsRootDirNextToSdk)
import qualified Wasp.Generator.WaspLibs.WaspLib as WaspLib

genWaspLibs :: AS.AppSpec -> Generator [FileDraft]
genWaspLibs spec = do
  waspLibs <- getWaspLibs
  return [mkLibCopyDraft libsDestDir waspLib | libsDestDir <- libsDestDirs, waspLib <- waspLibs]
  where
    mkLibCopyDraft :: Path' (Rel ProjectRootDir) (Dir LibsRootDir) -> WaspLib.WaspLib -> FileDraft
    mkLibCopyDraft libsDestDir waspLib =
      createCopyFileDraft
        (libsDestDir </> WaspLib.getTarballPathInLibsRootDir waspLib)
        (WaspLib.waspDataDirTarballAbsPath waspLib)

    -- We need to accomodate the SDK hacks with libs, so we copy the libs
    -- differently depending on the context:
    -- 1. When running `wasp start` - copy them only to the `.wasp/out` dir
    -- 2. When running `wasp build` - copy them to the `.wasp/build` AND
    --   `.wasp/out` dir.
    libsDestDirs =
      if AS.isBuild spec
        then [libsRootDirInGeneratedCodeDir, libsRootDirNextToSdk]
        else [libsRootDirInGeneratedCodeDir]
