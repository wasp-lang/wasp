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

    -- When we build the app for production, the Wasp SDK is not generated inside the
    -- build directory (to understand why read: https://github.com/wasp-lang/wasp/issues/1769),
    -- and since libs are required both inside the build directory and next to the SDK,
    -- we need to copy them to both locations.
    libsDestDirs =
      if AS.isProduction spec
        then [libsRootDirInGeneratedCodeDir, libsRootDirNextToSdk]
        else [libsRootDirInGeneratedCodeDir]
