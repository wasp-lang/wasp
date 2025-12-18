module Wasp.Generator.WaspLibs
  ( genWaspLibs,
  )
where

import StrongPath (Abs, Dir, Path', Rel, (</>))
import qualified Wasp.AppSpec as AS
import Wasp.Generator.Common (ProjectRootDir)
import Wasp.Generator.FileDraft (FileDraft, createCopyFileDraft)
import Wasp.Generator.Monad (Generator)
import qualified Wasp.Generator.Monad as Generator
import qualified Wasp.Generator.WaspLibs.AvailableLibs as WaspLibs.AvailableLibs
import Wasp.Generator.WaspLibs.Common (LibsRootDir, LibsSourceDir, libsRootDirInGeneratedCodeDir, libsRootDirNextToSdk)
import qualified Wasp.Generator.WaspLibs.WaspLib as WaspLib

genWaspLibs :: AS.AppSpec -> Generator [FileDraft]
genWaspLibs spec = do
  libsSourceDir <- Generator.getLibsSourceDir
  return
    [ mkLibCopyDraft libsSourceDir libsDestDir waspLib
      | libsDestDir <- libsDestDirs,
        waspLib <- WaspLibs.AvailableLibs.waspLibs
    ]
  where
    mkLibCopyDraft :: Path' Abs (Dir LibsSourceDir) -> Path' (Rel ProjectRootDir) (Dir LibsRootDir) -> WaspLib.WaspLib -> FileDraft
    mkLibCopyDraft libsSourceDir libsDestDir waspLib =
      createCopyFileDraft
        (libsDestDir </> WaspLib.getTarballPathInLibsRootDir waspLib)
        (WaspLib.getTarballAbsPathInLibsSourceDir libsSourceDir waspLib)

    -- When we build the app for production, the Wasp SDK is not generated inside the
    -- build directory (to understand why read: https://github.com/wasp-lang/wasp/issues/1769),
    -- and since libs are required both inside the build directory and next to the SDK,
    -- we need to copy them to both locations.
    libsDestDirs =
      if AS.isBuild spec
        then [libsRootDirInGeneratedCodeDir, libsRootDirNextToSdk]
        else [libsRootDirInGeneratedCodeDir]
