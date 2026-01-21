module Wasp.Generator.WaspLibs
  ( genWaspLibs,
  )
where

import StrongPath (Dir, Path', Rel, (</>))
import qualified Wasp.AppSpec as AS
import Wasp.Generator.Common (ProjectRootDir)
import Wasp.Generator.FileDraft (FileDraft, createCopyLibFileDraft)
import Wasp.Generator.Monad (Generator)
import Wasp.Generator.WaspLibs.AvailableLibs (waspLibs)
import Wasp.Generator.WaspLibs.Common (LibsRootDir, libsRootDirInGeneratedCodeDir, libsRootDirNextToSdk)
import Wasp.Generator.WaspLibs.WaspLib (WaspLib, getTarballPathInLibsRootDir, getTarballPathInLibsSourceDir)

genWaspLibs :: AS.AppSpec -> Generator [FileDraft]
genWaspLibs spec =
  return
    [ mkLibCopyDraft libsDestDir waspLib
      | libsDestDir <- libsDestDirs,
        waspLib <- waspLibs
    ]
  where
    mkLibCopyDraft :: Path' (Rel ProjectRootDir) (Dir LibsRootDir) -> WaspLib -> FileDraft
    mkLibCopyDraft libsDestDir waspLib =
      createCopyLibFileDraft
        (libsDestDir </> getTarballPathInLibsRootDir waspLib)
        (getTarballPathInLibsSourceDir waspLib)

    -- When we build the app for production, the Wasp SDK is not generated inside the
    -- build directory (to understand why read: https://github.com/wasp-lang/wasp/issues/1769),
    -- and since libs are required both inside the build directory and next to the SDK,
    -- we need to copy them to both locations.
    libsDestDirs =
      if AS.isProduction spec
        then [libsRootDirInGeneratedCodeDir, libsRootDirNextToSdk]
        else [libsRootDirInGeneratedCodeDir]
