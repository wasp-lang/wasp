module Wasp.Generator.WaspLibs
  ( genWaspLibs,
  )
where

import StrongPath (Abs, Dir, Dir', Path', Rel, reldir, (</>))
import qualified StrongPath as SP
import qualified Wasp.AppSpec as AS
import Wasp.Generator.Common (ProjectRootDir)
import Wasp.Generator.FileDraft (FileDraft, createCopyDirFileDraft)
import Wasp.Generator.FileDraft.CopyDirFileDraft (CopyDirFileDraftDstDirStrategy (WriteOverExistingDstDir))
import Wasp.Generator.Monad (Generator, getDataDirPath, getWaspLibs)
import Wasp.Generator.WaspLibs.Common (LibsRootDir, LibsSourceDir, libsRootDirInGeneratedCodeDir, libsRootDirNextToSdk, libsSrcDirPathInDataDir)
import qualified Wasp.Generator.WaspLibs.WaspLib as WaspLib

genWaspLibs :: AS.AppSpec -> Generator [FileDraft]
genWaspLibs spec = do
  waspLibs <- getWaspLibs
  dataDirPath <- getDataDirPath
  let libsSourceDirAbsPath = dataDirPath </> libsSrcDirPathInDataDir
  return [mkLibCopyDraft libsSourceDirAbsPath libsDestDir waspLib | libsDestDir <- libsDestDirs, waspLib <- waspLibs]
  where
    mkLibCopyDraft :: Path' Abs (Dir LibsSourceDir) -> Path' (Rel ProjectRootDir) (Dir LibsRootDir) -> WaspLib.WaspLib -> FileDraft
    mkLibCopyDraft libsSourceDirAbsPath libsDestDir waspLib =
      createCopyDirFileDraft
        WriteOverExistingDstDir
        (libsDestDir </> WaspLib.getLibDirPathInLibsRootDir waspLib)
        (SP.castDir libsSourceDirAbsPath </> SP.castDir (WaspLib.getLibDirPathInLibsRootDir waspLib))

    -- When we build the app for production, the Wasp SDK is not generated inside the
    -- build directory (to understand why read: https://github.com/wasp-lang/wasp/issues/1769),
    -- and since libs are required both inside the build directory and next to the SDK,
    -- we need to copy them to both locations.
    libsDestDirs =
      if AS.isBuild spec
        then [libsRootDirInGeneratedCodeDir, libsRootDirNextToSdk]
        else [libsRootDirInGeneratedCodeDir]
