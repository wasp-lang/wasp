module Wasp.Generator.WaspLibs
  ( initWaspLibs,
    genWaspLibs,
  )
where

import StrongPath ((</>))
import qualified Wasp.AppSpec as AS
import qualified Wasp.ExternalConfig.Npm.Tarball as Npm.Tarball
import Wasp.Generator.FileDraft (FileDraft, createCopyFileDraft)
import Wasp.Generator.Monad (Generator, getWaspLibs)
import Wasp.Generator.WaspLibs.Common (libsRootDirInGeneratedCodeDir, libsRootDirNextToSdk)
import qualified Wasp.Generator.WaspLibs.WaspLib as WaspLib

-- NOTE: The package names of the libs should match the names in the
-- `package.json` files of the libs in the ./libs directory.
initWaspLibs :: IO [WaspLib.WaspLib]
initWaspLibs = sequence [WaspLib.makeWaspLib "@wasp.sh/lib-auth"]

genWaspLibs :: AS.AppSpec -> Generator [FileDraft]
genWaspLibs spec = do
  waspLibs <- getWaspLibs
  return [mkLibCopyDraft destDir waspLib | destDir <- destDirs, waspLib <- waspLibs]
  where
    mkLibCopyDraft destDir waspLib =
      createCopyFileDraft
        (destDir </> Npm.Tarball.filename (WaspLib.generatedCodeDirTarball waspLib))
        (WaspLib.waspDataDirTarballAbsPath waspLib)

    -- We need to accomodate the SDK hacks with libs, so we copy the libs
    -- differently depending on the context:
    -- 1. When running `wasp start` - copy them only to the `.wasp/out` dir
    -- 2. When running `wasp build` - copy them to the `.wasp/build` AND
    --   `.wasp/out` dir.
    destDirs =
      if AS.isBuild spec
        then [libsRootDirInGeneratedCodeDir, libsRootDirNextToSdk]
        else [libsRootDirInGeneratedCodeDir]
