module Wasp.Generator.WaspLibs.Common
  ( libsRootDirInGeneratedCodeDir,
    libsSrcDirPathInDataDir,
    libsRootDirFromSdkDir,
    libsRootDirFromServerDir,
    getAbsLibsSourceDirPath,
    LibsSourceDir,
    LibsRootDir,
  )
where

import StrongPath (Abs, Dir, Path', Rel, Rel', reldir, (</>))
import qualified Wasp.Data as Data
import Wasp.Generator.Common (ProjectRootDir)

data LibsSourceDir

-- | Type representing the destination directory where Wasp lib tarballs
-- are copied in the generated project (e.g., .wasp/out/libs/).
--
-- LibsRootDir has a flat structure with tarball files at the top level:
--   libs/
--   ├── wasp.sh-lib-auth-<checksum>.tgz
--   └── wasp.sh-lib-other-<checksum>.tgz
data LibsRootDir

libsRootDirInGeneratedCodeDir :: Path' (Rel ProjectRootDir) (Dir LibsRootDir)
libsRootDirInGeneratedCodeDir = [reldir|libs|]

libsSrcDirPathInDataDir :: Path' (Rel Data.DataDir) (Dir LibsSourceDir)
libsSrcDirPathInDataDir = [reldir|Generator/libs|]

libsRootDirFromSdkDir :: Path' Rel' (Dir LibsRootDir)
libsRootDirFromSdkDir = [reldir|../../|] </> libsRootDirInGeneratedCodeDir

libsRootDirFromServerDir :: Path' Rel' (Dir LibsRootDir)
libsRootDirFromServerDir = [reldir|../|] </> libsRootDirInGeneratedCodeDir

getAbsLibsSourceDirPath :: IO (Path' Abs (Dir LibsSourceDir))
getAbsLibsSourceDirPath = (</> libsSrcDirPathInDataDir) <$> Data.getAbsDataDirPath
