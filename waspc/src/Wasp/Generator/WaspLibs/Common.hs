module Wasp.Generator.WaspLibs.Common
  ( libsRootDirInGeneratedCodeDir,
    libsSrcDirPathInDataDir,
    getAbsLibsSourceDirPath,
    LibsSourceDir,
    LibsRootDir,
  )
where

import StrongPath (Abs, Dir, Path', Rel, reldir, (</>))
import qualified Wasp.Data as Data
import Wasp.Generator.Common (ProjectRootDir)

data LibsSourceDir

-- | Type representing the destination directory where Wasp lib tarballs
-- are copied in the generated project (e.g., .wasp/out/libs/).
--
-- LibsRootDir has a flat structure with tarball files at the top level:
--   libs/
--   ├── wasp.sh-lib-auth-<wasp-version>.tgz
--   └── wasp.sh-lib-other-<wasp-version>.tgz
data LibsRootDir

libsRootDirInGeneratedCodeDir :: Path' (Rel ProjectRootDir) (Dir LibsRootDir)
libsRootDirInGeneratedCodeDir = [reldir|libs|]

libsSrcDirPathInDataDir :: Path' (Rel Data.DataDir) (Dir LibsSourceDir)
libsSrcDirPathInDataDir = [reldir|Generator/libs|]

getAbsLibsSourceDirPath :: IO (Path' Abs (Dir LibsSourceDir))
getAbsLibsSourceDirPath = (</> libsSrcDirPathInDataDir) <$> Data.getAbsDataDirPath
