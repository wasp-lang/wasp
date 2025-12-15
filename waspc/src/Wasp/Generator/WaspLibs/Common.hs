module Wasp.Generator.WaspLibs.Common
  ( libsRootDirNextToSdk,
    libsRootDirInGeneratedCodeDir,
    libsSrcDirPathInDataDir,
    libsRootDirFromSdkDir,
    libsRootDirFromServerDir,
    getAbsLibsSourceDirPath,
    LibsSourceDir,
    LibsRootDir,
  )
where

import StrongPath (Abs, Dir, Path', Rel, Rel', basename, reldir, (</>))
import qualified Wasp.Data as Data
import Wasp.Generator.Common (ProjectRootDir)
import Wasp.Project.Common (generatedCodeDirInDotWaspDir)

data LibsSourceDir

-- | Type representing the destination directory where Wasp lib tarballs
-- are copied in the generated project (e.g., .wasp/out/libs/).
--
-- LibsRootDir has a flat structure with tarball files at the top level:
--   libs/
--   ├── wasp.sh-lib-auth-<wasp-version>.tgz
--   └── wasp.sh-lib-other-<wasp-version>.tgz
data LibsRootDir

-- We are following the locaton of the SDK because of the
-- hack from `Wasp.Generator.SdkGenerator.sdkRootDirInProjectRootDir`.
-- To understand the hack, read this issue:
-- https://github.com/wasp-lang/wasp/issues/1769
libsRootDirNextToSdk :: Path' (Rel ProjectRootDir) (Dir LibsRootDir)
libsRootDirNextToSdk =
  [reldir|../|]
    </> basename generatedCodeDirInDotWaspDir
    </> libsRootDirInGeneratedCodeDir

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
