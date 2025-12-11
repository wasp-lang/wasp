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

-- | Type representing the destination directory where Wasp library directories
-- are copied in the generated project (e.g., .wasp/out/libs/).
--
-- LibsRootDir contains subdirectories for each library:
--   libs/
--   ├── auth/
--   │   ├── package.json
--   │   ├── dist/
--   │   └── ...
--   └── other/
--       └── ...
data LibsRootDir

-- We are repeating the SDK hack here - becuase the libs needs to be next to
-- the SDK.
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
