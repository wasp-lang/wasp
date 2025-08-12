module Wasp.Generator.WaspLibs.Common
  ( libsRootDirInProjectRootDir,
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

data LibsRootDir

-- We are repeating the SDK hack here - becuase the libs needs to be next to
-- the SDK.
libsRootDirInProjectRootDir :: Path' (Rel ProjectRootDir) (Dir LibsRootDir)
libsRootDirInProjectRootDir =
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
