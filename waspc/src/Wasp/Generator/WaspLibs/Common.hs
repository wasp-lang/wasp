module Wasp.Generator.WaspLibs.Common
  ( libsRootDirInProjectRootDir,
    libsRootDirInGeneratedCodeDir,
    libsDirPathInDataDir,
    libsDirFromSdkDir,
    LibsSourceDir,
    LibsRootDir,
  )
where

import StrongPath (Dir, Dir', Path', Rel, Rel', basename, reldir, (</>))
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
    </> [reldir|libs|]

libsRootDirInGeneratedCodeDir :: Path' (Rel ProjectRootDir) (Dir LibsRootDir)
libsRootDirInGeneratedCodeDir = [reldir|libs|]

libsDirPathInDataDir :: Path' (Rel Data.DataDir) (Dir LibsSourceDir)
libsDirPathInDataDir = [reldir|Generator/libs|]

libsDirFromSdkDir :: Path' Rel' Dir'
libsDirFromSdkDir = [reldir|../../libs|]
