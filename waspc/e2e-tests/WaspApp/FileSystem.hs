module WaspApp.FileSystem
  ( WaspAppDir,
    WaspAppDotWaspDir,
    WaspAppOutDir,
    WaspAppBuildDir,
    WaspAppMigrationDir,
    waspAppDotWaspDirInWaspAppDir,
    waspAppOutDirInWaspAppDotWaspDir,
    waspAppBuildDirInWaspAppDotWaspDir,
    waspAppBuildDirInWaspAppDir,
    waspAppOutDirInWaspAppDir,
    waspAppMigrationsDirInWaspAppDir,
    waspAppMigrationsDirInWaspAppOutDir,
  )
where

import StrongPath

-- | The directory of some Wasp project.
data WaspAppDir

data WaspAppDotWaspDir

data WaspAppOutDir

data WaspAppBuildDir

data WaspAppMigrationDir

waspAppDotWaspDirInWaspAppDir :: Path' (Rel WaspAppDir) (Dir WaspAppDotWaspDir)
waspAppDotWaspDirInWaspAppDir = [reldir|.wasp|]

waspAppOutDirInWaspAppDotWaspDir :: Path' (Rel WaspAppDotWaspDir) (Dir WaspAppOutDir)
waspAppOutDirInWaspAppDotWaspDir = [reldir|out|]

waspAppBuildDirInWaspAppDotWaspDir :: Path' (Rel WaspAppDotWaspDir) (Dir WaspAppBuildDir)
waspAppBuildDirInWaspAppDotWaspDir = [reldir|build|]

waspAppBuildDirInWaspAppDir :: Path' (Rel WaspAppDir) (Dir WaspAppBuildDir)
waspAppBuildDirInWaspAppDir = waspAppDotWaspDirInWaspAppDir </> waspAppBuildDirInWaspAppDotWaspDir

waspAppOutDirInWaspAppDir :: Path' (Rel WaspAppDir) (Dir WaspAppOutDir)
waspAppOutDirInWaspAppDir = waspAppDotWaspDirInWaspAppDir </> waspAppOutDirInWaspAppDotWaspDir

waspAppMigrationsDirInWaspAppDir :: Path' (Rel WaspAppDir) (Dir WaspAppMigrationDir)
waspAppMigrationsDirInWaspAppDir = [reldir|migrations|]

waspAppMigrationsDirInWaspAppOutDir :: Path' (Rel WaspAppOutDir) (Dir WaspAppMigrationDir)
waspAppMigrationsDirInWaspAppOutDir = [reldir|db/migrations|]
