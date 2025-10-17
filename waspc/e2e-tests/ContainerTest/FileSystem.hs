module ContainerTest.FileSystem
  ( ContainerDockerfileFile,
    dontainerDockerfileFileInE2eTestsDir,
    getContainerDockerfileFile,
  )
where

import FileSystem (E2eTestsDir, getE2eTestsDir)
import StrongPath (Abs, File, Path', Rel, relfile, (</>))

data ContainerDockerfileFile

dontainerDockerfileFileInE2eTestsDir :: Path' (Rel E2eTestsDir) (File ContainerDockerfileFile)
dontainerDockerfileFileInE2eTestsDir = [relfile|ContainerTest/Dockerfile|]

getContainerDockerfileFile :: IO (Path' Abs (File ContainerDockerfileFile))
getContainerDockerfileFile = (</> dontainerDockerfileFileInE2eTestsDir) <$> getE2eTestsDir
