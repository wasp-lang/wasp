module Wasp.Project.Install
  ( installWaspDependenciesIO,
    isInstalledWaspSpecMatchingCliVersion,
  )
where

import Control.Concurrent (newChan)
import StrongPath (Abs, Dir, Path', (</>))
import Wasp.Generator.NpmInstall (installProjectNpmDependencies)
import Wasp.Generator.WaspLibs (ensureWaspLibsAreInGeneratedAppDir)
import Wasp.NodePackageFFI (InstallablePackage (WaspSpecPackage), ensurePackageIsAtInstallationPathInProject, tryGettingInstalledPackageVersion)
import Wasp.Project.Common (WaspProjectDir, generatedAppDirInWaspProjectDir)
import qualified Wasp.Version as WV

installWaspDependenciesIO :: Path' Abs (Dir WaspProjectDir) -> IO (Either String ())
installWaspDependenciesIO projectDir = do
  ensurePackageIsAtInstallationPathInProject projectDir WaspSpecPackage
  -- Module packages peer-depend on the `wasp` SDK package. npm auto-installs
  -- required peers, so the SDK's `file:` lib tarballs must exist before npm runs.
  ensureWaspLibsAreInGeneratedAppDir $ projectDir </> generatedAppDirInWaspProjectDir
  messageChan <- newChan
  installProjectNpmDependencies messageChan projectDir

isInstalledWaspSpecMatchingCliVersion :: Path' Abs (Dir WaspProjectDir) -> IO Bool
isInstalledWaspSpecMatchingCliVersion projectDir =
  tryGettingInstalledPackageVersion projectDir WaspSpecPackage >>= \case
    Right installedVersion -> return $ installedVersion == WV.waspVersion
    Left _ -> return False
