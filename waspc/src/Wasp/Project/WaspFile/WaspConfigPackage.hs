module Wasp.Project.WaspFile.WaspConfigPackage
  ( ensureWaspConfigPackageInstalled,
  )
where

import Control.Concurrent (newChan)
import Control.Concurrent.Async (concurrently)
import StrongPath (Abs, Dir, File, Path', Rel, fromAbsDir, fromAbsFile, reldir, relfile, (</>))
import qualified StrongPath as SP
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.Exit (ExitCode (..))
import qualified Wasp.Job as J
import Wasp.Job.IO (readJobMessagesAndPrintThemPrefixed)
import Wasp.Job.Process (runNodeCommandAsJob)
import Wasp.NodePackageFFI (InstallablePackage (WaspConfigPackage), getPackageInstallationPath)
import Wasp.Project.Common (DotWaspDir, NodeModulesDir, WaspProjectDir, dotWaspDirInWaspProjectDir, nodeModulesDirInWaspProjectDir)
import qualified Wasp.Util.IO as IOUtil

data WaspConfigPackagesDir

packageJsonInNodeModules :: Path' (Rel NodeModulesDir) (File ())
packageJsonInNodeModules = [relfile|wasp-config/package.json|]

packagesDirInDotWaspDir :: Path' (Rel DotWaspDir) (Dir WaspConfigPackagesDir)
packagesDirInDotWaspDir = [reldir|packages|]

waspConfigDirInPackagesDir :: Path' (Rel WaspConfigPackagesDir) (Dir ())
waspConfigDirInPackagesDir = [reldir|wasp-config|]

ensureWaspConfigPackageInstalled :: Path' Abs (Dir WaspProjectDir) -> IO (Either String ())
ensureWaspConfigPackageInstalled projectDir = do
  let packageJsonPath = fromAbsFile $ projectDir </> nodeModulesDirInWaspProjectDir </> packageJsonInNodeModules
  alreadyInstalled <- doesFileExist packageJsonPath
  if alreadyInstalled
    then return $ Right ()
    else installWaspConfigPackage projectDir

installWaspConfigPackage :: Path' Abs (Dir WaspProjectDir) -> IO (Either String ())
installWaspConfigPackage projectDir = do
  srcPath <- getPackageInstallationPath WaspConfigPackage
  let packagesDir = projectDir </> dotWaspDirInWaspProjectDir </> packagesDirInDotWaspDir
      destDir = packagesDir </> waspConfigDirInPackagesDir
  case SP.parseAbsDir srcPath of
    Nothing -> return $ Left $ "Failed to parse wasp-config source path: " ++ srcPath
    Just absSrc -> do
      createDirectoryIfMissing True (fromAbsDir packagesDir)
      IOUtil.copyDirectory absSrc destDir
      chan <- newChan
      (_, exitCode) <-
        concurrently
          (readJobMessagesAndPrintThemPrefixed chan)
          (runNodeCommandAsJob projectDir "npm" ["install", "--save-dev", "file:.wasp/packages/wasp-config"] J.Wasp chan)
      return $ case exitCode of
        ExitSuccess -> Right ()
        ExitFailure _ -> Left "npm install of wasp-config failed."
