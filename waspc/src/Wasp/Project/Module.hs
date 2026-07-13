module Wasp.Project.Module
  ( createModuleOnDisk,
    installWaspDependenciesIO,
    installModuleIO,
    buildModuleIO,
    packageNameToDirName,
  )
where

import Control.Concurrent (newChan)
import Control.Monad (forM_)
import Data.Char (isAlphaNum, toLower)
import Data.List (intercalate)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Path.IO (copyDirRecur)
import StrongPath (Abs, Dir, Dir', Path', Rel, fromAbsDir, fromAbsFile, reldir, relfile, (</>))
import StrongPath.Path (toPathAbsDir)
import System.Directory (doesFileExist, doesPathExist)
import System.Exit (ExitCode (..))
import qualified System.FilePath as FP
import qualified System.Process as P
import Validation (Validation (..))
import qualified Wasp.Data as Data
import Wasp.Generator.NpmInstall (installProjectNpmDependencies)
import Wasp.NodePackageFFI (InstallablePackage (WaspSpecPackage), RunnablePackage (ModuleBuilderPackage), ensurePackageIsAtInstallationPathInProject, getPackageProcessOptions, tryGettingInstalledPackageVersion)
import Wasp.Project.Common (WaspProjectDir)
import Wasp.Project.ExternalConfig.SrcTsConfig (parseAndValidateModuleSrcTsConfig)
import Wasp.Project.ExternalConfig.WaspTsConfig (parseAndValidateWaspTsConfig)
import qualified Wasp.Util.IO as IOUtil
import Wasp.Util.Terminal (styleCode)
import qualified Wasp.Version as WV

createModuleOnDisk :: Path' Abs (Dir WaspProjectDir) -> String -> IO (Either String ())
createModuleOnDisk moduleDir packageName = do
  doesModuleDirExist <- doesPathExist $ fromAbsDir moduleDir
  if doesModuleDirExist
    then return $ Left $ fromAbsDir moduleDir ++ " already exists."
    else do
      dataDir <- Data.getAbsDataDirPath
      copyDirRecur (toPathAbsDir $ dataDir </> moduleTemplateDirInDataDir) (toPathAbsDir moduleDir)
      replaceModuleTemplatePlaceholders moduleDir packageName
      IOUtil.renameDotfiles moduleDir moduleTemplateDotfiles
      return $ Right ()

installModuleIO :: Path' Abs (Dir WaspProjectDir) -> IO (Either String ())
installModuleIO moduleDir = do
  ensureIsModuleDir moduleDir >>= \case
    Left errorMessage -> return $ Left errorMessage
    Right () ->
      ensureValidModuleTsConfigs moduleDir >>= \case
        Left errorMessage -> return $ Left errorMessage
        Right () -> installModuleDependenciesIO moduleDir

installWaspDependenciesIO :: Path' Abs (Dir WaspProjectDir) -> IO (Either String ())
installWaspDependenciesIO projectDir = do
  ensurePackageIsAtInstallationPathInProject projectDir WaspSpecPackage
  messageChan <- newChan
  installProjectNpmDependencies messageChan projectDir

installModuleDependenciesIO :: Path' Abs (Dir WaspProjectDir) -> IO (Either String ())
installModuleDependenciesIO moduleDir = do
  ensurePackageIsAtInstallationPathInProject moduleDir WaspSpecPackage
  ensureWaspSdkTypeShimIO moduleDir
  messageChan <- newChan
  installProjectNpmDependencies messageChan moduleDir

buildModuleIO :: Path' Abs (Dir WaspProjectDir) -> IO (Either String ())
buildModuleIO moduleDir = do
  ensureIsModuleDir moduleDir >>= \case
    Left errorMessage -> return $ Left errorMessage
    Right () ->
      ensureValidModuleTsConfigs moduleDir >>= \case
        Left errorMessage -> return $ Left errorMessage
        Right () -> do
          ensureWaspSdkTypeShimIO moduleDir
          ensureInstalledModuleDependencies moduleDir >>= \case
            Left errorMessage -> return $ Left errorMessage
            Right () -> runModuleBuilder moduleDir

ensureValidModuleTsConfigs :: Path' Abs (Dir WaspProjectDir) -> IO (Either String ())
ensureValidModuleTsConfigs moduleDir = do
  srcTsConfigValidation <- parseAndValidateModuleSrcTsConfig moduleDir [relfile|tsconfig.src.json|]
  waspTsConfigValidation <- parseAndValidateWaspTsConfig moduleDir [relfile|tsconfig.wasp.json|]
  return $ case srcTsConfigValidation *> waspTsConfigValidation of
    Success _ -> Right ()
    Failure errors -> Left $ intercalate "\n" errors

ensureInstalledModuleDependencies :: Path' Abs (Dir WaspProjectDir) -> IO (Either String ())
ensureInstalledModuleDependencies moduleDir =
  tryGettingInstalledPackageVersion moduleDir WaspSpecPackage >>= \case
    Left _ -> return $ Left missingDepsError
    Right installedWaspSpecVersion
      | installedWaspSpecVersion == WV.waspVersion -> return $ Right ()
      | otherwise -> return $ Left missingDepsError
  where
    missingDepsError =
      "Your module dependencies are out of date. Run " ++ styleCode "wasp module install" ++ " to fix this."

runModuleBuilder :: Path' Abs (Dir WaspProjectDir) -> IO (Either String ())
runModuleBuilder moduleDir = do
  cp <- getPackageProcessOptions ModuleBuilderPackage ["--module-dir", fromAbsDir moduleDir]
  let cpInheritHandles =
        cp
          { P.std_in = P.Inherit,
            P.std_out = P.Inherit,
            P.std_err = P.Inherit,
            P.delegate_ctlc = True
          }
  exitCode <- P.withCreateProcess cpInheritHandles $ \_ _ _ ph -> P.waitForProcess ph
  case exitCode of
    ExitSuccess -> return $ Right ()
    ExitFailure code -> return $ Left $ "Module build failed with exit code: " ++ show code

ensureIsModuleDir :: Path' Abs (Dir WaspProjectDir) -> IO (Either String ())
ensureIsModuleDir moduleDir = do
  hasModuleSpec <- doesFileExist $ fromAbsFile $ moduleDir </> [relfile|module.wasp.ts|]
  if hasModuleSpec
    then return $ Right ()
    else return $ Left $ fromAbsDir moduleDir ++ " is not a Wasp module directory. Expected module.wasp.ts."

moduleTemplateDirInDataDir :: Path' (Rel Data.DataDir) (Dir Dir')
moduleTemplateDirInDataDir = [reldir|Cli/module-template|]

-- | Files stored without their leading dot in the module template directory,
-- to prevent tools (e.g. npm, cabal) from stripping them during packaging.
moduleTemplateDotfiles :: [String]
moduleTemplateDotfiles = ["gitignore"]

waspSdkModuleShimTemplateDirInDataDir :: Path' (Rel Data.DataDir) (Dir Dir')
waspSdkModuleShimTemplateDirInDataDir = [reldir|Generator/templates/sdk/wasp/module-shim|]

ensureWaspSdkTypeShimIO :: Path' Abs (Dir WaspProjectDir) -> IO ()
ensureWaspSdkTypeShimIO moduleDir = do
  dataDir <- Data.getAbsDataDirPath
  let waspShimDir = moduleDir </> [reldir|.wasp/wasp|]
  IOUtil.deleteDirectoryIfExists waspShimDir
  IOUtil.copyDirectory (dataDir </> waspSdkModuleShimTemplateDirInDataDir) waspShimDir

replaceModuleTemplatePlaceholders :: Path' Abs (Dir WaspProjectDir) -> String -> IO ()
replaceModuleTemplatePlaceholders moduleDir packageName = do
  forM_ ["package.json", "README.md"] $ \fileName -> do
    let filePath = fromAbsDir moduleDir FP.</> fileName
    contents <- TIO.readFile filePath
    TIO.writeFile filePath $ T.replace "__waspModulePackageName__" (T.pack packageName) contents

packageNameToDirName :: String -> FilePath
packageNameToDirName name = case sanitized of
  "" -> "wasp-module"
  _ -> sanitized
  where
    sanitized = trimDashes $ map sanitizeChar $ map toLower name
    sanitizeChar c
      | isAlphaNum c = c
      | otherwise = '-'
    trimDashes = dropWhile (== '-') . reverse . dropWhile (== '-') . reverse
