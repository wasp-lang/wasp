module Wasp.Project.Module
  ( ModuleBuildMode (..),
    createModuleOnDisk,
    installWaspDependenciesIO,
    installModuleIO,
    buildModuleIO,
    packageNameToDirName,
  )
where

import Control.Concurrent (newChan)
import Control.Monad (forM_)
import Data.Char (isAlphaNum, toLower)
import Data.Maybe (fromJust)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Path.IO (copyDirRecur)
import StrongPath (Abs, Dir, Dir', Path', Rel, fromAbsDir, fromAbsFile, reldir, relfile, (</>))
import qualified StrongPath as SP
import StrongPath.Path (toPathAbsDir)
import System.Directory (doesFileExist, doesPathExist)
import System.Exit (ExitCode (..))
import qualified System.FilePath as FP
import qualified System.Process as P
import qualified Wasp.Data as Data
import Wasp.Generator.NpmInstall (installProjectNpmDependencies)
import Wasp.NodePackageFFI (InstallablePackage (WaspSpecPackage), RunnablePackage (ModuleBuilderPackage), ensurePackageIsAtInstallationPathInProject, getPackageProcessOptions, tryGettingInstalledPackageVersion)
import Wasp.Project.Common (WaspProjectDir)
import Wasp.Util.Terminal (styleCode)
import qualified Wasp.Version as WV

data ModuleBuildMode = BuildOnce | BuildAndWatch

createModuleOnDisk :: FilePath -> String -> IO (Either String ())
createModuleOnDisk moduleDir packageName = do
  doesModuleDirExist <- doesPathExist moduleDir
  if doesModuleDirExist
    then return $ Left $ moduleDir ++ " already exists."
    else do
      dataDir <- Data.getAbsDataDirPath
      let absModuleDir = SP.castDir $ fromJust $ SP.parseAbsDir moduleDir
      copyDirRecur (toPathAbsDir $ dataDir </> moduleTemplateDirInDataDir) (toPathAbsDir absModuleDir)
      replaceModuleTemplatePlaceholders absModuleDir packageName
      return $ Right ()

installModuleIO :: Path' Abs (Dir WaspProjectDir) -> IO (Either String ())
installModuleIO moduleDir = do
  ensureIsModuleDir moduleDir >>= \case
    Left errorMessage -> return $ Left errorMessage
    Right () -> installWaspDependenciesIO moduleDir

installWaspDependenciesIO :: Path' Abs (Dir WaspProjectDir) -> IO (Either String ())
installWaspDependenciesIO projectDir = do
  ensurePackageIsAtInstallationPathInProject projectDir WaspSpecPackage
  messageChan <- newChan
  installProjectNpmDependencies messageChan projectDir

buildModuleIO :: Path' Abs (Dir WaspProjectDir) -> ModuleBuildMode -> IO (Either String ())
buildModuleIO moduleDir buildMode = do
  ensureIsModuleDir moduleDir >>= \case
    Left errorMessage -> return $ Left errorMessage
    Right () ->
      ensureInstalledModuleDependencies moduleDir >>= \case
        Left errorMessage -> return $ Left errorMessage
        Right () -> runModuleBuilder moduleDir buildMode

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

runModuleBuilder :: Path' Abs (Dir WaspProjectDir) -> ModuleBuildMode -> IO (Either String ())
runModuleBuilder moduleDir buildMode = do
  cp <- getPackageProcessOptions ModuleBuilderPackage $ buildModuleBuilderArgs moduleDir buildMode
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

buildModuleBuilderArgs :: Path' Abs (Dir WaspProjectDir) -> ModuleBuildMode -> [String]
buildModuleBuilderArgs moduleDir buildMode =
  ["--module-dir", fromAbsDir moduleDir]
    ++ case buildMode of
      BuildOnce -> []
      BuildAndWatch -> ["--watch"]

ensureIsModuleDir :: Path' Abs (Dir WaspProjectDir) -> IO (Either String ())
ensureIsModuleDir moduleDir = do
  hasModuleSpec <- doesFileExist $ fromAbsFile $ moduleDir </> [relfile|module.wasp.ts|]
  if hasModuleSpec
    then return $ Right ()
    else return $ Left $ fromAbsDir moduleDir ++ " is not a Wasp module directory. Expected module.wasp.ts."

moduleTemplateDirInDataDir :: Path' (Rel Data.DataDir) (Dir Dir')
moduleTemplateDirInDataDir = [reldir|Cli/module-template|]

replaceModuleTemplatePlaceholders :: Path' Abs (Dir WaspProjectDir) -> String -> IO ()
replaceModuleTemplatePlaceholders moduleDir packageName = do
  forM_ ["package.json", "README.md"] $ \fileName -> do
    let filePath = fromAbsDir moduleDir FP.</> fileName
    contents <- TIO.readFile filePath
    TIO.writeFile filePath $ T.replace "__waspModulePackageName__" (T.pack packageName) contents

packageNameToDirName :: String -> FilePath
packageNameToDirName = sanitizePackageName

sanitizePackageName :: String -> String
sanitizePackageName name = case sanitized of
  "" -> "wasp-module"
  _ -> sanitized
  where
    sanitized = trimDashes $ map sanitizeChar $ map toLower name
    sanitizeChar c
      | isAlphaNum c = c
      | otherwise = '-'
    trimDashes = dropWhile (== '-') . reverse . dropWhile (== '-') . reverse
