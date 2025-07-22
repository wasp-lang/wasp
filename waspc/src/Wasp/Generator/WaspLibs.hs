module Wasp.Generator.WaspLibs
  ( setUpLibs,
    waspLibsDeps,
  )
where

import Control.Exception (catch)
import StrongPath
  ( Abs,
    Dir,
    Dir',
    Path',
    Rel,
    Rel',
    basename,
    fromAbsDir,
    reldir,
    (</>),
  )
import System.Exit (ExitCode (..))
import qualified System.Process as P
import qualified Wasp.Data as Data
import qualified Wasp.ExternalConfig.Npm.Dependency as Npm.Dependency
import Wasp.Generator.Common (ProjectRootDir)
import Wasp.Generator.Monad (GeneratorError (GenericGeneratorError))
import Wasp.Util.IO (copyDirectoryIf, listDirectory)

data LibsDir

setUpLibs :: Path' Abs (Dir ProjectRootDir) -> IO [GeneratorError]
setUpLibs dstDir = do
  (setUpLibsIO >> return [])
    `catch` (\e -> return [GenericGeneratorError $ show (e :: IOError)])
  where
    setUpLibsIO :: IO ()
    setUpLibsIO = do
      srcPath <- (</> libsDirPathInDataDir) <$> Data.getAbsDataDirPath
      let dstPath = dstDir </> [reldir|libs|]
      copyDirectoryIf
        srcPath
        dstPath
        (\dirName -> basename dirName /= [reldir|node_modules|])
      ensureLibsReady dstPath

libsDirPathInDataDir :: Path' (Rel Data.DataDir) (Dir LibsDir)
libsDirPathInDataDir = [reldir|Generator/libs|]

waspLibsDeps :: [Npm.Dependency.Dependency]
waspLibsDeps =
  Npm.Dependency.fromList
    [ ("@wasp.sh/libs-auth", "file:../../libs/auth")
    ]

ensureLibsReady :: Path' Abs Dir' -> IO ()
ensureLibsReady libsDir = do
  (_, dirs) <- listDirectory libsDir
  mapM_ ensureLibReady dirs
  where
    ensureLibReady :: Path' Rel' Dir' -> IO ()
    ensureLibReady libDirRelPath = do
      let libDir = libsDir </> libDirRelPath
      installDeps libDir
      buildLib libDir

installDeps :: Path' Abs Dir' -> IO ()
installDeps libDir = do
  let npmInstallCreateProcess = npmInstallProcess {P.cwd = Just $ fromAbsDir libDir}
  (exitCode, _out, _err) <- P.readCreateProcessWithExitCode npmInstallCreateProcess ""
  case exitCode of
    ExitSuccess -> return ()
    _ -> error $ "npm install failed for library: " ++ show (fromAbsDir libDir)
  where
    npmInstallProcess = P.proc "npm" ["install"]

buildLib :: Path' Abs Dir' -> IO ()
buildLib libDir = do
  let npmInstallCreateProcess = npmBuildProcess {P.cwd = Just $ fromAbsDir libDir}
  (exitCode, _out, _err) <- P.readCreateProcessWithExitCode npmInstallCreateProcess ""
  case exitCode of
    ExitSuccess -> return ()
    _ -> error $ "npm build failed for library: " ++ show (fromAbsDir libDir)
  where
    npmBuildProcess = P.proc "npm" ["run", "build"]
