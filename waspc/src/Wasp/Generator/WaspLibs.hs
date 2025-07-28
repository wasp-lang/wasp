module Wasp.Generator.WaspLibs
  ( setUpLibs,
    waspLibsDeps,
    libsRootDirInGeneratedCodeDir,
  )
where

import Control.Exception (catch)
import StrongPath
  ( Abs,
    Dir,
    Path',
    Rel,
    basename,
    castRel,
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
import Wasp.Project.Common (generatedCodeDirInDotWaspDir)
import Wasp.Util.IO (copyDirectoryIf, listDirectory)

data LibsSourceDir

data LibsRootDir

setUpLibs :: Path' Abs (Dir ProjectRootDir) -> IO [GeneratorError]
setUpLibs projectRootDir = do
  (setUpLibsIO >> return [])
    `catch` (\e -> return [GenericGeneratorError $ show (e :: IOError)])
  where
    setUpLibsIO :: IO ()
    setUpLibsIO = do
      srcPath <- (</> libsDirPathInDataDir) <$> Data.getAbsDataDirPath
      let dstPath = projectRootDir </> libsRootDirInProjectRootDir
      copyDirectoryIf
        srcPath
        dstPath
        (\dirName -> basename dirName /= [reldir|node_modules|])
      ensureLibsReady dstPath

-- TODO: dedupe this with the same logic in SDK
-- The libs need to follow the SDK location in the generated code directory.
libsRootDirInProjectRootDir :: Path' (Rel ProjectRootDir) (Dir LibsRootDir)
libsRootDirInProjectRootDir =
  [reldir|../|]
    </> basename generatedCodeDirInDotWaspDir
    </> [reldir|libs|]

libsRootDirInGeneratedCodeDir :: Path' (Rel ProjectRootDir) (Dir LibsRootDir)
libsRootDirInGeneratedCodeDir = [reldir|libs|]

libsDirPathInDataDir :: Path' (Rel Data.DataDir) (Dir LibsSourceDir)
libsDirPathInDataDir = [reldir|Generator/libs|]

waspLibsDeps :: [Npm.Dependency.Dependency]
waspLibsDeps =
  Npm.Dependency.fromList
    [ ("@wasp.sh/libs-auth", "file:../../libs/auth")
    ]

ensureLibsReady :: Path' Abs (Dir d) -> IO ()
ensureLibsReady libsDir = do
  (_, dirs) <- listDirectory libsDir
  mapM_ ensureLibReady dirs
  where
    ensureLibReady :: Path' (Rel r) (Dir d) -> IO ()
    ensureLibReady libDirRelPath = do
      let libDir = libsDir </> castRel libDirRelPath
      installDeps libDir
      buildLib libDir

installDeps :: Path' Abs (Dir d) -> IO ()
installDeps libDir = do
  let npmInstallCreateProcess = npmInstallProcess {P.cwd = Just $ fromAbsDir libDir}
  (exitCode, _out, _err) <- P.readCreateProcessWithExitCode npmInstallCreateProcess ""
  case exitCode of
    ExitSuccess -> return ()
    _ -> error $ "npm install failed for library: " ++ show (fromAbsDir libDir)
  where
    npmInstallProcess = P.proc "npm" ["install"]

buildLib :: Path' Abs (Dir d) -> IO ()
buildLib libDir = do
  let npmInstallCreateProcess = npmBuildProcess {P.cwd = Just $ fromAbsDir libDir}
  (exitCode, _out, _err) <- P.readCreateProcessWithExitCode npmInstallCreateProcess ""
  case exitCode of
    ExitSuccess -> return ()
    _ -> error $ "npm build failed for library: " ++ show (fromAbsDir libDir)
  where
    npmBuildProcess = P.proc "npm" ["run", "build"]
