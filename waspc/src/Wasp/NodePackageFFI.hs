module Wasp.NodePackageFFI
  ( -- * Node Package FFI

    -- Provides utilities for running the TypeScript packages that come with
    -- the project's @wasp/wasp (see "Wasp.Project.WaspNpmPackage").
    RunnablePackage (..),
    getPackageProcessOptions,
  )
where

import StrongPath (Abs, Dir, File', Path', Rel, fromAbsDir, fromAbsFile, (</>))
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)
import qualified System.Process as P
import qualified Wasp.Node.Version as NodeVersion
import Wasp.Project.Common (WaspProjectDir)
import Wasp.Project.WaspNpmPackage (waspDeployScriptInWaspProjectDir, waspStudioScriptInWaspProjectDir)

-- | The packages waspc runs as separate Node.js processes. They are
-- dependencies of the project's @wasp/wasp, which ships a run script for each.
data RunnablePackage
  = DeployPackage
  | WaspStudioPackage

-- | Get a 'P.CreateProcess' for a particular package, running in the project
-- dir. The project's dependencies must be installed, which the
-- @WaspNpmPackageAvailable@ requirement of the calling command ensures.
getPackageProcessOptions :: Path' Abs (Dir WaspProjectDir) -> RunnablePackage -> [String] -> IO P.CreateProcess
getPackageProcessOptions waspProjectDir package args = do
  NodeVersion.checkUserNodeAndNpmMeetWaspRequirements >>= \case
    Left errorMsg -> do
      hPutStrLn stderr errorMsg
      exitFailure
    Right () -> pure ()

  let scriptFile = waspProjectDir </> runScriptInWaspProjectDir package
  return $ (P.proc "node" (fromAbsFile scriptFile : args)) {P.cwd = Just $ fromAbsDir waspProjectDir}

runScriptInWaspProjectDir :: RunnablePackage -> Path' (Rel WaspProjectDir) File'
runScriptInWaspProjectDir = \case
  DeployPackage -> waspDeployScriptInWaspProjectDir
  WaspStudioPackage -> waspStudioScriptInWaspProjectDir
