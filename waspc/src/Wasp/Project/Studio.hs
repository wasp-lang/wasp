module Wasp.Project.Studio
  ( startStudio,
  )
where

import StrongPath (Abs, Dir, Path', relfile, (</>))
import qualified StrongPath as SP
import qualified System.Process as P
import Wasp.Generator.StudioGenerator (studioDataFileInGeneratedAppDir)
import Wasp.Project.Common (WaspProjectDir, generatedAppDirInWaspProjectDir, nodeModulesDirInWaspProjectDir)
import Wasp.Util (checkExitSuccess)

-- | Serves Wasp Studio on its own. It runs from the project's
-- @node_modules@, where compiling the project installs it.
startStudio :: Path' Abs (Dir WaspProjectDir) -> IO (Either String ())
startStudio waspProjectDir = do
  let standaloneScript =
        waspProjectDir
          </> nodeModulesDirInWaspProjectDir
          </> [relfile|@wasp/lib-studio/dist/standalone.mjs|]
  let dataFile =
        waspProjectDir
          </> generatedAppDirInWaspProjectDir
          </> studioDataFileInGeneratedAppDir
  let cp =
        (P.proc "node" [SP.fromAbsFile standaloneScript, "--data-file", SP.fromAbsFile dataFile])
          { P.cwd = Just $ SP.fromAbsDir waspProjectDir,
            -- Inherits the handles, and shuts the process down on Ctrl+C.
            -- See https://hackage.haskell.org/package/process-1.6.17.0/docs/System-Process.html#g:4.
            P.std_in = P.Inherit,
            P.std_out = P.Inherit,
            P.std_err = P.Inherit,
            P.delegate_ctlc = True
          }
  exitCode <- P.withCreateProcess cp $ \_ _ _ ph -> P.waitForProcess ph
  return $ checkExitSuccess "Studio command" exitCode
