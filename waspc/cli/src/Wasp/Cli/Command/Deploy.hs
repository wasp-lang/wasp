module Wasp.Cli.Command.Deploy
  ( deploy,
  )
where

import Control.Monad.IO.Class (liftIO)
import qualified StrongPath as SP
import System.Environment (getEnv)
import qualified System.Process as P
import Wasp.Cli.Command (Command)
import Wasp.Cli.Command.Common (findWaspProjectRootDirFromCwd)

-- TODO:
--  - check for correct node version
--  - handle interactive input
deploy :: [String] -> Command ()
deploy cmdArgs = do
  waspProjectDir <- findWaspProjectRootDirFromCwd
  waspDataDir <- liftIO $ getEnv "waspc_datadir"
  let deployPackageDir = waspDataDir ++ "/packages/deploy"
  -- TODO: check for node_modules existence first
  (_, _, _, ph1) <- liftIO $ P.createProcess (P.proc "npm" ["install"]) {P.cwd = Just deployPackageDir}
  _ <- liftIO $ P.waitForProcess ph1
  (_, _, _, ph2) <-
    liftIO $
      P.createProcess
        ( P.proc
            "node"
            (["dist/index.js"] ++ cmdArgs ++ ["--wasp-dir", SP.toFilePath waspProjectDir])
        )
          { P.cwd = Just deployPackageDir
          }
  _ <- liftIO $ P.waitForProcess ph2
  return ()
