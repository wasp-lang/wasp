module Wasp.Cli.Command.Deploy
  ( deploy,
  )
where

import Control.Monad.IO.Class (liftIO)
import StrongPath (reldir, (</>))
import qualified StrongPath as SP
import System.Directory (doesDirectoryExist)
import qualified System.Process as P
import Wasp.Cli.Command (Command)
import Wasp.Cli.Command.Common (findWaspProjectRootDirFromCwd)
import qualified Wasp.Data as Data
import Wasp.Util (unlessM)

-- TODO:
--  - check for correct node version
--  - handle interactive input
--  - send deploy telemetry event
deploy :: [String] -> Command ()
deploy cmdArgs = do
  waspProjectDir <- findWaspProjectRootDirFromCwd
  liftIO $ do
    waspDataDir <- Data.getAbsDataDirPath
    let deployPackageDir = waspDataDir </> [reldir|packages/deploy|]
    let nodeModulesDir = deployPackageDir </> [reldir|node_modules|]
    unlessM (doesDirectoryExist $ SP.toFilePath nodeModulesDir) $ do
      (_, _, _, ph1) <- P.createProcess (P.proc "npm" ["install"]) {P.cwd = Just $ SP.toFilePath deployPackageDir}
      _ <- P.waitForProcess ph1
      return ()
    (_, _, _, ph2) <-
      P.createProcess
        ( P.proc
            "node"
            (["dist/index.js"] ++ cmdArgs ++ ["--wasp-dir", SP.toFilePath waspProjectDir])
        )
          { P.cwd = Just $ SP.toFilePath deployPackageDir
          }
    _ <- P.waitForProcess ph2
    return ()
