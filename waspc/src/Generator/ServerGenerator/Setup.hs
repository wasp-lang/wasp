module Generator.ServerGenerator.Setup
    ( setupServer
    ) where

import qualified System.Process as P
import System.Exit (ExitCode(..))

import qualified StrongPath as SP
import StrongPath (Path, Abs, Dir)
import qualified Generator.ServerGenerator.Common as C
import Generator.Common (ProjectRootDir)

type Stdout = String
type Stderr = String

setupServer :: Path Abs (Dir ProjectRootDir) -> IO (ExitCode, Stdout, Stderr)
setupServer projectDir = do
    let serverDir = projectDir SP.</> C.serverRootDirInProjectRootDir

    -- TODO: Check npm/node version.

    let process = (P.proc "npm" ["install"]){ P.cwd = Just (SP.toFilePath serverDir) } -- TODO: Do I need to set more stuff here?
    P.readCreateProcessWithExitCode process ""
    -- TODO: What about exceptions that command above could throw, how do we handle those?

