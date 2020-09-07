module Generator.ServerGenerator.Start
    ( startServer
    ) where

import qualified System.Process as P
import System.Exit (ExitCode(..))

import qualified StrongPath as SP
import StrongPath (Path, Abs, Dir)
import qualified Generator.ServerGenerator.Common as C
import Generator.Common (ProjectRootDir)


startServer :: Path Abs (Dir ProjectRootDir) -> IO (ExitCode, String, String) -- ^ (exit code, stdout, stderr)
startServer projectDir = do
    let serverDir = projectDir SP.</> C.serverRootDirInProjectRootDir

    -- TODO: Check npm/node version.

    -- TODO: npm start. I need to run process this in async manner!
    --  Check out https://stackoverflow.com/a/47788165/1509394 .
    error "To be implemented"

