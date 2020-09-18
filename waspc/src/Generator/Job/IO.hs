module Generator.Job.IO
    ( printJobMessage
    ) where

import           System.Exit   (ExitCode (..))
import           System.IO     (hPutStrLn, stderr, stdout)

import qualified Generator.Job as J


printJobMessage :: J.JobMessage -> IO ()
printJobMessage jobMsg = do
    let outHandle = case J._data jobMsg of
            J.JobOutput _ outputType ->
                case outputType of
                    J.Stdout -> stdout
                    J.Stderr -> stderr
            J.JobExit _ -> stderr
    let prefix = case J._jobType jobMsg of
            J.Server -> "\ESC[35mServer:\ESC[0m " -- Magenta
            J.WebApp -> "\ESC[36mWeb app:\ESC[0m " -- Cyan
            J.Db     -> "\ESC[37mDb:\ESC[0m " -- White
    let message = case J._data jobMsg of
            J.JobOutput output _ -> output
            J.JobExit ExitSuccess -> "Job exited successfully."
            J.JobExit (ExitFailure exitCode) -> "Job failed with exit code " ++ show exitCode
    let outputLines = map (\l -> prefix ++ l) (lines message)
    mapM_ (hPutStrLn outHandle) outputLines
