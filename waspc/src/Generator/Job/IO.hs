module Generator.Job.IO
    ( printJobMessage
    ) where

import           System.Exit   (ExitCode (..))
import           System.IO     (hPutStrLn, stderr, stdout)

import qualified Generator.Job as J
import qualified Util.Terminal as Term


printJobMessage :: J.JobMessage -> IO ()
printJobMessage jobMsg = do
    let outHandle = case J._data jobMsg of
            J.JobOutput _ outputType ->
                case outputType of
                    J.Stdout -> stdout
                    J.Stderr -> stderr
            J.JobExit _ -> stdout
    let jobType = case J._jobType jobMsg of
            J.Server -> Term.applyStyles [Term.Magenta] "Server"
            J.WebApp -> Term.applyStyles [Term.Cyan] "Web app"
            J.Db     -> Term.applyStyles [Term.White] "Db"
    let jobOutputType = if outHandle == stderr then " (stderr)" else ""
    let prefix = jobType ++ jobOutputType ++ ": "
    let message = case J._data jobMsg of
            J.JobOutput output _ -> output
            J.JobExit ExitSuccess -> "Job exited successfully."
            J.JobExit (ExitFailure exitCode) -> "Job failed with exit code " ++ show exitCode
    let outputLines = map (prefix ++) (lines message)
    mapM_ (hPutStrLn outHandle) outputLines
