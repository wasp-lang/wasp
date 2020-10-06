module Generator.Job.Process
    ( runProcessAsJob
    , runNodeCommandAsJob
    ) where

import           Control.Concurrent       (writeChan)
import           Control.Concurrent.Async (Concurrently (..))
import qualified Data.ByteString.Char8 as BS
import           Data.Conduit          (runConduit, (.|))
import qualified Data.Conduit.List     as CL
import qualified Data.Conduit.Process  as CP
import           System.Exit           (ExitCode (..))
import qualified System.Process        as P
import           Text.Read             (readMaybe)
import qualified Text.Regex.TDFA       as R

import qualified Generator.Common      as C
import qualified Generator.Job         as J
import           StrongPath            (Abs, Dir, Path)
import qualified StrongPath            as SP


-- | Runs a given process while streaming its stderr and stdout to provided channel.
--   Returns exit code of the process once it finishes, and also sends it to he channel.
runProcessAsJob :: P.CreateProcess -> J.JobType -> J.Job
runProcessAsJob process jobType chan = do
    (CP.ClosedStream, stdoutStream, stderrStream, processHandle) <- CP.streamingProcess process

    let forwardStdoutToChan = runConduit $ stdoutStream .| CL.mapM_
            (\bs -> writeChan chan $ J.JobMessage { J._data = J.JobOutput (BS.unpack bs) J.Stdout
                                                  , J._jobType = jobType })

    let forwardStderrToChan = runConduit $ stderrStream .| CL.mapM_
            (\bs -> writeChan chan $ J.JobMessage { J._data = J.JobOutput (BS.unpack bs) J.Stderr
                                                  , J._jobType = jobType })

    exitCode <- runConcurrently $
        Concurrently forwardStdoutToChan *>
        Concurrently forwardStderrToChan *>
        Concurrently (CP.waitForStreamingProcess processHandle)

    writeChan chan $ J.JobMessage { J._data = J.JobExit exitCode
                                  , J._jobType = jobType }

    return exitCode

runNodeCommandAsJob :: Path Abs (Dir a) -> String -> [String] -> J.JobType -> J.Job
runNodeCommandAsJob fromDir command args jobType chan = do
    errorOrNodeVersion <- getNodeVersion
    case errorOrNodeVersion of
        Left errorMsg -> exitWithError (ExitFailure 1) errorMsg
        Right nodeVersion -> if nodeVersion < C.nodeVersion
            then exitWithError (ExitFailure 1)
                 ("Your node version is too low, it should be >= " ++ C.nodeVersionAsText)
            else do
                let process = (P.proc command args) { P.cwd = Just $ SP.toFilePath fromDir }
                runProcessAsJob process jobType chan
  where
      exitWithError exitCode errorMsg = do
          writeChan chan $ J.JobMessage
              { J._data = J.JobOutput errorMsg J.Stderr
              , J._jobType = jobType }
          writeChan chan $ J.JobMessage { J._data = J.JobExit exitCode
                                        , J._jobType = jobType }
          return exitCode

      getNodeVersion :: IO (Either String (Int, Int, Int))
      getNodeVersion = do
          (exitCode, stdout, stderr) <- P.readProcessWithExitCode "node" ["--version"] ""
          return $ case exitCode of
              ExitFailure _ -> Left $ "Running 'node --version' failed: " ++ stderr
              ExitSuccess -> case parseNodeVersion stdout of
                                 Nothing -> Left "Wasp failed to parse node version."
                                 Just version -> Right version

      parseNodeVersion :: String -> Maybe (Int, Int, Int)
      parseNodeVersion nodeVersionStr =
          case nodeVersionStr R.=~ ("v([^\\.]+).([^\\.]+).(.+)" :: String) of
              ((_ , _, _, [majorStr, minorStr, patchStr]) :: (String, String, String, [String])) -> do
                  major <- readMaybe majorStr
                  minor <- readMaybe minorStr
                  patch <- readMaybe patchStr
                  return (major, minor, patch)
              _ -> Nothing
