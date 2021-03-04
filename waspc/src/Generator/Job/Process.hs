{-# LANGUAGE ScopedTypeVariables #-}

module Generator.Job.Process
    ( runProcessAsJob
    , runNodeCommandAsJob
    ) where

import           Control.Concurrent       (writeChan)
import           Control.Concurrent.Async (Concurrently (..))
import           Control.Exception        (bracket)
import           Data.Conduit             (runConduit, (.|))
import qualified Data.Conduit.List        as CL
import qualified Data.Conduit.Process     as CP
import qualified Data.Text                as T
import           Data.Text.Encoding       (decodeUtf8)
import           System.Exit              (ExitCode (..))
import           System.IO.Error          (catchIOError, isDoesNotExistError)
import qualified System.Process           as P
import           Text.Read                (readMaybe)
import qualified Text.Regex.TDFA          as R

import qualified Generator.Common         as C
import qualified Generator.Job            as J
import           StrongPath               (Abs, Dir, Path)
import qualified StrongPath               as SP

-- TODO:
--   Switch from Data.Conduit.Process to Data.Conduit.Process.Typed.
--   It is a new module meant to replace Data.Conduit.Process which is about to become deprecated.

-- | Runs a given process while streaming its stderr and stdout to provided channel. Stdin is inherited.
--   Returns exit code of the process once it finishes, and also sends it to the channel.
--   Makes sure to terminate the process if exception occurs.
runProcessAsJob :: P.CreateProcess -> J.JobType -> J.Job
runProcessAsJob process jobType chan = bracket
    (CP.streamingProcess process)
    (\(_, _, _, sph) -> terminateStreamingProcess sph)
    runStreamingProcessAsJob
  where
    runStreamingProcessAsJob (CP.Inherited, stdoutStream, stderrStream, processHandle) = do
      let forwardStdoutToChan = runConduit $ stdoutStream .| CL.mapM_
              (\bs -> writeChan chan $ J.JobMessage { J._data = J.JobOutput (decodeUtf8 bs) J.Stdout
                                                    , J._jobType = jobType })

      let forwardStderrToChan = runConduit $ stderrStream .| CL.mapM_
              (\bs -> writeChan chan $ J.JobMessage { J._data = J.JobOutput (decodeUtf8 bs) J.Stderr
                                                    , J._jobType = jobType })

      exitCode <- runConcurrently $
          Concurrently forwardStdoutToChan *>
          Concurrently forwardStderrToChan *>
          Concurrently (CP.waitForStreamingProcess processHandle)

      writeChan chan $ J.JobMessage { J._data = J.JobExit exitCode
                                    , J._jobType = jobType }

      return exitCode

    terminateStreamingProcess streamingProcessHandle = do
        let processHandle = CP.streamingProcessHandleRaw streamingProcessHandle
        P.terminateProcess processHandle
        return $ ExitFailure 1

runNodeCommandAsJob :: Path Abs (Dir a) -> String -> [String] -> J.JobType -> J.Job
runNodeCommandAsJob fromDir command args jobType chan = do
    errorOrNodeVersion <- getNodeVersion
    case errorOrNodeVersion of
        Left errorMsg -> exitWithError (ExitFailure 1) (T.pack errorMsg)
        Right nodeVersion -> if nodeVersion < C.nodeVersion
            then exitWithError (ExitFailure 1)
                 (T.pack $ "Your node version is too low. " ++ waspNodeRequirementMessage)
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
              `catchIOError` (\e -> if isDoesNotExistError e
                                    then return (ExitFailure 1, "", "Command 'node' not found.")
                                    else ioError e)
          return $ case exitCode of
              ExitFailure _ -> Left ("Running 'node --version' failed: " ++ stderr
                                     ++ " " ++ waspNodeRequirementMessage)
              ExitSuccess -> case parseNodeVersion stdout of
                  Nothing -> Left ("Wasp failed to parse node version."
                                   ++ " This is most likely a bug in Wasp, please file an issue.")
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

      waspNodeRequirementMessage = "Wasp requires node >= " ++ C.nodeVersionAsText ++ " ."
          ++ " Check Wasp docs for more details: https://wasp-lang.dev/docs#requirements ."
