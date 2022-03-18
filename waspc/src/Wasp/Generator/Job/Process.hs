{-# LANGUAGE ScopedTypeVariables #-}

module Wasp.Generator.Job.Process
  ( runProcessAsJob,
    runNodeCommandAsJob,
    parseNodeVersion,
  )
where

import Control.Concurrent (writeChan)
import Control.Concurrent.Async (Concurrently (..))
import Data.Conduit (runConduit, (.|))
import qualified Data.Conduit.List as CL
import qualified Data.Conduit.Process as CP
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import StrongPath (Abs, Dir, Path')
import qualified StrongPath as SP
import System.Exit (ExitCode (..))
import System.IO.Error (catchIOError, isDoesNotExistError)
import qualified System.Process as P
import Text.Read (readMaybe)
import qualified Text.Regex.TDFA as R
import UnliftIO.Exception (bracket)
import qualified Wasp.Generator.Common as C
import qualified Wasp.Generator.Job as J
import Wasp.SemanticVersion (SemanticVersion (..), isVersionInBounds)

-- TODO:
--   Switch from Data.Conduit.Process to Data.Conduit.Process.Typed.
--   It is a new module meant to replace Data.Conduit.Process which is about to become deprecated.

-- | Runs a given process while streaming its stderr and stdout to provided channel. Stdin is inherited.
--   Returns exit code of the process once it finishes, and also sends it to the channel.
--   Makes sure to terminate the process if exception occurs.
runProcessAsJob :: P.CreateProcess -> J.JobType -> J.Job
runProcessAsJob process jobType chan =
  bracket
    (CP.streamingProcess process)
    (\(_, _, _, sph) -> terminateStreamingProcess sph)
    runStreamingProcessAsJob
  where
    runStreamingProcessAsJob (CP.Inherited, stdoutStream, stderrStream, processHandle) = do
      let forwardStdoutToChan =
            runConduit $
              stdoutStream
                .| CL.mapM_
                  ( \bs ->
                      writeChan chan $
                        J.JobMessage
                          { J._data = J.JobOutput (decodeUtf8 bs) J.Stdout,
                            J._jobType = jobType
                          }
                  )

      let forwardStderrToChan =
            runConduit $
              stderrStream
                .| CL.mapM_
                  ( \bs ->
                      writeChan chan $
                        J.JobMessage
                          { J._data = J.JobOutput (decodeUtf8 bs) J.Stderr,
                            J._jobType = jobType
                          }
                  )

      exitCode <-
        runConcurrently $
          Concurrently forwardStdoutToChan
            *> Concurrently forwardStderrToChan
            *> Concurrently (CP.waitForStreamingProcess processHandle)

      writeChan chan $
        J.JobMessage
          { J._data = J.JobExit exitCode,
            J._jobType = jobType
          }

      return exitCode

    terminateStreamingProcess streamingProcessHandle = do
      let processHandle = CP.streamingProcessHandleRaw streamingProcessHandle
      P.terminateProcess processHandle
      return $ ExitFailure 1

runNodeCommandAsJob :: Path' Abs (Dir a) -> String -> [String] -> J.JobType -> J.Job
runNodeCommandAsJob fromDir command args jobType chan = do
  errorOrNodeVersion <- getNodeVersion
  case errorOrNodeVersion of
    Left errorMsg -> exitWithError (ExitFailure 1) (T.pack errorMsg)
    Right nodeVersion ->
      if isVersionInBounds nodeVersion C.nodeVersionBounds
        then do
          let process = (P.proc command args) {P.cwd = Just $ SP.fromAbsDir fromDir}
          runProcessAsJob process jobType chan
        else
          exitWithError
            (ExitFailure 1)
            (T.pack $ makeNodeVersionMismatchMessage nodeVersion)
  where
    exitWithError exitCode errorMsg = do
      writeChan chan $
        J.JobMessage
          { J._data = J.JobOutput errorMsg J.Stderr,
            J._jobType = jobType
          }
      writeChan chan $
        J.JobMessage
          { J._data = J.JobExit exitCode,
            J._jobType = jobType
          }
      return exitCode

getNodeVersion :: IO (Either String SemanticVersion)
getNodeVersion = do
  (exitCode, stdout, stderr) <-
    P.readProcessWithExitCode "node" ["--version"] ""
      `catchIOError` ( \e ->
                         if isDoesNotExistError e
                           then return (ExitFailure 1, "", "Command 'node' not found.")
                           else ioError e
                     )
  return $ case exitCode of
    ExitFailure _ ->
      Left
        ( "Running 'node --version' failed: " ++ stderr
            ++ " "
            ++ waspNodeRequirementMessage
        )
    ExitSuccess -> case parseNodeVersion stdout of
      Nothing ->
        Left
          ( "Wasp failed to parse node version."
              ++ " This is most likely a bug in Wasp, please file an issue."
          )
      Just version -> Right version

parseNodeVersion :: String -> Maybe SemanticVersion
parseNodeVersion nodeVersionStr =
  case nodeVersionStr R.=~ ("v([^\\.]+).([^\\.]+).(.+)" :: String) of
    ((_, _, _, [majorStr, minorStr, patchStr]) :: (String, String, String, [String])) -> do
      mjr <- readMaybe majorStr
      mnr <- readMaybe minorStr
      ptc <- readMaybe patchStr
      return $ SemanticVersion mjr mnr ptc
    _ -> Nothing

makeNodeVersionMismatchMessage :: SemanticVersion -> String
makeNodeVersionMismatchMessage nodeVersion =
  unwords
    [ "Your node version does not match Wasp's requirements.",
      "You are running node" ++ show nodeVersion ++ ".",
      waspNodeRequirementMessage
    ]

waspNodeRequirementMessage :: String
waspNodeRequirementMessage =
  unwords
    [ "Wasp requires node " ++ show C.nodeVersionBounds ++ ".",
      "Check Wasp docs for more details: https://wasp-lang.dev/docs#requirements."
    ]
