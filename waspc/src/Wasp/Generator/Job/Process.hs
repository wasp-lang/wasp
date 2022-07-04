{-# LANGUAGE ScopedTypeVariables #-}

module Wasp.Generator.Job.Process
  ( runProcessAsJob,
    runNodeDependentCommandAsJob,
    parseNodeVersion,
  )
where

import Control.Concurrent (writeChan)
import Control.Concurrent.Async (Concurrently (..))
import Data.Conduit (runConduit, (.|))
import qualified Data.Conduit.List as CL
import qualified Data.Conduit.Process as CP
import qualified Data.Text as T
import StrongPath (Abs, Dir, Path')
import qualified StrongPath as SP
import System.Exit (ExitCode (..))
import System.IO.Error (catchIOError, isDoesNotExistError)
import qualified System.Info
import qualified System.Process as P
import Text.Read (readMaybe)
import qualified Text.Regex.TDFA as R
import UnliftIO.Exception (bracket)
import qualified Wasp.Generator.Common as C
import qualified Wasp.Generator.Job as J
import qualified Wasp.SemanticVersion as SV
import qualified Wasp.Util.Encoding as E

-- TODO:
--   Switch from Data.Conduit.Process to Data.Conduit.Process.Typed.
--   It is a new module meant to replace Data.Conduit.Process which is about to become deprecated.

-- | Runs a given process while streaming its stderr and stdout to provided channel. Stdin is inherited.
--   Returns exit code of the process once it finishes, and also sends it to the channel.
--   Makes sure to terminate the process (or process group on *nix) if exception occurs.
runProcessAsJob :: P.CreateProcess -> J.JobType -> J.Job
runProcessAsJob process jobType chan =
  bracket
    (CP.streamingProcess process)
    (\(_, _, _, sph) -> terminateStreamingProcess sph)
    runStreamingProcessAsJob
  where
    runStreamingProcessAsJob (CP.Inherited, stdoutStream, stderrStream, processHandle) = do
      let forwardStdoutToChan = forwardStandardOutputStreamToChan stdoutStream J.Stdout
      let forwardStderrToChan = forwardStandardOutputStreamToChan stderrStream J.Stderr

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
      where
        -- @stream@ can be stdout stream or stderr stream.
        forwardStandardOutputStreamToChan stream jobOutputType = runConduit $ stream .| CL.mapM_ forwardByteStringChunkToChan
          where
            forwardByteStringChunkToChan bs =
              writeChan chan $
                J.JobMessage
                  { -- Since this is output of a command that was supposed to be shown in the terminal,
                    -- it is our safest bet to assume it is using locale encoding (default encoding on the machine),
                    -- instead of assuming it is utf8 (like we do for text files).
                    -- Take a look at https://serokell.io/blog/haskell-with-utf8 for detailed reasoning.
                    J._data = J.JobOutput (E.decodeLocaleEncoding bs) jobOutputType,
                    J._jobType = jobType
                  }

    -- NOTE(shayne): On *nix, we use interruptProcessGroupOf instead of terminateProcess because many
    -- processes we run will spawn child processes, which themselves may spawn child processes.
    -- We want to ensure the entire process chain is stopped.
    -- We are limiting support of this to *nix only now, as Windows requires create_group=True
    -- but that surfaces an issue where a new process group that needs stdin but is started as a
    -- background process gets terminated, appearing to hang.
    -- Ref: https://stackoverflow.com/questions/61856063/spawning-a-process-with-create-group-true-set-pgid-hangs-when-starting-docke
    terminateStreamingProcess streamingProcessHandle = do
      let processHandle = CP.streamingProcessHandleRaw streamingProcessHandle
      if System.Info.os == "mingw32"
        then P.terminateProcess processHandle
        else P.interruptProcessGroupOf processHandle
      return $ ExitFailure 1

-- | First checks if correct version of node is installed on the machine, then runs the given command
-- as a Job (since it assumes this command requires node to be installed).
runNodeDependentCommandAsJob :: J.JobType -> Path' Abs (Dir a) -> (String, [String]) -> J.Job
runNodeDependentCommandAsJob jobType fromDir (command, args) chan = do
  errorOrNodeVersion <- getNodeVersion
  case errorOrNodeVersion of
    Left errorMsg -> exitWithError (ExitFailure 1) (T.pack errorMsg)
    Right nodeVersion ->
      if SV.isVersionInRange nodeVersion C.nodeVersionRange
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

getNodeVersion :: IO (Either String SV.Version)
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

parseNodeVersion :: String -> Maybe SV.Version
parseNodeVersion nodeVersionStr =
  case nodeVersionStr R.=~ ("v([^\\.]+).([^\\.]+).(.+)" :: String) of
    ((_, _, _, [majorStr, minorStr, patchStr]) :: (String, String, String, [String])) -> do
      mjr <- readMaybe majorStr
      mnr <- readMaybe minorStr
      ptc <- readMaybe patchStr
      return $ SV.Version mjr mnr ptc
    _ -> Nothing

makeNodeVersionMismatchMessage :: SV.Version -> String
makeNodeVersionMismatchMessage nodeVersion =
  unwords
    [ "Your node version does not match Wasp's requirements.",
      "You are running node " ++ show nodeVersion ++ ".",
      waspNodeRequirementMessage
    ]

waspNodeRequirementMessage :: String
waspNodeRequirementMessage =
  unwords
    [ "Wasp requires node " ++ show C.nodeVersionRange ++ ".",
      "Check Wasp docs for more details: https://wasp-lang.dev/docs#requirements."
    ]
