{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}

module Wasp.Generator.Job.Process
  ( runProcessAsJob,
    runNodeDependentCommandAsJob,
    parseNodeVersion,
  )
where

import Control.Concurrent (writeChan)
import Control.Concurrent.Async (Concurrently (..))
import Data.ByteString (ByteString)
import Data.Conduit (runConduit, (.|))
import qualified Data.Conduit.List as CL
import qualified Data.Conduit.Process as CP
import qualified Data.Knob as K
import qualified Data.Text as T
import qualified Data.Text.IO as T
import GHC.IO.Encoding (initLocaleEncoding)
import GHC.IO.Handle (hSetEncoding)
import StrongPath (Abs, Dir, Path')
import qualified StrongPath as SP
import System.Exit (ExitCode (..))
import System.IO (IOMode (ReadMode))
import System.IO.Error (catchIOError, isDoesNotExistError)
import qualified System.Info
import qualified System.Process as P
import Text.Read (readMaybe)
import qualified Text.Regex.TDFA as R
import UnliftIO.Exception (bracket)
import qualified Wasp.Generator.Common as C
import qualified Wasp.Generator.Job as J
import qualified Wasp.SemanticVersion as SV

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
            forwardByteStringChunkToChan bs = do
              content <- decodeLocaleEncoding bs
              writeChan chan $
                J.JobMessage
                  { J._data = J.JobOutput content jobOutputType,
                    J._jobType = jobType
                  }

            decodeLocaleEncoding :: ByteString -> IO T.Text
            decodeLocaleEncoding bs = do
              -- TODO: We turn bytestring into handle, because we want to use Haskell's capabilities
              --   for decoding encodings, and those are most easily available when dealing with handles.
              --   This is somewhat silly, since we go from handles to bytestring back to handles, and we have to use
              --   `knob` library which is cool but is very small and it would be nicer to not have to use anything.
              --   We set encoding to locale encoding, since that is the best option when dealing with standard in/outputs.
              --   Here is a blog explaining this in more details: https://serokell.io/blog/haskell-with-utf8 .
              --   Nicer solutions could be:
              --   1. Run the streaming process differently, so that we get handles from it, and not bytestrings.
              --      Then we just encoding to initLocaleEncoding, do hGetContents, and that is it.
              --      Basically we can skip the first step with the `knob`.
              --   2. Use (mkTextDecoder initLocaleEncoding) to decode the ByteString (it has pretty complex types though).
              handle <- K.newKnob bs >>= \k -> K.newFileHandle k "job_stdout/stderr" ReadMode
              hSetEncoding handle initLocaleEncoding
              T.hGetContents handle

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
