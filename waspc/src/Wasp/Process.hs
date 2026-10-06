{-# LANGUAGE ScopedTypeVariables #-}

module Wasp.Process
  ( InputMode (..),
    OutputStream (..),
    run,
  )
where

import Control.Concurrent.Async (Concurrently (..))
import Data.Conduit (runConduit, (.|))
import qualified Data.Conduit.List as CL
import qualified Data.Conduit.Process as CP
import qualified Data.Conduit.Text as CT
import qualified Data.Text
import System.Exit (ExitCode)
import qualified System.Info
import qualified System.Process as P
import UnliftIO.Exception (bracket, finally)

data InputMode = InheritTerminal | NoInput
  deriving (Show, Eq)

data OutputStream = Stdout | Stderr deriving (Show, Eq)

-- TODO(#4575):
--   Switch from Data.Conduit.Process to Data.Conduit.Process.Typed.
--   It is a new module meant to replace Data.Conduit.Process which is about to become deprecated.

-- | Runs the process to completion and forwards its output.
run :: InputMode -> P.CreateProcess -> (OutputStream -> Data.Text.Text -> IO ()) -> IO ExitCode
run InheritTerminal process emit = runWithStdin CP.Inherited process emit
run NoInput process emit = runWithStdin CP.ClosedStream process emit

runWithStdin :: forall stdin. (CP.InputSource stdin) => stdin -> P.CreateProcess -> (OutputStream -> Data.Text.Text -> IO ()) -> IO ExitCode
runWithStdin _stdin process emit =
  bracket
    (CP.streamingProcess process)
    cleanUpStreamingProcess
    runStreamingProcessAndStreamOutput
  where
    cleanUpStreamingProcess (_, _, _, streamingProcessHandle) =
      terminateStreamingProcess streamingProcessHandle
        `finally` CP.closeStreamingProcessHandle streamingProcessHandle

    runStreamingProcessAndStreamOutput (_ :: stdin, stdoutStream, stderrStream, processHandle) = do
      let forwardOutput outputKind stream =
            runConduit $
              stream .| CT.decodeUtf8Lenient .| CL.mapM_ (emit outputKind)

      runConcurrently $
        Concurrently (forwardOutput Stdout stdoutStream)
          *> Concurrently (forwardOutput Stderr stderrStream)
          *> Concurrently (CP.waitForStreamingProcess processHandle)

    terminateStreamingProcess streamingProcessHandle = do
      let processHandle = CP.streamingProcessHandleRaw streamingProcessHandle
      if System.Info.os == "mingw32"
        then P.terminateProcess processHandle
        else P.interruptProcessGroupOf processHandle
