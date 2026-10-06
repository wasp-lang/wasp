{-# LANGUAGE TupleSections #-}

module Wasp.Job.Output
  ( runAndPrintPrefixedOutput,
    runAndPrintOutput,
    runAndCaptureOutput,
    raceAndPrintPrefixedOutput,
  )
where

import Control.Concurrent (newChan, readChan, writeChan)
import Control.Concurrent.Async (race, wait, withAsync)
import Control.Exception (finally)
import Control.Monad.IO.Class (liftIO)
import Data.Conduit (ConduitT, fuseBoth, fuseUpstream, runConduit, runConduitRes, yield, (.|))
import qualified Data.Conduit.List as CL
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T.IO
import System.IO (hFlush)
import Wasp.Job (Job)
import qualified Wasp.Job as Job
import Wasp.Job.Kind (JobKind)
import Wasp.Job.Output.Prefixed (JobOutput, printPrefixed)

runAndPrintPrefixedOutput :: JobKind -> Job a -> IO a
runAndPrintPrefixedOutput jobKind job =
  runConduitRes $ job `fuseUpstream` (labelWith jobKind .| printPrefixed)

runAndPrintOutput :: Job a -> IO a
runAndPrintOutput = Job.runWith $ \stream text -> do
  let handle = Job.getOutputHandle stream
  T.IO.hPutStr handle text
  hFlush handle

runAndCaptureOutput :: Job a -> IO (a, Text)
runAndCaptureOutput job = do
  (result, chunks) <- runConduitRes $ job `fuseBoth` (CL.map (\(Job.Output _ text) -> text) .| CL.consume)
  return (result, T.concat chunks)

-- | Runs both jobs concurrently and prints their prefixed output until the
-- first one finishes, then stops the other one.
raceAndPrintPrefixedOutput :: (JobKind, Job a) -> (JobKind, Job b) -> IO (Either a b)
raceAndPrintPrefixedOutput (jobKindA, jobA) (jobKindB, jobB) = do
  outputs <- newChan
  let runAndSendOutput jobKind job =
        runConduitRes $
          job `fuseUpstream` (labelWith jobKind .| CL.mapM_ (liftIO . writeChan outputs . Just))
  let runJobs = race (runAndSendOutput jobKindA jobA) (runAndSendOutput jobKindB jobB)
  let printOutputs = runConduit $ sourceUntilNothing (readChan outputs) .| printPrefixed
  -- Even if a job fails, we print the output it produced before failing.
  withAsync printOutputs $ \printing ->
    runJobs `finally` (writeChan outputs Nothing >> wait printing)
  where
    sourceUntilNothing readNext =
      liftIO readNext >>= \case
        Nothing -> return ()
        Just output -> yield output >> sourceUntilNothing readNext

labelWith :: (Monad m) => JobKind -> ConduitT Job.Output JobOutput m ()
labelWith jobKind = CL.map (jobKind,)
