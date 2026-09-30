module JobTest where

import Control.Concurrent (newChan, newEmptyMVar, putMVar, readChan, takeMVar, threadDelay)
import qualified Control.Concurrent.Async as Async
import Control.Exception (bracket_)
import Control.Monad.Except (catchError)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Resource (register)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Maybe (isNothing)
import System.Exit (ExitCode (..))
import System.Timeout (timeout)
import Test.Hspec (Spec, describe, expectationFailure, it, shouldBe, shouldReturn, shouldSatisfy)
import qualified Wasp.Job as Job
import qualified Wasp.Job.Kind as Kind
import qualified Wasp.Job.Output as Output
import qualified Wasp.Job.Output.Event as Event
import Wasp.Util (secondsToMicroSeconds)

spec_Job :: Spec
spec_Job =
  describe "Job" $ do
    it "uses the execution label for every event when reusing a job" $ do
      let job = Job.emitJobOutput Event.Stdout "output"
      let checkLabel kind = do
            events <- newChan
            Job.runJob kind job events `shouldReturn` ExitSuccess
            output <- readChan events
            exit <- readChan events
            Event._jobKind output `shouldBe` kind
            Event._jobKind exit `shouldBe` kind
      checkLabel Kind.Server
      checkLabel Kind.WebApp

    it "short-circuits on a required subprocess failure" $ do
      events <- newChan
      let action = do
            Job.emitJobOutput Event.Stdout "before failure"
            Job.requireExitSuccess $ ExitFailure 7
            Job.emitJobOutput Event.Stdout "after failure"

      exitCode <- Job.runJob Kind.Wasp action events

      exitCode `shouldBe` ExitFailure 7
      firstEvent <- readChan events
      Event._jobKind firstEvent `shouldBe` Kind.Wasp
      case Event._eventData firstEvent of
        Event.JobOutput Event.Stdout output -> output `shouldBe` "before failure"
        eventData -> expectationFailure $ "Expected stdout output, got: " <> show eventData

      secondEvent <- readChan events
      case Event._eventData secondEvent of
        Event.JobExited jobExitCode -> jobExitCode `shouldBe` ExitFailure 7
        eventData -> expectationFailure $ "Expected JobExited, got: " <> show eventData

      remainingEvent <- timeout (secondsToMicroSeconds 0.1) $ readChan events
      remainingEvent `shouldSatisfy` isNothing

    it "releases resources before emitting JobExited" $ do
      events <- newChan
      released <- newIORef False
      let action = do
            _ <- register $ writeIORef released True
            Job.requireExitSuccess $ ExitFailure 7

      _ <- Job.runJob Kind.Wasp action events

      readIORef released `shouldReturn` True
      event <- readChan events
      case Event._eventData event of
        Event.JobExited exitCode -> exitCode `shouldBe` ExitFailure 7
        eventData -> expectationFailure $ "Expected JobExited, got: " <> show eventData

    it "releases resources without emitting JobExited when cancelled" $ do
      events <- newChan
      resourceRegistered <- newEmptyMVar
      released <- newEmptyMVar
      let action = do
            _ <- register $ putMVar released ()
            liftIO $ putMVar resourceRegistered ()
            liftIO $ threadDelay $ secondsToMicroSeconds 10

      Async.withAsync (Job.runJob Kind.Wasp action events) $ \job -> do
        takeMVar resourceRegistered
        Async.cancel job

      takeMVar released
      maybeEvent <- timeout (secondsToMicroSeconds 0.1) $ readChan events
      maybeEvent `shouldSatisfy` isNothing

spec_runAndCaptureOutput :: Spec
spec_runAndCaptureOutput =
  describe "runAndCaptureOutput" $ do
    it "returns all stdout and stderr chunks in emission order" $ do
      let action = do
            Job.emitJobOutput Event.Stdout "first "
            Job.emitJobOutput Event.Stderr "second "
            Job.emitJobOutput Event.Stdout "last"
      Output.runAndCaptureOutput Kind.Wasp action
        `shouldReturn` (ExitSuccess, "first second last")

    it "returns output and the failure code after releasing resources" $ do
      released <- newIORef False
      let action = do
            _ <- register $ writeIORef released True
            Job.emitJobOutput Event.Stderr "failed"
            Job.requireExitSuccess $ ExitFailure 7
            Job.emitJobOutput Event.Stdout "unreachable"
      Output.runAndCaptureOutput Kind.Wasp action
        `shouldReturn` (ExitFailure 7, "failed")
      readIORef released `shouldReturn` True

spec_withBackgroundOutputWorker :: Spec
spec_withBackgroundOutputWorker =
  describe "withBackgroundOutputWorker" $ do
    it "forwards output and stops the worker before returning its result" $ do
      started <- newEmptyMVar
      stopped <- newIORef False
      block <- newEmptyMVar
      let worker emit =
            bracket_
              (emit Event.Stdout "progress" >> putMVar started ())
              (writeIORef stopped True)
              (takeMVar block)
          action = do
            result <- Job.withBackgroundOutputWorker worker $ do
              liftIO $ takeMVar started
              return (42 :: Int)
            liftIO $ result `shouldBe` 42
            liftIO $ readIORef stopped `shouldReturn` True
      timeout (secondsToMicroSeconds 5) (Output.runAndCaptureOutput Kind.Wasp action)
        `shouldReturn` Just (ExitSuccess, "progress")

    it "stops the worker before an enclosing action handles job failure" $ do
      started <- newEmptyMVar
      stopped <- newIORef False
      block <- newEmptyMVar
      let worker _ =
            bracket_
              (putMVar started ())
              (writeIORef stopped True)
              (takeMVar block)
          action =
            Job.withBackgroundOutputWorker
              worker
              (liftIO (takeMVar started) >> Job.failWithExitCode 7)
              `catchError` \_ -> liftIO $ readIORef stopped `shouldReturn` True
      timeout (secondsToMicroSeconds 5) (Output.runAndCaptureOutput Kind.Wasp action)
        `shouldReturn` Just (ExitSuccess, "")

    it "stops the worker when the job is cancelled" $ do
      started <- newEmptyMVar
      stopped <- newIORef False
      block <- newEmptyMVar
      events <- newChan
      let worker _ =
            bracket_
              (putMVar started ())
              (writeIORef stopped True)
              (takeMVar block)
          action = Job.withBackgroundOutputWorker worker $ liftIO $ takeMVar block
          cancelJob =
            Async.withAsync (Job.runJob Kind.Wasp action events) $ \job -> do
              takeMVar started
              Async.cancel job
              readIORef stopped `shouldReturn` True
      timeout (secondsToMicroSeconds 5) cancelJob `shouldReturn` Just ()
