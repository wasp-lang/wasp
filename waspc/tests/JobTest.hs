module JobTest where

import Control.Concurrent (newChan, newEmptyMVar, putMVar, readChan, takeMVar, threadDelay)
import qualified Control.Concurrent.Async as Async
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Resource (register)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Maybe (isNothing)
import System.Exit (ExitCode (..))
import System.Timeout (timeout)
import Test.Hspec (Spec, describe, expectationFailure, it, shouldBe, shouldReturn, shouldSatisfy)
import qualified Wasp.Job as Job
import Wasp.Util (secondsToMicroSeconds)

spec_Job :: Spec
spec_Job =
  describe "Job" $ do
    it "short-circuits on a required subprocess failure" $ do
      events <- newChan
      let action = do
            Job.emitJobOutput Job.Stdout "before failure"
            Job.requireExitSuccess $ ExitFailure 7
            Job.emitJobOutput Job.Stdout "after failure"

      exitCode <- Job.runJob (Job.makeJob Job.Wasp action) events

      exitCode `shouldBe` ExitFailure 7
      firstEvent <- readChan events
      Job._jobKind firstEvent `shouldBe` Job.Wasp
      case Job._eventData firstEvent of
        Job.JobOutput output Job.Stdout -> output `shouldBe` "before failure"
        eventData -> expectationFailure $ "Expected stdout output, got: " <> show eventData

      secondEvent <- readChan events
      case Job._eventData secondEvent of
        Job.JobExited jobExitCode -> jobExitCode `shouldBe` ExitFailure 7
        eventData -> expectationFailure $ "Expected JobExited, got: " <> show eventData

      remainingEvent <- timeout (secondsToMicroSeconds 0.1) $ readChan events
      remainingEvent `shouldSatisfy` isNothing

    it "releases resources before emitting JobExited" $ do
      events <- newChan
      released <- newIORef False
      let action = do
            _ <- register $ writeIORef released True
            Job.requireExitSuccess $ ExitFailure 7

      _ <- Job.runJob (Job.makeJob Job.Wasp action) events

      readIORef released `shouldReturn` True
      event <- readChan events
      case Job._eventData event of
        Job.JobExited exitCode -> exitCode `shouldBe` ExitFailure 7
        eventData -> expectationFailure $ "Expected JobExited, got: " <> show eventData

    it "releases resources without emitting JobExited when cancelled" $ do
      events <- newChan
      resourceRegistered <- newEmptyMVar
      released <- newEmptyMVar
      let action = do
            _ <- register $ putMVar released ()
            liftIO $ putMVar resourceRegistered ()
            liftIO $ threadDelay $ secondsToMicroSeconds 10

      Async.withAsync (Job.runJob (Job.makeJob Job.Wasp action) events) $ \job -> do
        takeMVar resourceRegistered
        Async.cancel job

      takeMVar released
      maybeEvent <- timeout (secondsToMicroSeconds 0.1) $ readChan events
      maybeEvent `shouldSatisfy` isNothing
