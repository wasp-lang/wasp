module JobTest where

import Control.Concurrent (newEmptyMVar, putMVar, takeMVar, threadDelay)
import qualified Control.Concurrent.Async as Async
import Control.Exception (bracket_)
import Control.Monad.Except (catchError)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Resource (register)
import Data.IORef (newIORef, readIORef, writeIORef)
import System.Exit (ExitCode (..))
import System.Timeout (timeout)
import Test.Hspec (Spec, describe, it, shouldBe, shouldReturn)
import qualified Wasp.Job as Job
import qualified Wasp.Job.Output as Output
import Wasp.Util (secondsToMicroSeconds)

spec_Job :: Spec
spec_Job =
  describe "Job" $ do
    it "short-circuits on a required subprocess failure" $ do
      let action = do
            Job.emitJobOutput Job.Stdout "before failure"
            Job.requireExitSuccess $ ExitFailure 7
            Job.emitJobOutput Job.Stdout "after failure"
      Output.capturing (`Job.runJob` action)
        `shouldReturn` (ExitFailure 7, "before failure")

    it "releases resources before returning" $ do
      released <- newIORef False
      let action = do
            _ <- register $ writeIORef released True
            Job.requireExitSuccess $ ExitFailure 7

      _ <- Job.runJob ignoreOutput action

      readIORef released `shouldReturn` True

    it "releases resources when cancelled" $ do
      resourceRegistered <- newEmptyMVar
      released <- newEmptyMVar
      let action = do
            _ <- register $ putMVar released ()
            liftIO $ putMVar resourceRegistered ()
            liftIO $ threadDelay $ secondsToMicroSeconds 10

      Async.withAsync (Job.runJob ignoreOutput action) $ \job -> do
        takeMVar resourceRegistered
        Async.cancel job

      timeout (secondsToMicroSeconds 5) (takeMVar released) `shouldReturn` Just ()

    it "keeps the output a cancelled job writes while releasing its resources" $ do
      resourceRegistered <- newEmptyMVar
      let cancelledJob = do
            sink <- Job.getSink
            _ <- register $ sink Job.Stdout "released"
            liftIO $ putMVar resourceRegistered ()
            liftIO $ threadDelay $ secondsToMicroSeconds 10
          finishingJob = liftIO $ takeMVar resourceRegistered
      (_, output) <-
        Output.capturing $ \sink ->
          Job.runJob sink cancelledJob `Async.race` Job.runJob sink finishingJob
      output `shouldBe` "released"

spec_capturing :: Spec
spec_capturing =
  describe "capturing" $ do
    it "returns all stdout and stderr chunks in emission order" $ do
      let action = do
            Job.emitJobOutput Job.Stdout "first "
            Job.emitJobOutput Job.Stderr "second "
            Job.emitJobOutput Job.Stdout "last"
      Output.capturing (`Job.runJob` action)
        `shouldReturn` (ExitSuccess, "first second last")

spec_withBackgroundOutputWorker :: Spec
spec_withBackgroundOutputWorker =
  describe "withBackgroundOutputWorker" $ do
    it "forwards output and stops the worker before returning its result" $ do
      started <- newEmptyMVar
      stopped <- newIORef False
      block <- newEmptyMVar
      let worker sink =
            bracket_
              (sink Job.Stdout "progress" >> putMVar started ())
              (writeIORef stopped True)
              (takeMVar block)
          action = do
            result <- Job.withBackgroundOutputWorker worker $ do
              liftIO $ takeMVar started
              return (42 :: Int)
            liftIO $ result `shouldBe` 42
            liftIO $ readIORef stopped `shouldReturn` True
      timeout (secondsToMicroSeconds 5) (Output.capturing (`Job.runJob` action))
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
      timeout (secondsToMicroSeconds 5) (Job.runJob ignoreOutput action)
        `shouldReturn` Just ExitSuccess

    it "stops the worker when the job is cancelled" $ do
      started <- newEmptyMVar
      stopped <- newIORef False
      block <- newEmptyMVar
      let worker _ =
            bracket_
              (putMVar started ())
              (writeIORef stopped True)
              (takeMVar block)
          action = Job.withBackgroundOutputWorker worker $ liftIO $ takeMVar block
          cancelJob =
            Async.withAsync (Job.runJob ignoreOutput action) $ \job -> do
              takeMVar started
              Async.cancel job
              readIORef stopped `shouldReturn` True
      timeout (secondsToMicroSeconds 5) cancelJob `shouldReturn` Just ()

ignoreOutput :: Job.Sink
ignoreOutput _ _ = return ()
