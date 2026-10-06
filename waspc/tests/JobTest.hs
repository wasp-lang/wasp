module JobTest where

import Control.Concurrent (modifyMVar_, newEmptyMVar, newMVar, putMVar, readMVar, takeMVar, threadDelay)
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
      Output.capturing (`runJob` action)
        `shouldReturn` (ExitFailure 7, "before failure")

    it "releases resources before returning" $ do
      released <- newIORef False
      let action = do
            _ <- register $ writeIORef released True
            Job.requireExitSuccess $ ExitFailure 7

      _ <- runJob ignoreOutput action

      readIORef released `shouldReturn` True

    it "releases resources when cancelled" $ do
      resourceRegistered <- newEmptyMVar
      released <- newEmptyMVar
      let action = do
            _ <- register $ putMVar released ()
            liftIO $ putMVar resourceRegistered ()
            liftIO $ threadDelay $ secondsToMicroSeconds 10

      Async.withAsync (runJob ignoreOutput action) $ \job -> do
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
        Output.capturing $ \printer ->
          runJob printer cancelledJob `Async.race` runJob printer finishingJob
      output `shouldBe` "released"

    it "labels output with the kind set by the job" $ do
      chunks <- newMVar []
      let printer kind _ output = modifyMVar_ chunks $ return . ((kind, output) :)
          action = do
            Job.emitJobOutput Job.Stdout "wasp"
            Job.withKind Job.Db $ do
              Job.emitJobOutput Job.Stdout "db"
              Job.withKind Job.Server $ Job.emitJobOutput Job.Stdout "server"
      runJob printer action `shouldReturn` ExitSuccess
      reverse <$> readMVar chunks
        `shouldReturn` [(Job.Wasp, "wasp"), (Job.Db, "db"), (Job.Server, "server")]

    it "fails with the message set by the job" $ do
      let describe' code = "Step failed with exit code: " <> show code
      result <- Job.runJob ignoreOutput $ Job.describeFailure describe' $ Job.failWithExitCode 7
      either Job.jobFailureMessage (const "") result `shouldBe` "Step failed with exit code: 7"
      either Job.jobFailureExitCode (const 0) result `shouldBe` 7

spec_capturing :: Spec
spec_capturing =
  describe "capturing" $ do
    it "returns all stdout and stderr chunks in emission order" $ do
      let action = do
            Job.emitJobOutput Job.Stdout "first "
            Job.emitJobOutput Job.Stderr "second "
            Job.emitJobOutput Job.Stdout "last"
      Output.capturing (`runJob` action)
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
      timeout (secondsToMicroSeconds 5) (Output.capturing (`runJob` action))
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
      timeout (secondsToMicroSeconds 5) (runJob ignoreOutput action)
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
            Async.withAsync (runJob ignoreOutput action) $ \job -> do
              takeMVar started
              Async.cancel job
              readIORef stopped `shouldReturn` True
      timeout (secondsToMicroSeconds 5) cancelJob `shouldReturn` Just ()

ignoreOutput :: Job.Printer
ignoreOutput _ _ _ = return ()

-- | Runs the job and returns the exit code it finished with.
runJob :: Job.Printer -> Job.Job () -> IO ExitCode
runJob printer job = either (ExitFailure . Job.jobFailureExitCode) (const ExitSuccess) <$> Job.runJob printer job
