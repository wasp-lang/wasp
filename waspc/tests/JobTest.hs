module JobTest where

import Control.Concurrent (newEmptyMVar, putMVar, takeMVar, threadDelay)
import qualified Control.Concurrent.Async as Async
import Control.Exception (ErrorCall (..), bracket_, throwIO, try)
import Data.Conduit (runConduitRes, (.|))
import qualified Data.Conduit.List as CL
import Data.IORef (modifyIORef, newIORef, readIORef)
import System.Exit (ExitCode (..))
import System.Timeout (timeout)
import Test.Hspec (Spec, describe, it, shouldBe, shouldReturn)
import qualified Wasp.Job as Job
import qualified Wasp.Job.Output as Output
import Wasp.Process (OutputStream (..))
import Wasp.Util (secondsToMicroSeconds)

spec_Job :: Spec
spec_Job =
  describe "Job" $ do
    it "runs jobs in sequence" $ do
      let job = do
            Job.emit Stdout "first "
            Job.emit Stderr "second "
            Job.emit Stdout "last"
            return ExitSuccess
      Output.runAndCaptureOutput job `shouldReturn` (ExitSuccess, "first second last")

    it "passes output to the callback in order" $ do
      received <- newIORef []
      let job = Job.emit Stdout "first" >> Job.emit Stderr "second" >> return (42 :: Int)
      Job.runWith (\stream text -> modifyIORef received ((stream, text) :)) job `shouldReturn` 42
      (reverse <$> readIORef received) `shouldReturn` [(Stdout, "first"), (Stderr, "second")]

spec_fromCallback :: Spec
spec_fromCallback =
  describe "Job.fromCallback" $ do
    it "streams everything the action emits before returning its result" $ do
      let job = Job.fromCallback $ \emit -> do
            mapM_ (emit Stdout) ["a", "b", "c"]
            return (7 :: Int)
      Output.runAndCaptureOutput job `shouldReturn` (7, "abc")

    it "rethrows the action's exception" $ do
      let job = Job.fromCallback $ \_ -> throwIO (ErrorCall "boom") :: IO ()
      try (Output.runAndCaptureOutput job) `shouldReturn` Left (ErrorCall "boom")

    it "cancels the action when the job is cancelled" $ do
      started <- newEmptyMVar
      stopped <- newEmptyMVar
      let job = Job.fromCallback $ \_ ->
            bracket_ (putMVar started ()) (putMVar stopped ()) (threadDelay $ secondsToMicroSeconds 10)
      Async.withAsync (Output.runAndCaptureOutput job) $ \running -> do
        takeMVar started
        Async.cancel running
      timeout (secondsToMicroSeconds 5) (takeMVar stopped) `shouldReturn` Just ()

    it "cancels the action when the consumer stops early" $ do
      stopped <- newEmptyMVar
      let job = Job.fromCallback $ \emit ->
            bracket_ (emit Stdout "first") (putMVar stopped ()) (threadDelay $ secondsToMicroSeconds 10)
      outputs <- timeout (secondsToMicroSeconds 5) $ runConduitRes $ job .| CL.take 1
      outputs `shouldBe` Just [Job.Output Stdout "first"]
      timeout (secondsToMicroSeconds 5) (takeMVar stopped) `shouldReturn` Just ()
