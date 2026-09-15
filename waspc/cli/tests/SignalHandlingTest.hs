{-# LANGUAGE CPP #-}

module SignalHandlingTest where

import Test.Hspec (Spec, describe, it, shouldReturn)
import Wasp.Cli.SignalHandling (withGracefulTermination)

#if !mingw32_HOST_OS
import Control.Concurrent (newEmptyMVar, takeMVar, threadDelay, tryPutMVar)
import Control.Exception (SomeException, finally, fromException, try)
import Control.Monad (forever, void)
import Data.IORef (newIORef, readIORef, writeIORef)
import System.Exit (ExitCode (ExitFailure))
import qualified System.Posix.Signals as Signals
import System.Timeout (timeout)
import Test.Hspec (expectationFailure, shouldBe)
#endif

spec_withGracefulTermination :: Spec
#if mingw32_HOST_OS
spec_withGracefulTermination =
  describe "withGracefulTermination" $
    it "leaves actions unchanged on Windows" $
      withGracefulTermination (return (42 :: Int)) `shouldReturn` 42
#else
spec_withGracefulTermination =
  describe "withGracefulTermination" $
    it "cleans up, restores handlers, and preserves SIGINT and SIGTERM exit statuses" $ do
      assertSignalExitAndHandlerRestoration
      assertSignalExit Signals.sigTERM (ExitFailure 143)

assertSignalExitAndHandlerRestoration :: IO ()
assertSignalExitAndHandlerRestoration = do
  restoredHandlerRan <- newEmptyMVar
  previousHandler <-
    Signals.installHandler
      Signals.sigINT
      (Signals.Catch $ void $ tryPutMVar restoredHandlerRan ())
      Nothing
  ( do
      assertSignalExit Signals.sigINT (ExitFailure 130)
      Signals.raiseSignal Signals.sigINT
      timeout 5000000 (takeMVar restoredHandlerRan) `shouldReturn` Just ()
    )
    `finally` void (Signals.installHandler Signals.sigINT previousHandler Nothing)

assertSignalExit :: Signals.Signal -> ExitCode -> IO ()
assertSignalExit signal expectedExitCode = do
  cleanupRan <- newIORef False
  outcome <-
    try
      ( timeout 5000000 $
          withGracefulTermination $
            (Signals.raiseSignal signal >> forever (threadDelay 1000000))
              `finally` writeIORef cleanupRan True
      ) :: IO (Either SomeException (Maybe ()))

  case outcome of
    Left exception -> fromException exception `shouldBe` Just expectedExitCode
    Right Nothing -> expectationFailure "Timed out waiting for signal handling"
    Right (Just ()) -> expectationFailure "Signal did not terminate the action"

  readIORef cleanupRan `shouldReturn` True
#endif
