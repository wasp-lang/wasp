{-# LANGUAGE CPP #-}

module Wasp.Cli.SignalHandling
  ( withGracefulTermination,
  )
where

#if !mingw32_HOST_OS
import Control.Concurrent (myThreadId)
import Control.Exception (bracket, throwTo)
import Control.Monad (void)
import System.Exit (ExitCode (ExitFailure))
import qualified System.Posix.Signals as Signals
#endif

withGracefulTermination :: IO a -> IO a
#if mingw32_HOST_OS
withGracefulTermination = id
#else
withGracefulTermination action = do
  targetThreadId <- myThreadId
  let withTerminationHandler signal innerAction =
        bracket
          (Signals.installHandler signal (Signals.Catch $ throwTo targetThreadId $ exitCodeForSignal signal) Nothing)
          (\previousHandler -> void $ Signals.installHandler signal previousHandler Nothing)
          (const innerAction)
  withTerminationHandler Signals.sigINT $
    withTerminationHandler Signals.sigTERM action
  where
    exitCodeForSignal signal = ExitFailure $ 128 + fromIntegral signal
#endif
