{-# LANGUAGE CPP #-}

module Wasp.Cli.SignalHandling
  ( withGracefulTermination,
  )
where

import Control.Concurrent (myThreadId)
import Control.Exception (AsyncException (UserInterrupt), bracket, throwTo)

#if !mingw32_HOST_OS
import qualified System.Posix.Signals as Signals
#endif

withGracefulTermination :: IO a -> IO a
#if mingw32_HOST_OS
withGracefulTermination = id
#else
withGracefulTermination action = do
  targetThreadId <- myThreadId
  let interruptCurrentCommand = Signals.Catch $ throwTo targetThreadId UserInterrupt
  bracket
    (mapM (installHandler interruptCurrentCommand) [Signals.sigINT, Signals.sigTERM])
    restoreHandlers
    (const action)
  where
    installHandler handler signal = do
      previousHandler <- Signals.installHandler signal handler Nothing
      return (signal, previousHandler)

    restoreHandlers =
      mapM_ $ \(signal, previousHandler) ->
        Signals.installHandler signal previousHandler Nothing
#endif
