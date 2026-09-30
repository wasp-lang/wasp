{-# LANGUAGE CPP #-}

module Wasp.Cli.TerminationSignals
  ( handleTerminationSignalsLikeInterrupt,
  )
where

#if !defined(mingw32_HOST_OS)
import Control.Concurrent (myThreadId, throwTo)
import Control.Exception (AsyncException (UserInterrupt))
import qualified System.Posix.Signals as Signals
#endif

-- | Makes SIGTERM and SIGHUP stop Wasp the same way SIGINT (Ctrl+C) does: by
-- throwing 'UserInterrupt' to the calling thread, so that the cleanup (e.g.
-- the 'bracket's stopping the processes Wasp started) runs before Wasp exits.
-- By default, these signals kill Wasp right away, skipping any cleanup.
--
-- Must be called from the main thread. Like with SIGINT, only the first signal
-- of each kind is handled, so sending it again kills Wasp right away.
--
-- Does nothing on Windows, which doesn't have these signals.
handleTerminationSignalsLikeInterrupt :: IO ()
#if defined(mingw32_HOST_OS)
handleTerminationSignalsLikeInterrupt = return ()
#else
handleTerminationSignalsLikeInterrupt = do
  mainThreadId <- myThreadId
  let interruptMainThread = Signals.CatchOnce $ throwTo mainThreadId UserInterrupt
  mapM_
    (\signal -> Signals.installHandler signal interruptMainThread Nothing)
    [Signals.sigTERM, Signals.sigHUP]
#endif
