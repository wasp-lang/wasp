module Wasp.LSP.Reactor
  ( -- * Reactor Thread

    -- To avoid long-running tasks blocking the main thread that serves responses
    -- to the LSP client, these tasks are run on the \"reactor thread\". This
    -- thread reacts to inputs sent on a 'TChan' and runs the corresponding IO
    -- action.
    ReactorInput (..),
    reactor,
    startReactorThread,
  )
where

import Control.Concurrent (MVar, forkFinally, readMVar)
import Control.Concurrent.Async (async, waitAnyCancel)
import Control.Concurrent.STM (TChan, atomically, readTChan)
import Control.Monad (forever, void)

-- | An action sent to the reactor thread.
newtype ReactorInput = ReactorAction (IO ())

-- | Run the LSP reactor in the thread that runs this function. Reads actions
-- synchronously from the 'TChan' and executes them.
--
-- The reactor does not catch any error that occurs in the actions it runs.
reactor :: TChan ReactorInput -> IO ()
reactor rin = do
  forever $ do
    ReactorAction act <- atomically $ readTChan rin
    act

-- | @startReactorThread lifetime rin@ spawns a thread that runs the reactor
-- and runs forever until it is told to stop, via @lifetime@ being filled.
--
-- When the reactor crashes, a new thread that runs the reactor is immediately
-- spawned.
startReactorThread :: MVar () -> TChan ReactorInput -> IO ()
startReactorThread lifetime rin = run
  where
    run = void $
      forkFinally (runUntilMVarIsFull lifetime $ reactor rin) $ \case
        Left _ -> run -- Restart reactor on crash.
        Right () -> pure () -- Reactor ended peacefully, don't restart.

runUntilMVarIsFull :: MVar () -> IO () -> IO ()
runUntilMVarIsFull lifetime action =
  void $ waitAnyCancel =<< traverse async [action, readMVar lifetime]
