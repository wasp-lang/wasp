module Wasp.LSP.Reactor
  ( ReactorInput (..),
    reactor,
  )
where

import Control.Concurrent.STM.TChan (TChan, readTChan)
import Control.Monad (forever)
import Control.Monad.STM (atomically)

newtype ReactorInput = ReactorAction (IO ())

-- | Run the LSP reactor. Reads actions synchronously from the 'TChan' and runs
-- them.
reactor :: TChan ReactorInput -> IO ()
reactor rin = do
  forever $ do
    ReactorAction act <- atomically $ readTChan rin
    act
