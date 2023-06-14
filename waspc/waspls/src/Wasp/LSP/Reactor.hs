module Wasp.LSP.Reactor
  ( ReactorInput (..),
    reactor,
  )
where

import Control.Concurrent.STM (TChan, atomically, readTChan)
import Control.Monad (forever)

newtype ReactorInput = ReactorAction (IO ())

-- | Run the LSP reactor. Reads actions synchronously from the 'TChan' and runs
-- them.
reactor :: TChan ReactorInput -> IO ()
reactor rin = do
  forever $ do
    ReactorAction act <- atomically $ readTChan rin
    act
