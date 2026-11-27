module Wasp.Cli.Console.Reporter
  ( Reporter (..),
  )
where

import qualified Wasp.Message as Msg

-- | How Wasp says things to the user.
newtype Reporter = Reporter
  { send :: Msg.Message -> IO ()
  }
