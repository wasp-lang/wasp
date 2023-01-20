module Wasp.Cli.Command.Utils.User
  ( getHomeDir,
    HomeDir,
  )
where

import Data.Maybe (fromJust)
import qualified StrongPath as SP
import System.Directory
  ( getHomeDirectory,
  )

data HomeDir

getHomeDir :: IO (SP.Path' SP.Abs (SP.Dir HomeDir))
getHomeDir = fromJust . SP.parseAbsDir <$> getHomeDirectory
