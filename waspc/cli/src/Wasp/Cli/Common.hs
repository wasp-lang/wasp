module Wasp.Cli.Common
  ( waspSays,
    waspWarns,
    waspScreams,
  )
where

import qualified Data.Text as T
import System.IO (stdout)
import qualified Wasp.Util.Interactive as Interactive

waspSays :: String -> IO ()
waspSays = Interactive.writeOutput Interactive.WaspMessage stdout . T.pack . (++ "\n")

waspWarns :: String -> IO ()
waspWarns = waspSays

waspScreams :: String -> IO ()
waspScreams = waspSays
