module Wasp.Cli.Common
  ( waspSays,
    waspWarns,
    waspScreams,
  )
where

import qualified Wasp.Util.Terminal as Term

waspSays :: String -> IO ()
waspSays what = putStrLn $ Term.applyStyles [Term.Yellow] what

waspWarns :: String -> IO ()
waspWarns = putStrLn

waspScreams :: String -> IO ()
waspScreams = putStrLn
