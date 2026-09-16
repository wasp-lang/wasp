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
waspWarns what = putStrLn $ Term.applyStyles [Term.Magenta] what

waspScreams :: String -> IO ()
waspScreams what = putStrLn $ Term.applyStyles [Term.Red] what
