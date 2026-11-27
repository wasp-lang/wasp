module Wasp.Cli.Common
  ( waspSays,
    waspWarns,
    waspScreams,
  )
where

import qualified Wasp.Util.TextStyle as TextStyle

waspSays :: String -> IO ()
waspSays what = putStrLn $ TextStyle.applyStyles [TextStyle.Yellow] what

waspWarns :: String -> IO ()
waspWarns what = putStrLn $ TextStyle.applyStyles [TextStyle.Magenta] what

waspScreams :: String -> IO ()
waspScreams what = putStrLn $ TextStyle.applyStyles [TextStyle.Red] what
