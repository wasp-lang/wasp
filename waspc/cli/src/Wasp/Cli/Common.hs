module Wasp.Cli.Common
  ( waspSays,
    waspWarns,
    waspWarnsOnStderr,
    waspScreams,
  )
where

import System.IO (hPutStrLn, stderr)
import qualified Wasp.Util.TextStyle as TextStyle

waspSays :: String -> IO ()
waspSays what = putStrLn $ TextStyle.applyStyles [TextStyle.Yellow] what

waspWarns :: String -> IO ()
waspWarns what = putStrLn $ TextStyle.applyStyles [TextStyle.Magenta] what

waspWarnsOnStderr :: String -> IO ()
waspWarnsOnStderr what = hPutStrLn stderr $ TextStyle.applyStyles [TextStyle.Magenta] what

waspScreams :: String -> IO ()
waspScreams what = putStrLn $ TextStyle.applyStyles [TextStyle.Red] what
