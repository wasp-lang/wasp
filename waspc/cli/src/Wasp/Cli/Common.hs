module Wasp.Cli.Common
  ( waspSays,
    waspWarns,
    waspWarnsOnStderr,
    waspScreams,
  )
where

import System.IO (hPutStrLn, stderr)
import qualified Wasp.Util.Terminal as Term

waspSays :: String -> IO ()
waspSays what = putStrLn $ Term.applyStyles [Term.Yellow] what

waspWarns :: String -> IO ()
waspWarns what = putStrLn $ Term.applyStyles [Term.Magenta] what

waspWarnsOnStderr :: String -> IO ()
waspWarnsOnStderr what = hPutStrLn stderr $ Term.applyStyles [Term.Magenta] what

waspScreams :: String -> IO ()
waspScreams what = putStrLn $ Term.applyStyles [Term.Red] what
