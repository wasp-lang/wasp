module Wasp.Cli.Common
  ( CliTemplatesDir,
    waspSays,
    waspWarns,
    waspScreams,
  )
where

import qualified Wasp.Util.Terminal as Term

data CliTemplatesDir

waspSays :: String -> IO ()
waspSays what = putStrLn $ Term.applyStyles [Term.Yellow] what

waspWarns :: String -> IO ()
waspWarns what = putStrLn $ Term.applyStyles [Term.Magenta] what

waspScreams :: String -> IO ()
waspScreams what = putStrLn $ Term.applyStyles [Term.Red] what
