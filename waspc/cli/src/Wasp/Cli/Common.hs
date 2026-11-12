module Wasp.Cli.Common
  ( waspSays,
    waspWarns,
    waspScreams,
  )
where

waspSays :: String -> IO ()
waspSays = putStrLn

waspWarns :: String -> IO ()
waspWarns = putStrLn

waspScreams :: String -> IO ()
waspScreams = putStrLn
