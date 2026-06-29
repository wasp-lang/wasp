module Wasp.Cli.Common
  ( CliTemplatesDir,
    waspSays,
    waspWarns,
    waspScreams,
  )
where

import System.IO (hFlush, stdout)
import qualified Wasp.Util.Terminal as Term

data CliTemplatesDir

waspSays :: String -> IO ()
waspSays what = printAndFlush $ Term.applyStyles [Term.Yellow] what

waspWarns :: String -> IO ()
waspWarns what = printAndFlush $ Term.applyStyles [Term.Magenta] what

waspScreams :: String -> IO ()
waspScreams what = printAndFlush $ Term.applyStyles [Term.Red] what

printAndFlush :: String -> IO ()
printAndFlush what = putStrLn what >> hFlush stdout
