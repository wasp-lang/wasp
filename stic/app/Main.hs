module Main where

import System.Environment

import Lib (compile)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [waspFilePath, outDir] -> compile waspFilePath outDir >>=
                                either putStrLn (\_ -> print ("Success!" :: String))
    _ -> print ("Usage: ./stic <wasp_file_path> <out_dir>" :: String)
