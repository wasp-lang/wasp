module Main where

import System.Environment

import Lib (compile)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [waspFilePath] -> compile waspFilePath "sticWebApp" >>= either print (\_ -> print "Success!")
    _ -> print "Usage: ./stic <wasp_file_path>"
