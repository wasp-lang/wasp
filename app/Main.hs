module Main where

import System.Environment
import System.FilePath ((</>), takeDirectory)

import CompileOptions (CompileOptions (..))
import Lib (compile)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [waspFilePath, outDir] -> do
        -- TODO(martin): Take compile options as arguments to the command, right now I hardcoded the value.
        let options = CompileOptions
                { externalCodeDirPath = (takeDirectory waspFilePath) </> "src"
                }
        result <- compile waspFilePath outDir options
        either putStrLn (\_ -> print ("Success!" :: String)) result
    _ -> print ("Usage: ./stic <wasp_file_path> <out_dir>" :: String)
