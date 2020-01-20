module Main where

import System.Environment
import System.Directory
import qualified System.FilePath as FilePath
import Path ((</>), reldir)
import qualified Path
import qualified Path.Aliases as Path

import CompileOptions (CompileOptions (..))
import Lib (compile)


main :: IO ()
main = do
  absCwdPath <- getCurrentDirectory >>= Path.parseAbsDir
  args <- getArgs
  case args of
    [waspFilePath, outDirPath] -> do
        absWaspFilePath <- Path.parseAbsFile (ensurePathIsAbs absCwdPath waspFilePath)
        absOutDirPath <- Path.parseAbsDir (ensurePathIsAbs absCwdPath outDirPath)
        -- TODO(martin): Take compile options as arguments to the command, right now I hardcoded the value.
        let options = CompileOptions
                { externalCodeDirPath = (Path.parent absWaspFilePath) </> [reldir|src|]
                }
        result <- compile absWaspFilePath absOutDirPath options
        either putStrLn (\_ -> print ("Success!" :: String)) result
    _ -> print ("Usage: ./stic <wasp_file_path> <out_dir>" :: String)
  where
    -- | If path is not absolute, it is prefixed with given absolute directory.
    ensurePathIsAbs :: Path.AbsDir -> FilePath -> FilePath
    ensurePathIsAbs absDirPath path = if FilePath.isAbsolute path
                                      then path
                                      else (Path.toFilePath absDirPath) FilePath.</> path
