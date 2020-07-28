module Main where

import System.Environment
import System.Directory
import qualified System.FilePath as FilePath
import qualified Path as P

import CompileOptions (CompileOptions (..))
import StrongPath (Path, Abs, Dir)
import qualified StrongPath as SP
import Lib (compile)


main :: IO ()
main = do
  absCwdPath <- getCurrentDirectory >>= SP.parseAbsDir
  args <- getArgs
  case args of
    [waspFilePath, outDirPath] -> do
        absWaspFilePath <- SP.parseAbsFile (ensurePathIsAbs absCwdPath waspFilePath)
        absOutDirPath <- SP.parseAbsDir (ensurePathIsAbs absCwdPath outDirPath)
        -- TODO(martin): Take compile options as arguments to the command, right now I hardcoded the value.
        let options = CompileOptions
                { externalCodeDirPath = (SP.parent absWaspFilePath) SP.</> SP.fromPathRelDir [P.reldir|ext|]
                }
        result <- compile absWaspFilePath absOutDirPath options
        either putStrLn (\_ -> print ("Success!" :: String)) result
    _ -> print ("Usage: ./waspc <wasp_file_path> <out_dir>" :: String)
  where
    -- | If path is not absolute, it is prefixed with given absolute directory.
    ensurePathIsAbs :: Path Abs (Dir a) -> FilePath -> FilePath
    ensurePathIsAbs absDirPath path = if FilePath.isAbsolute path
                                      then path
                                      else (SP.toFilePath absDirPath) FilePath.</> path
