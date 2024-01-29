module Wasp.Project.Common
  ( findFileInWaspProjectDir,
    CompileError,
    CompileWarning,
    WaspProjectDir,
  )
where

import StrongPath (Abs, Dir, File', Path', Rel, toFilePath, (</>))
import System.Directory (doesFileExist)

data WaspProjectDir -- Root dir of Wasp project, containing source files.

type CompileError = String

type CompileWarning = String

findFileInWaspProjectDir ::
  Path' Abs (Dir WaspProjectDir) ->
  Path' (Rel WaspProjectDir) File' ->
  IO (Maybe (Path' Abs File'))
findFileInWaspProjectDir waspDir file = do
  let fileAbsFp = waspDir </> file
  fileExists <- doesFileExist $ toFilePath fileAbsFp
  return $ if fileExists then Just fileAbsFp else Nothing
