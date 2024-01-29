module Wasp.Project.Common
  ( findFileInWaspProjectDir,
    CompileError,
    CompileWarning,
    WaspProjectDir,
    packageJsonInWaspProjectDir,
  )
where

import StrongPath (Abs, Dir, File', Path', Rel, relfile, toFilePath, (</>))
import System.Directory (doesFileExist)

data WaspProjectDir -- Root dir of Wasp project, containing source files.

type CompileError = String

type CompileWarning = String

packageJsonInWaspProjectDir :: Path' (Rel WaspProjectDir) File'
packageJsonInWaspProjectDir = [relfile|package.json|]

findFileInWaspProjectDir ::
  Path' Abs (Dir WaspProjectDir) ->
  Path' (Rel WaspProjectDir) File' ->
  IO (Maybe (Path' Abs File'))
findFileInWaspProjectDir waspDir file = do
  let fileAbsFp = waspDir </> file
  fileExists <- doesFileExist $ toFilePath fileAbsFp
  return $ if fileExists then Just fileAbsFp else Nothing
