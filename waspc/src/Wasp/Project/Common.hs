module Wasp.Project.Common
  ( findFileInWaspProjectDir,
    extCodeDirInWaspProjectDir,
    extPublicDirInWaspProjectDir,
    CompileError,
    CompileWarning,
    WaspProjectDir,
  )
where

import StrongPath (Abs, Dir, File', Path', Rel, toFilePath, (</>))
import StrongPath.TH (reldir)
import System.Directory (doesFileExist)
import Wasp.AppSpec.ExternalFiles (SourceExternalCodeDir, SourceExternalPublicDir)

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

extCodeDirInWaspProjectDir :: Path' (Rel WaspProjectDir) (Dir SourceExternalCodeDir)
extCodeDirInWaspProjectDir = [reldir|src|]

extPublicDirInWaspProjectDir :: Path' (Rel WaspProjectDir) (Dir SourceExternalPublicDir)
extPublicDirInWaspProjectDir = [reldir|public|]
