module Wasp.Project.Common
  ( WaspProjectDir,
    DotWaspDir,
    NodeModulesDir,
    findFileInWaspProjectDir,
    dotWaspDirInWaspProjectDir,
    generatedCodeDirInDotWaspDir,
    buildDirInDotWaspDir,
    waspProjectDirFromProjectRootDir,
    dotWaspRootFileInWaspProjectDir,
    dotWaspInfoFileInGeneratedCodeDir,
    srcDirInWaspProjectDir,
    extServerCodeDirInWaspProjectDir,
    extClientCodeDirInWaspProjectDir,
    extSharedCodeDirInWaspProjectDir,
    packageJsonInWaspProjectDir,
    nodeModulesDirInWaspProjectDir,
    CompileError,
    CompileWarning,
    tsconfigInWaspProjectDir,
  )
where

import StrongPath (Abs, Dir, File', Path', Rel, reldir, relfile, toFilePath, (</>))
import System.Directory (doesFileExist)
import Wasp.AppSpec.ExternalCode (SourceExternalCodeDir)
import qualified Wasp.Generator.Common

data WaspProjectDir -- Root dir of Wasp project, containing source files.

data DotWaspDir -- Here we put everything that wasp generates.

data NodeModulesDir

-- | NOTE: If you change the depth of this path, also update @waspProjectDirFromProjectRootDir@ below.
-- TODO: SHould this be renamed to include word "root"?
dotWaspDirInWaspProjectDir :: Path' (Rel WaspProjectDir) (Dir DotWaspDir)
dotWaspDirInWaspProjectDir = [reldir|.wasp|]

-- | NOTE: If you change the depth of this path, also update @waspProjectDirFromProjectRootDir@ below.
-- TODO: Hm this has different name than it has in Generator.
generatedCodeDirInDotWaspDir :: Path' (Rel DotWaspDir) (Dir Wasp.Generator.Common.ProjectRootDir)
generatedCodeDirInDotWaspDir = [reldir|out|]

-- | NOTE: If you change the depth of this path, also update @waspProjectDirFromProjectRootDir@ below.
buildDirInDotWaspDir :: Path' (Rel DotWaspDir) (Dir Wasp.Generator.Common.ProjectRootDir)
buildDirInDotWaspDir = [reldir|build|]

-- | NOTE: This path is calculated from the values of @dotWaspDirInWaspProjectDir@,
-- @generatedCodeDirInDotWaspDir@ and @buildDirInDotWaspDir@., which are the three functions just above.
-- Also, it assumes @generatedCodeDirInDotWaspDir@ and @buildDirInDotWaspDir@ have same depth.
-- If any of those change significantly (their depth), this path should be adjusted.
waspProjectDirFromProjectRootDir :: Path' (Rel Wasp.Generator.Common.ProjectRootDir) (Dir WaspProjectDir)
waspProjectDirFromProjectRootDir = [reldir|../../|]

dotWaspRootFileInWaspProjectDir :: Path' (Rel WaspProjectDir) File'
dotWaspRootFileInWaspProjectDir = [relfile|.wasproot|]

dotWaspInfoFileInGeneratedCodeDir :: Path' (Rel Wasp.Generator.Common.ProjectRootDir) File'
dotWaspInfoFileInGeneratedCodeDir = [relfile|.waspinfo|]

srcDirInWaspProjectDir :: Path' (Rel WaspProjectDir) (Dir SourceExternalCodeDir)
srcDirInWaspProjectDir = [reldir|src|]

extServerCodeDirInWaspProjectDir :: Path' (Rel WaspProjectDir) (Dir SourceExternalCodeDir)
extServerCodeDirInWaspProjectDir = srcDirInWaspProjectDir

extClientCodeDirInWaspProjectDir :: Path' (Rel WaspProjectDir) (Dir SourceExternalCodeDir)
extClientCodeDirInWaspProjectDir = srcDirInWaspProjectDir

extSharedCodeDirInWaspProjectDir :: Path' (Rel WaspProjectDir) (Dir SourceExternalCodeDir)
extSharedCodeDirInWaspProjectDir = srcDirInWaspProjectDir

packageJsonInWaspProjectDir :: Path' (Rel WaspProjectDir) File'
packageJsonInWaspProjectDir = [relfile|package.json|]

tsconfigInWaspProjectDir :: Path' (Rel WaspProjectDir) File'
tsconfigInWaspProjectDir = [relfile|tsconfig.json|]

nodeModulesDirInWaspProjectDir :: Path' (Rel WaspProjectDir) (Dir NodeModulesDir)
nodeModulesDirInWaspProjectDir = [reldir|node_modules|]

findFileInWaspProjectDir ::
  Path' Abs (Dir WaspProjectDir) ->
  Path' (Rel WaspProjectDir) File' ->
  IO (Maybe (Path' Abs File'))
findFileInWaspProjectDir waspDir file = do
  let fileAbsFp = waspDir </> file
  fileExists <- doesFileExist $ toFilePath fileAbsFp
  return $ if fileExists then Just fileAbsFp else Nothing

type CompileError = String

type CompileWarning = String
