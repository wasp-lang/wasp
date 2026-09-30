module Wasp.Cli.Command.Start.ServerRuntimeInputChange
  ( classifyServerEffect,
  )
where

import qualified StrongPath as SP
import qualified System.FilePath as FP
import Wasp.Cli.Command.Watch (ProjectFileChange (..))
import Wasp.Generator.Common (GeneratedAppDir)
import Wasp.Generator.FileDraft.Writeable (FileOrDirPathRelativeTo)
import qualified Wasp.Generator.ServerGenerator.Common as ServerGenerator.Common
import Wasp.Generator.ServerGenerator.Start (ServerEffect (..))
import Wasp.Project.Common (srcDirInWaspProjectDir)
import Wasp.Util.Glob (compile, dirAndDescendantsGlobs, match, recursiveFileGlobsWithExtensions)

classifyServerEffect :: [ProjectFileChange] -> [FileOrDirPathRelativeTo GeneratedAppDir] -> ServerEffect
classifyServerEffect projectFileChanges changedGeneratedAppPaths =
  foldMap projectFileChangeServerEffect projectFileChanges
    <> foldMap generatedAppPathServerEffect changedGeneratedAppPaths
  where
    projectFileChangeServerEffect (ProjectFileChange path)
      | projectServerInputPatterns `matchesAny` path = RebundleAndRestartServer
      | otherwise = NoServerEffect

    generatedAppPathServerEffect :: FileOrDirPathRelativeTo GeneratedAppDir -> ServerEffect
    generatedAppPathServerEffect (Left file)
      | file == generatedServerPackageFile = RebundleAndRestartServer
      | file == generatedServerEnvFile = RestartServer
      | generatedServerInputPatterns `matchesAny` SP.fromRelFile file = RebundleAndRestartServer
      | otherwise = NoServerEffect
    generatedAppPathServerEffect (Right dir)
      | generatedServerInputPatterns `matchesAny` FP.dropTrailingPathSeparator (SP.fromRelDir dir) = RebundleAndRestartServer
      | otherwise = NoServerEffect

    projectServerInputPatterns =
      map compile $ recursiveFileGlobsWithExtensions projectSrcDir serverRuntimeInputFileExtensions

    generatedServerInputPatterns =
      map compile $
        dirAndDescendantsGlobs generatedServerSrcDir
          -- The server bundles SDK code, which can change independently.
          ++ dirAndDescendantsGlobs "sdk/wasp/server"

    patterns `matchesAny` path = any (`match` path) patterns

    projectSrcDir = FP.dropTrailingPathSeparator $ SP.fromRelDir srcDirInWaspProjectDir
    generatedServerSrcDir = FP.dropTrailingPathSeparator $ SP.fromRelDir ServerGenerator.Common.serverSrcDirInGeneratedAppDir
    generatedServerEnvFile = ServerGenerator.Common.serverRootDirInGeneratedAppDir SP.</> [SP.relfile|.env|]
    generatedServerPackageFile = ServerGenerator.Common.serverRootDirInGeneratedAppDir SP.</> [SP.relfile|package.json|]
    serverRuntimeInputFileExtensions = [".ts", ".mts", ".js", ".mjs", ".json"]
