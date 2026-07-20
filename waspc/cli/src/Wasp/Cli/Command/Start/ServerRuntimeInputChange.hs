module Wasp.Cli.Command.Start.ServerRuntimeInputChange
  ( classifyServerEffect,
  )
where

import qualified StrongPath as SP
import qualified System.FilePath as FP
import Wasp.Cli.Command.Compile (CompileResult (..))
import Wasp.Cli.Command.Watch (ProjectFileChange (..), WatchCompileResult (..))
import Wasp.Generator.Common (GeneratedAppDir)
import Wasp.Generator.FileDraft.Writeable (FileOrDirPathRelativeTo)
import qualified Wasp.Generator.ServerGenerator.Common as ServerGenerator.Common
import Wasp.Generator.ServerGenerator.Start (ServerEffect (..))
import Wasp.Project.Common (srcDirInWaspProjectDir)
import Wasp.Util.Glob (compile, dirAndDescendantsGlobs, match, recursiveFileGlobsWithExtensions)

classifyServerEffect :: WatchCompileResult -> ServerEffect
classifyServerEffect watchCompileResult =
  foldMap projectFileChangeServerEffect (_watchProjectFileChanges watchCompileResult)
    <> foldMap generatedAppPathServerEffect (_compileChangedGeneratedAppPaths $ _watchCompileResult watchCompileResult)
  where
    projectFileChangeServerEffect (ProjectFileChange path)
      | projectServerInputPatterns `matchesAny` path = RebundleAndRestartServer
      | otherwise = NoServerEffect

    generatedAppPathServerEffect :: FileOrDirPathRelativeTo GeneratedAppDir -> ServerEffect
    generatedAppPathServerEffect (Left file)
      | file == generatedServerPackageFile = RebundleAndRestartServer
      | file == generatedServerEnvFile = RestartServer
      | generatedServerSrcPatterns `matchesAny` SP.fromRelFile file = RebundleAndRestartServer
      | otherwise = NoServerEffect
    generatedAppPathServerEffect (Right dir)
      | generatedServerSrcPatterns `matchesAny` FP.dropTrailingPathSeparator (SP.fromRelDir dir) = RebundleAndRestartServer
      | otherwise = NoServerEffect

    -- SDK changes ('sdk/wasp/...') are deliberately not covered by these globs,
    -- even though the server bundle includes the SDK. We assume every
    -- server-relevant SDK regeneration comes with a change to the generated
    -- server src or the user's src.
    projectServerInputPatterns =
      map compile $ recursiveFileGlobsWithExtensions projectSrcDir serverRuntimeInputFileExtensions

    generatedServerSrcPatterns = map compile $ dirAndDescendantsGlobs generatedServerSrcDir

    patterns `matchesAny` path = any (`match` path) patterns

    projectSrcDir = FP.dropTrailingPathSeparator $ SP.fromRelDir srcDirInWaspProjectDir
    generatedServerSrcDir = FP.dropTrailingPathSeparator $ SP.fromRelDir ServerGenerator.Common.serverSrcDirInGeneratedAppDir
    generatedServerEnvFile = ServerGenerator.Common.serverRootDirInGeneratedAppDir SP.</> [SP.relfile|.env|]
    generatedServerPackageFile = ServerGenerator.Common.serverRootDirInGeneratedAppDir SP.</> [SP.relfile|package.json|]
    serverRuntimeInputFileExtensions = [".ts", ".mts", ".js", ".mjs", ".json"]
