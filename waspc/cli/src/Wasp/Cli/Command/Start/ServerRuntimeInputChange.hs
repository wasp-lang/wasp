module Wasp.Cli.Command.Start.ServerRuntimeInputChange
  ( classifyServerEffect,
  )
where

import qualified StrongPath as SP
import qualified System.FilePath as FP
import Wasp.Cli.Command.Watch (ProjectFileChange (..))
import Wasp.Generator.Common (GeneratedAppDir)
import Wasp.Generator.DbGenerator.Common (dbSchemaFileInGeneratedAppDir)
import Wasp.Generator.FileDraft.Writeable (FileOrDirPathRelativeTo)
import Wasp.Generator.SdkGenerator.Common (sdkRootDirInGeneratedAppDir)
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
      -- The server loads the Prisma client, which is regenerated from the
      -- schema, at runtime.
      | file == SP.castFile dbSchemaFileInGeneratedAppDir = RestartServer
      | isGeneratedServerInput $ SP.fromRelFile file = RebundleAndRestartServer
      | otherwise = NoServerEffect
    generatedAppPathServerEffect (Right dir)
      | isGeneratedServerInput $ FP.dropTrailingPathSeparator $ SP.fromRelDir dir = RebundleAndRestartServer
      | otherwise = NoServerEffect

    isGeneratedServerInput path =
      generatedServerInputPatterns `matchesAny` path
        && not (generatedSdkClientPatterns `matchesAny` path)

    projectServerInputPatterns =
      map compile $ recursiveFileGlobsWithExtensions projectSrcDir serverRuntimeInputFileExtensions

    generatedServerInputPatterns =
      map compile $
        dirAndDescendantsGlobs generatedServerSrcDir
          -- The server bundles SDK code, which can change independently. All of
          -- the SDK except its client code can end up in the server's bundle.
          ++ dirAndDescendantsGlobs generatedSdkDir

    generatedSdkClientPatterns = map compile $ dirAndDescendantsGlobs $ generatedSdkDir FP.</> "client"

    patterns `matchesAny` path = any (`match` path) patterns

    projectSrcDir = FP.dropTrailingPathSeparator $ SP.fromRelDir srcDirInWaspProjectDir
    generatedSdkDir = FP.dropTrailingPathSeparator $ SP.fromRelDir sdkRootDirInGeneratedAppDir
    generatedServerSrcDir = FP.dropTrailingPathSeparator $ SP.fromRelDir ServerGenerator.Common.serverSrcDirInGeneratedAppDir
    generatedServerEnvFile = ServerGenerator.Common.serverRootDirInGeneratedAppDir SP.</> [SP.relfile|.env|]
    generatedServerPackageFile = ServerGenerator.Common.serverRootDirInGeneratedAppDir SP.</> [SP.relfile|package.json|]
    serverRuntimeInputFileExtensions = [".ts", ".mts", ".js", ".mjs", ".json"]
