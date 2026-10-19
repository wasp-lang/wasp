module Wasp.Generator.ServerGenerator.ServerEffect
  ( ServerEffect (..),
    classifyServerEffect,
  )
where

import StrongPath (File', Path', Rel, reldir, relfile, (</>))
import qualified StrongPath as SP
import System.FilePath.Glob (Pattern, match)
import Wasp.Generator.Common (GeneratedAppDir)
import Wasp.Generator.DbGenerator.Common (dbSchemaFileInGeneratedAppDir)
import Wasp.Generator.FileDraft.Writeable (FileOrDirPathRelativeTo)
import Wasp.Generator.SdkGenerator.Common (sdkRootDirInGeneratedAppDir)
import Wasp.Generator.ServerGenerator.Common (serverRootDirInGeneratedAppDir, serverSrcDirInGeneratedAppDir)
import Wasp.Project.Common (WaspProjectDir, srcDirInWaspProjectDir)
import Wasp.Util.Glob (descendantsGlob, dirGlob)

-- | Effect of a successful compile on a healthy, running server. Without one,
-- the controller conservatively rebundles before starting.
--
-- Each successive effect "contains" the previous one in terms of impact on the
-- server. We take advantage of that and derive 'Ord' so that we can easily
-- determine the "maximum" effect among a set of changes, and 'Semigroup' so
-- that we can combine them easily.
data ServerEffect
  = NoChange
  | RestartServer
  | RebundleAndRestartServer
  deriving (Eq, Ord, Show)

-- | The effect that a compile's changes have on the server: the project files
-- that changed, and the generated app paths that the compile changed.
classifyServerEffect :: [Path' (Rel WaspProjectDir) File'] -> [FileOrDirPathRelativeTo GeneratedAppDir] -> ServerEffect
classifyServerEffect changedProjectFiles changedGeneratedAppPaths =
  maximum
    -- `maximum` needs at least one element in the list, so we add the default effect
    $ (NoChange :)
    $ (changedProjectFileServerEffect <$> changedProjectFiles)
      <> (generatedAppPathServerEffect <$> changedGeneratedAppPaths)
  where
    changedProjectFileServerEffect :: Path' (Rel WaspProjectDir) File' -> ServerEffect
    changedProjectFileServerEffect file
      | projectServerInputPatterns `matchesAny` file = RebundleAndRestartServer
      | otherwise = NoChange

    generatedAppPathServerEffect :: FileOrDirPathRelativeTo GeneratedAppDir -> ServerEffect
    generatedAppPathServerEffect (Left file)
      | file == generatedServerPackageFile = RebundleAndRestartServer
      | file == generatedServerEnvFile = RestartServer
      -- The server loads the Prisma client, which is regenerated from the
      -- schema, at runtime.
      | file == SP.castFile dbSchemaFileInGeneratedAppDir = RestartServer
      | isGeneratedServerInput file = RebundleAndRestartServer
      | otherwise = NoChange
    generatedAppPathServerEffect (Right dir)
      | isGeneratedServerInput dir = RebundleAndRestartServer
      | otherwise = NoChange

    isGeneratedServerInput path =
      generatedServerInputPatterns `matchesAny` path
        && not (generatedSdkClientPatterns `matchesAny` path)

    projectServerInputPatterns =
      (descendantsGlob srcDirInWaspProjectDir <>) <$> serverRuntimeInputFileExtensions

    generatedServerInputPatterns =
      [dirGlob, descendantsGlob]
        <*> [ SP.castDir serverSrcDirInGeneratedAppDir,
              -- The server bundles SDK code, which can change independently. All of
              -- the SDK except its client code can end up in the server's bundle.
              SP.castDir sdkRootDirInGeneratedAppDir
            ]

    generatedSdkClientPatterns =
      [dirGlob, descendantsGlob]
        <*> [generatedSdkClientFolder]

    generatedSdkClientFolder = sdkRootDirInGeneratedAppDir </> [reldir|client|]
    generatedServerEnvFile = serverRootDirInGeneratedAppDir </> [relfile|.env|]
    generatedServerPackageFile = serverRootDirInGeneratedAppDir </> [relfile|package.json|]
    serverRuntimeInputFileExtensions = [".ts", ".mts", ".js", ".mjs", ".json"]

    matchesAny :: [Pattern] -> Path' b t -> Bool
    patterns `matchesAny` path = any (`match` SP.toFilePath path) patterns
