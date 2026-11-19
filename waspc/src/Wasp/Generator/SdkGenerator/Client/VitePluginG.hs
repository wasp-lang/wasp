module Wasp.Generator.SdkGenerator.Client.VitePluginG (genVitePlugin) where

import Data.Aeson (object, (.=))
import qualified Data.Aeson.Encode.Pretty as AesonPretty
import Data.Maybe (fromJust)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import StrongPath (relfile, (</>))
import qualified StrongPath as SP
import Wasp.AppSpec (AppSpec)
import qualified Wasp.AppSpec as AS
import qualified Wasp.AppSpec.Route as AS.Route
import Wasp.Generator.FileDraft (FileDraft, createTextFileDraft)
import qualified Wasp.Generator.JsImport as GJI
import Wasp.Generator.Monad (Generator)
import Wasp.Generator.SdkGenerator.Client.VitePlugin.Common (clientEntryPointPath, routesEntryPointPath, spaFallbackFile, ssrEntryPointPath)
import Wasp.Generator.SdkGenerator.Client.VitePlugin.VirtualFilesG (genVirtualFiles)
import Wasp.Generator.SdkGenerator.Common (sdkPackageName)
import qualified Wasp.Generator.SdkGenerator.Common as C
import qualified Wasp.Generator.SdkGenerator.VirtualUserModules as VUM
import Wasp.Generator.WebAppGenerator (viteBuildDirPath)
import qualified Wasp.Generator.WebAppGenerator.Common as WebApp
import Wasp.Project.Common
  ( generatedAppDirInWaspProjectDir,
    srcDirInWaspProjectDir,
  )
import Wasp.Project.Env (dotEnvClient)
import Wasp.Util ((<++>))

-- | The Vite plugin itself is static code in the \@wasp/sdk-core package. Here
-- we generate what it reads from the app: its config and the virtual files.
genVitePlugin :: AppSpec -> Generator [FileDraft]
genVitePlugin spec = sequence [genGeneratedViteConfig spec] <++> genVirtualFiles spec

-- | Keep in sync with the @GeneratedViteConfig@ type in \@wasp/sdk-core.
genGeneratedViteConfig :: AppSpec -> Generator FileDraft
genGeneratedViteConfig spec =
  return $
    createTextFileDraft
      (C.sdkRootDirInGeneratedAppDir </> C.viteDirInSdkRootDir </> [relfile|config.json|])
      (TL.toStrict $ TLE.decodeUtf8 $ AesonPretty.encodePretty generatedViteConfig)
  where
    generatedViteConfig =
      object
        [ "baseDir" .= SP.fromAbsDirP (WebApp.getBaseDir spec),
          "clientBuildDir" .= SP.fromRelDir viteBuildDirPath,
          "clientPortEnvVarName" .= WebApp.clientPortEnvVarName,
          "clientEnvFileName" .= SP.fromRelFile dotEnvClient,
          "clientEnvSchemaValidationModulePath" .= clientEnvSchemaValidationModulePath,
          "srcDir" .= SP.fromRelDir srcDirInWaspProjectDir,
          "srcTsConfigPath" .= SP.fromRelFile (AS.srcTsConfigPath spec),
          "clientEntryPointPath" .= clientEntryPointPath,
          "routesEntryPointPath" .= routesEntryPointPath,
          "ssrEntryPointPath" .= ssrEntryPointPath,
          "ssrPaths" .= concatMap AS.Route.prerender (AS.getRoutes spec),
          "spaFallbackFile" .= SP.fromRelFileP spaFallbackFile,
          "depsExcludedFromOptimization" .= depsExcludedFromOptimization,
          "virtualUserModules"
            .= VUM.mkVirtualUserModulePathMap
              GJI.extImportToRelativeSrcImportFromViteExecution
              (VUM.getClientVirtualUserModules spec)
        ]

    clientEnvSchemaValidationModulePath =
      SP.fromRelFileP . fromJust . SP.relFileToPosix $
        generatedAppDirInWaspProjectDir </> C.sdkRootDirInGeneratedAppDir </> [relfile|client/env.ts|]

    depsExcludedFromOptimization =
      -- Why do we exclude Wasp SDK from optimization?
      -- - Wasp SDK is a dep that's regenerated over time and we don't want Vite to optimize it
      --   and cache it (which would break hot module reloading).
      -- - Accidentally, we don't need to do this because Wasp SDK is symlinked and Vite would
      --   exclude it anyways - but we are keeping it here because we want to be explicit.
      --   Read more: https://vite.dev/guide/dep-pre-bundling#monorepos-and-linked-dependencies
      [sdkPackageName]
