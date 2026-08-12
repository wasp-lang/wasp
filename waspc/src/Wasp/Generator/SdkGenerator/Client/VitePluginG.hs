module Wasp.Generator.SdkGenerator.Client.VitePluginG (genVitePlugins) where

import Data.Aeson (object, (.=))
import Data.Maybe (fromJust)
import StrongPath (relfile, (</>))
import qualified StrongPath as SP
import qualified System.FilePath.Posix as FP.Posix
import Wasp.AppSpec (AppSpec)
import qualified Wasp.AppSpec as AS
import Wasp.Generator.Common (makeJsArrayFromHaskellList)
import Wasp.Generator.FileDraft (FileDraft)
import Wasp.Generator.Monad (Generator)
import Wasp.Generator.SdkGenerator.Client.VitePlugin.Common (clientEntryPointPath, getPrerenderPaths, spaFallbackFile, ssrEntryPointPath)
import Wasp.Generator.SdkGenerator.Client.VitePlugin.VirtualUserModulesPluginG (genVirtualUserModulesPlugin)
import Wasp.Generator.SdkGenerator.Client.VitePlugin.VirtualWaspModulesPluginG (genVirtualWaspModulesPlugin)
import Wasp.Generator.SdkGenerator.Common (sdkPackageName)
import qualified Wasp.Generator.SdkGenerator.Common as C
import Wasp.Generator.WaspLibs.AvailableLibs (waspLibs)
import qualified Wasp.Generator.WaspLibs.WaspLib as WaspLib
import Wasp.Generator.WebAppGenerator (viteBuildDirPath, webAppRootDirPath)
import qualified Wasp.Generator.WebAppGenerator.Common as WebApp
import Wasp.Project.Common
  ( dotWaspDirInWaspProjectDir,
    generatedAppDirInWaspProjectDir,
    srcDirInWaspProjectDir,
  )
import Wasp.Project.Env (dotEnvClient)
import Wasp.Util ((<++>))

genVitePlugins :: AppSpec -> Generator [FileDraft]
genVitePlugins spec =
  sequence
    [ genViteIndex,
      genWaspPlugin spec,
      genWaspConfigPlugin spec,
      genNitroBridgePlugin spec,
      genNitroRenderer,
      genEnvFilePlugin,
      genDetectServerImportsPlugin,
      genValidateEnvPlugin,
      genFileCopy [relfile|typescriptCheck.ts|],
      genVirtualUserModulesPlugin spec
    ]
    <++> genVirtualWaspModulesPlugin spec
  where
    genFileCopy = return . C.mkTmplFd . (C.vitePluginsDirInSdkTemplatesDir </>)

genViteIndex :: Generator FileDraft
genViteIndex = return $ C.mkTmplFd tmplPath
  where
    tmplPath = C.viteDirInSdkTemplatesDir </> [relfile|index.ts|]

genWaspPlugin :: AppSpec -> Generator FileDraft
genWaspPlugin spec = return $ C.mkTmplFdWithData tmplPath tmplData
  where
    tmplPath = C.vitePluginsDirInSdkTemplatesDir </> [relfile|wasp.ts|]
    tmplData =
      object
        [ "srcTsConfigPath" .= SP.fromRelFile (AS.srcTsConfigPath spec)
        ]

genNitroBridgePlugin :: AppSpec -> Generator FileDraft
genNitroBridgePlugin spec = return $ C.mkTmplFdWithData tmplPath tmplData
  where
    tmplPath = C.vitePluginsDirInSdkTemplatesDir </> [relfile|nitroBridge.ts|]
    tmplData =
      object
        [ "clientEntryPointPath" .= clientEntryPointPath,
          "ssrEntryPointPath" .= ssrEntryPointPath,
          "baseDir" .= SP.fromAbsDirP (WebApp.getBaseDir spec),
          "nitroOutputDirPath" .= SP.fromRelDir webAppRootDirPath,
          "clientBuildDirPath" .= SP.fromRelDir viteBuildDirPath,
          "spaFallbackFilePath" .= ("/" ++ SP.fromRelFileP spaFallbackFile),
          "prerenderPaths" .= makeJsArrayFromHaskellList (getPrerenderPaths spec)
        ]

-- | The Nitro renderer entry point. Unlike the rest of the SSR code, it is a
-- plain file in the SDK (not a Vite virtual file), because Nitro builds it
-- outside of Vite when prerendering. See the template for details.
genNitroRenderer :: Generator FileDraft
genNitroRenderer = return $ C.mkTmplFd tmplPath
  where
    tmplPath = C.viteDirInSdkTemplatesDir </> [relfile|nitroRenderer.ts|]

genWaspConfigPlugin :: AppSpec -> Generator FileDraft
genWaspConfigPlugin spec = return $ C.mkTmplFdWithData tmplPath tmplData
  where
    tmplPath = C.vitePluginsDirInSdkTemplatesDir </> [relfile|waspConfig.ts|]
    tmplData =
      object
        [ "baseDir" .= SP.fromAbsDirP (WebApp.getBaseDir spec),
          "defaultClientPort" .= WebApp.defaultClientPort,
          "depsExcludedFromOptimization" .= makeJsArrayFromHaskellList depsExcludedFromOptimization,
          "vitest"
            .= object
              [ "setupFilesArray" .= makeJsArrayFromHaskellList ["wasp/client/test/setup"],
                "excludeWaspArtefactsPattern" .= (SP.fromRelDirP (fromJust $ SP.relDirToPosix dotWaspDirInWaspProjectDir) FP.Posix.</> "**" FP.Posix.</> "*")
              ]
        ]

    depsExcludedFromOptimization =
      -- Why do we exclude Wasp SDK from optimization?
      -- - Wasp SDK is a dep that's regenerated over time and we don't want Vite to optimize it
      --   and cache it (which would break hot module reloading).
      -- - Accidentally, we don't need to do this because Wasp SDK is symlinked and Vite would
      --   exclude it anyways - but we are keeping it here because we want to be explicit.
      --   Read more: https://vite.dev/guide/dep-pre-bundling#monorepos-and-linked-dependencies
      sdkPackageName
        :
        -- Wasp libs are excluded from optimization because they are internal npm packages that
        -- have a static version during Wasp development which means once they are cached by Vite,
        -- they aren't updated even though the lib changes.
        -- Read more about libs versioning in `waspc/libs/README.md`.
        map WaspLib.packageName waspLibs

genEnvFilePlugin :: Generator FileDraft
genEnvFilePlugin = return $ C.mkTmplFdWithData tmplPath tmplData
  where
    tmplPath = C.vitePluginsDirInSdkTemplatesDir </> [relfile|envFile.ts|]
    tmplData = object ["clientEnvFileName" .= SP.fromRelFile dotEnvClient]

genDetectServerImportsPlugin :: Generator FileDraft
genDetectServerImportsPlugin = return $ C.mkTmplFdWithData tmplPath tmplData
  where
    tmplPath = C.vitePluginsDirInSdkTemplatesDir </> [relfile|detectServerImports.ts|]
    tmplData = object ["srcDirInWaspProjectDir" .= SP.fromRelDir srcDirInWaspProjectDir]

genValidateEnvPlugin :: Generator FileDraft
genValidateEnvPlugin = return $ C.mkTmplFdWithData tmplPath tmplData
  where
    tmplPath = C.vitePluginsDirInSdkTemplatesDir </> [relfile|validateEnv.ts|]
    tmplData = object ["clientEnvSchemaValidationModulePath" .= clientEnvSchemaValidationModulePath]

    clientEnvSchemaValidationModulePath = SP.fromRelFileP . fromJust . SP.relFileToPosix $ clientEnvSchemaValidationModuleDir
    clientEnvSchemaValidationModuleDir = generatedAppDirInWaspProjectDir </> C.sdkRootDirInGeneratedAppDir </> [relfile|client/env.ts|]
