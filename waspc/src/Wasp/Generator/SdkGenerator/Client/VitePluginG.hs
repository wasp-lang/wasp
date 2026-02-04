module Wasp.Generator.SdkGenerator.Client.VitePluginG (genVitePlugins) where

import Data.Aeson (object, (.=))
import Data.Maybe (fromJust)
import StrongPath (relfile, (</>))
import qualified StrongPath as SP
import qualified System.FilePath.Posix as FP.Posix
import Wasp.AppSpec (AppSpec)
import Wasp.Generator.Common (makeJsArrayFromHaskellList)
import Wasp.Generator.FileDraft (FileDraft)
import Wasp.Generator.Monad (Generator)
import Wasp.Generator.SdkGenerator.Client.VitePlugin.HtmlPluginG (genHtmlPlugin)
import Wasp.Generator.SdkGenerator.Client.VitePlugin.VirtualModulesPluginG (getVirtualModulesPlugin)
import Wasp.Generator.SdkGenerator.Common (sdkPackageName)
import qualified Wasp.Generator.SdkGenerator.Common as C
import Wasp.Generator.WaspLibs.AvailableLibs (waspLibs)
import qualified Wasp.Generator.WaspLibs.WaspLib as WaspLib
import Wasp.Generator.WebAppGenerator (viteBuildDirInWebAppDir, webAppRootDirInProjectRootDir)
import qualified Wasp.Generator.WebAppGenerator.Common as WebApp
import Wasp.Project.Common
  ( dotWaspDirInWaspProjectDir,
    generatedCodeDirInDotWaspDir,
    srcDirInWaspProjectDir,
  )
import Wasp.Project.Env (dotEnvClient)
import Wasp.Util ((<++>))

genVitePlugins :: AppSpec -> Generator [FileDraft]
genVitePlugins spec =
  sequence
    [ genViteIndex,
      genFileCopy [relfile|wasp.ts|],
      genWaspConfigPlugin spec,
      genEnvFilePlugin,
      genDetectServerImportsPlugin,
      genFileCopy [relfile|validateEnv.ts|],
      genFileCopy [relfile|typescriptCheck.ts|]
    ]
    <++> getVirtualModulesPlugin spec
    <++> genHtmlPlugin spec
  where
    genFileCopy = return . C.mkTmplFd . (C.vitePluginsDirInSdkTemplatesDir </>)

genViteIndex :: Generator FileDraft
genViteIndex = return $ C.mkTmplFd tmplPath
  where
    tmplPath = C.viteDirInSdkTemplatesDir </> [relfile|index.ts|]

genWaspConfigPlugin :: AppSpec -> Generator FileDraft
genWaspConfigPlugin spec = return $ C.mkTmplFdWithData tmplPath tmplData
  where
    tmplPath = C.vitePluginsDirInSdkTemplatesDir </> [relfile|waspConfig.ts|]
    tmplData =
      object
        [ "baseDir" .= SP.fromAbsDirP (WebApp.getBaseDir spec),
          "defaultClientPort" .= WebApp.defaultClientPort,
          "clientBuildDirPath" .= SP.fromRelDir viteBuildDirPath,
          "depsExcludedFromOptimization" .= makeJsArrayFromHaskellList depsExcludedFromOptimization,
          "vitest"
            .= object
              [ "setupFilesArray" .= makeJsArrayFromHaskellList ["wasp/client/test/setup"],
                "excludeWaspArtefactsPattern" .= (SP.fromRelDirP (fromJust $ SP.relDirToPosix dotWaspDirInWaspProjectDir) FP.Posix.</> "**" FP.Posix.</> "*")
              ]
        ]
    viteBuildDirPath =
      dotWaspDirInWaspProjectDir
        </> generatedCodeDirInDotWaspDir
        </> webAppRootDirInProjectRootDir
        </> viteBuildDirInWebAppDir

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
