module Wasp.Generator.SdkGenerator.Client.VitePluginG (genVitePlugins) where

import Data.Aeson (object, (.=))
import Data.Maybe (fromJust)
import StrongPath (relfile, (</>))
import qualified StrongPath as SP
import qualified System.FilePath.Posix as FP.Posix
import Wasp.AppSpec (AppSpec)
import qualified Wasp.AppSpec as AS
import qualified Wasp.AppSpec.Route as AS.Route
import Wasp.Generator.Common (makeJsArrayFromHaskellList)
import Wasp.Generator.FileDraft (FileDraft)
import Wasp.Generator.Monad (Generator)
import Wasp.Generator.SdkGenerator.Client.VitePlugin.Common (clientEntryPointPath, spaFallbackFile, ssrEntryPointPath)
import Wasp.Generator.SdkGenerator.Client.VitePlugin.VirtualUserModulesPluginG (genVirtualUserModulesPlugin)
import Wasp.Generator.SdkGenerator.Client.VitePlugin.VirtualWaspModulesPluginG (genVirtualWaspModulesPlugin)
import qualified Wasp.Generator.SdkGenerator.Common as C
import Wasp.Generator.WebAppGenerator (viteBuildDirPath)
import qualified Wasp.Generator.WebAppGenerator.Common as WebApp
import Wasp.Project.Common
  ( dotWaspDirInWaspProjectDir,
    generatedAppDirInWaspProjectDir,
    srcDirInWaspProjectDir,
  )
import Wasp.Project.Env (dotEnvClient)
import Wasp.Util ((<++>))
import Wasp.Util.Js (makeJsStringLiteral)

genVitePlugins :: AppSpec -> Generator [FileDraft]
genVitePlugins spec =
  sequence
    [ genViteIndex,
      genWaspPlugin spec,
      genWaspConfigPlugin spec,
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
        [ "clientEntryPointPath" .= clientEntryPointPath,
          "srcTsConfigPath" .= SP.fromRelFile (AS.srcTsConfigPath spec),
          "ssrEntryPointPath" .= ssrEntryPointPath,
          "spaFallbackFile" .= SP.fromRelFileP spaFallbackFile,
          "ssrPaths" .= makeJsArrayFromHaskellList prerenderPaths
        ]
    prerenderPaths =
      concatMap AS.Route.prerender (AS.getRoutes spec)

genWaspConfigPlugin :: AppSpec -> Generator FileDraft
genWaspConfigPlugin spec = return $ C.mkTmplFdWithData tmplPath tmplData
  where
    tmplPath = C.vitePluginsDirInSdkTemplatesDir </> [relfile|waspConfig.ts|]
    tmplData =
      object
        [ "baseDir" .= makeJsStringLiteral (SP.fromAbsDirP (WebApp.getBaseDir spec)),
          "clientPortEnvVarName" .= WebApp.clientPortEnvVarName,
          "clientBuildDirPath" .= SP.fromRelDir viteBuildDirPath,
          "vitest"
            .= object
              [ "setupFilesArray" .= makeJsArrayFromHaskellList ["wasp/client/test/setup"],
                "excludeWaspArtefactsPattern" .= (SP.fromRelDirP (fromJust $ SP.relDirToPosix dotWaspDirInWaspProjectDir) FP.Posix.</> "**" FP.Posix.</> "*")
              ]
        ]

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
