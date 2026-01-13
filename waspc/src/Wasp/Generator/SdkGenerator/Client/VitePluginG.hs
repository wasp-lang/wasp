module Wasp.Generator.SdkGenerator.Client.VitePluginG (genVitePlugins) where

import Data.Aeson (object, (.=))
import Data.Maybe (fromJust)
import StrongPath (Dir, Path', Rel, reldir, relfile, (</>))
import qualified StrongPath as SP
import qualified System.FilePath.Posix as FP.Posix
import Wasp.AppSpec (AppSpec)
import Wasp.Generator.Common (makeJsArrayFromHaskellList)
import Wasp.Generator.FileDraft (FileDraft)
import Wasp.Generator.Monad (Generator)
import Wasp.Generator.SdkGenerator.Common (SdkTemplatesDir)
import qualified Wasp.Generator.SdkGenerator.Common as C
import qualified Wasp.Generator.WebAppGenerator.Common as WebApp
import Wasp.Project.Common
  ( dotWaspDirInWaspProjectDir,
    generatedCodeDirInDotWaspDir,
    srcDirInWaspProjectDir,
  )

data VitePluginsDir

genVitePlugins :: AppSpec -> Generator [FileDraft]
genVitePlugins spec =
  sequence
    [ genViteIndex,
      genWaspPlugin spec,
      genDetectServerImportsPlugin,
      genValidateEnvPlugin
    ]

vitePluginsDirInSdkTemplatesDir :: Path' (Rel SdkTemplatesDir) (Dir VitePluginsDir)
vitePluginsDirInSdkTemplatesDir = [reldir|client/vite|]

genViteIndex :: Generator FileDraft
genViteIndex = return $ C.mkTmplFd tmplPath
  where
    tmplPath = vitePluginsDirInSdkTemplatesDir </> [relfile|index.ts|]

genWaspPlugin :: AppSpec -> Generator FileDraft
genWaspPlugin spec = return $ C.mkTmplFdWithData tmplPath tmplData
  where
    tmplPath = vitePluginsDirInSdkTemplatesDir </> [relfile|plugins/wasp.ts|]
    tmplData =
      object
        [ "baseDir" .= SP.fromAbsDirP (WebApp.getBaseDir spec),
          "defaultClientPort" .= WebApp.defaultClientPort,
          "vitest"
            .= object
              [ "setupFilesArray" .= makeJsArrayFromHaskellList vitestSetupFiles,
                "excludeWaspArtefactsPattern" .= (SP.fromRelDirP (fromJust $ SP.relDirToPosix dotWaspDirInWaspProjectDir) FP.Posix.</> "**" FP.Posix.</> "*")
              ]
        ]
    vitestSetupFiles =
      [ SP.fromRelFile $
          dotWaspDirInWaspProjectDir
            </> generatedCodeDirInDotWaspDir
            </> WebApp.webAppRootDirInProjectRootDir
            </> WebApp.webAppSrcDirInWebAppRootDir
            </> [relfile|test/vitest/setup.ts|]
      ]

genDetectServerImportsPlugin :: Generator FileDraft
genDetectServerImportsPlugin = return $ C.mkTmplFdWithData tmplPath tmplData
  where
    tmplPath = vitePluginsDirInSdkTemplatesDir </> [relfile|plugins/detectServerImports.ts|]
    tmplData =
      object
        [ "srcDirInWaspProjectDir" .= SP.fromRelDir srcDirInWaspProjectDir
        ]

genValidateEnvPlugin :: Generator FileDraft
genValidateEnvPlugin = return $ C.mkTmplFd tmplPath
  where
    tmplPath = vitePluginsDirInSdkTemplatesDir </> [relfile|plugins/validateEnv.ts|]
