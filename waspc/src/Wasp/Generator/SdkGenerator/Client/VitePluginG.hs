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
import qualified Wasp.Generator.SdkGenerator.Common as C
import Wasp.Project.Common
  ( dotWaspDirInWaspProjectDir,
    srcDirInWaspProjectDir,
  )
import qualified Wasp.Project.WebApp.Common as WebApp
import Wasp.Util ((<++>))

genVitePlugins :: AppSpec -> Generator [FileDraft]
genVitePlugins spec =
  sequence
    [ genViteIndex,
      genWaspPlugin spec,
      genDetectServerImportsPlugin,
      genValidateEnvPlugin
    ]
    <++> getVirtualModulesPlugin spec
    <++> genHtmlPlugin spec

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
        [ "baseDir" .= SP.fromAbsDirP (WebApp.getBaseDir spec),
          "defaultClientPort" .= WebApp.defaultClientPort,
          "vitest"
            .= object
              [ "setupFilesArray" .= makeJsArrayFromHaskellList ["wasp/client/test/setup"],
                "excludeWaspArtefactsPattern" .= (SP.fromRelDirP (fromJust $ SP.relDirToPosix dotWaspDirInWaspProjectDir) FP.Posix.</> "**" FP.Posix.</> "*")
              ]
        ]

genDetectServerImportsPlugin :: Generator FileDraft
genDetectServerImportsPlugin = return $ C.mkTmplFdWithData tmplPath tmplData
  where
    tmplPath = C.vitePluginsDirInSdkTemplatesDir </> [relfile|detectServerImports.ts|]
    tmplData =
      object
        [ "srcDirInWaspProjectDir" .= SP.fromRelDir srcDirInWaspProjectDir
        ]

genValidateEnvPlugin :: Generator FileDraft
genValidateEnvPlugin = return $ C.mkTmplFd tmplPath
  where
    tmplPath = C.vitePluginsDirInSdkTemplatesDir </> [relfile|validateEnv.ts|]
