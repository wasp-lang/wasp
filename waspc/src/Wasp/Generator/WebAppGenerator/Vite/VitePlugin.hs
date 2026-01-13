module Wasp.Generator.WebAppGenerator.Vite.VitePlugin
  ( genVitePlugins,
    vitePluginsGlob,
  )
where

import Data.Aeson (object, (.=))
import Data.Maybe (fromJust)
import qualified FilePath.Extra as FP.Extra
import StrongPath (Dir, Path', Rel, reldir, relfile, (</>))
import qualified StrongPath as SP
import qualified System.FilePath.Posix as FP.Posix
import Wasp.AppSpec (AppSpec)
import Wasp.Generator.Common (WebAppRootDir, makeJsArrayFromHaskellList)
import Wasp.Generator.FileDraft (FileDraft)
import Wasp.Generator.Monad (Generator)
import qualified Wasp.Generator.WebAppGenerator.Common as C
import Wasp.Project.Common
  ( WaspProjectDir,
    dotWaspDirInWaspProjectDir,
    generatedCodeDirInDotWaspDir,
    srcDirInWaspProjectDir,
    waspProjectDirFromAppComponentDir,
  )

data WebAppVitePluginsDir

genVitePlugins :: AppSpec -> Generator [FileDraft]
genVitePlugins spec =
  sequence
    [ genWaspPlugin spec,
      genDetectServerImportsPlugin,
      genValidateEnvPlugin
    ]

vitePluginsGlob :: String
vitePluginsGlob = SP.fromRelDir vitePluginsDirInWebAppDir <> "*"

vitePluginsDirInWebAppDir :: Path' (Rel WebAppRootDir) (Dir WebAppVitePluginsDir)
vitePluginsDirInWebAppDir = [reldir|vite|]

genWaspPlugin :: AppSpec -> Generator FileDraft
genWaspPlugin spec = return $ C.mkTmplFdWithData tmplPath tmplData
  where
    tmplPath = C.asTmplFile $ vitePluginsDirInWebAppDir </> [relfile|wasp.ts|]
    tmplData =
      object
        [ "baseDir" .= SP.fromAbsDirP (C.getBaseDir spec),
          "projectDir" .= SP.fromRelDirP relPathFromWebAppRootDirToWaspProjectDir,
          "defaultClientPort" .= C.defaultClientPort,
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
            </> C.webAppRootDirInProjectRootDir
            </> C.webAppSrcDirInWebAppRootDir
            </> [relfile|test/vitest/setup.ts|]
      ]
    relPathFromWebAppRootDirToWaspProjectDir =
      fromJust $
        SP.parseRelDirP $
          FP.Extra.reversePosixPath $
            SP.fromRelDir (dotWaspDirInWaspProjectDir </> generatedCodeDirInDotWaspDir </> C.webAppRootDirInProjectRootDir)

genDetectServerImportsPlugin :: Generator FileDraft
genDetectServerImportsPlugin = return $ C.mkTmplFdWithData tmplPath tmplData
  where
    tmplPath = C.asTmplFile $ vitePluginsDirInWebAppDir </> [relfile|plugins/detectServerImports.ts|]
    tmplData =
      object
        [ "waspProjectDirFromWebAppDir" .= SP.fromRelDir waspProjectDirFromWebAppDir,
          "srcDirInWaspProjectDir" .= SP.fromRelDir srcDirInWaspProjectDir
        ]

    waspProjectDirFromWebAppDir = waspProjectDirFromAppComponentDir :: Path' (Rel WebAppRootDir) (Dir WaspProjectDir)

genValidateEnvPlugin :: Generator FileDraft
genValidateEnvPlugin = return $ C.mkTmplFd tmplPath
  where
    tmplPath = C.asTmplFile $ vitePluginsDirInWebAppDir </> [relfile|plugins/validateEnv.ts|]
