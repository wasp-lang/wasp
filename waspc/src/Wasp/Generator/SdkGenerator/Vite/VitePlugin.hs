module Wasp.Generator.SdkGenerator.Vite.VitePlugin
  ( genVitePlugins,
    vitePlugins,
  )
where

import Data.Aeson (object, (.=))
import Data.Maybe (fromJust)
import qualified FilePath.Extra as FP.Extra
import StrongPath (Dir, File', Path, Path', Posix, Rel, reldir, relfile, (</>))
import qualified StrongPath as SP
import qualified System.FilePath.Posix as FP.Posix
import Wasp.AppSpec (AppSpec)
import Wasp.Generator.Common (WebAppRootDir)
import Wasp.Generator.FileDraft (FileDraft)
import Wasp.Generator.Monad (Generator)
import qualified Wasp.Generator.SdkGenerator.Common as SDK
import Wasp.Generator.WebAppGenerator.Common (webAppRootDirInProjectRootDir)
import qualified Wasp.Generator.WebAppGenerator.Common as C
import Wasp.Project.Common (WaspProjectDir, dotWaspDirInWaspProjectDir, generatedCodeDirInDotWaspDir, srcDirInWaspProjectDir, waspProjectDirFromAppComponentDir)

data VitePluginName = DetectServerImports | ValidateEnv | Wasp | ViteIndex
  deriving (Enum, Bounded)

type TmplFilePath = Path' (Rel SDK.SdkTemplatesDir) File'

-- We define it like this because we need a list of plugin
-- paths which we will use in the tsconfig.vite.json "include" section
type VitePlugin = (VitePluginName, TmplFilePath)

genVitePlugins :: AppSpec -> Generator [FileDraft]
genVitePlugins spec = mapM (genVitePlugin spec) vitePlugins

vitePlugins :: [VitePlugin]
vitePlugins =
  map
    (\name -> (name, getTmplFilePathForVitePlugin name))
    vitePluginNames
  where
    vitePluginNames = [minBound .. maxBound]

getTmplFilePathForVitePlugin :: VitePluginName -> TmplFilePath
getTmplFilePathForVitePlugin pluginName = SDK.asTmplFile $ [reldir|client/vite|] </> pluginFilePathInPluginsDir pluginName
  where
    pluginFilePathInPluginsDir DetectServerImports = [relfile|detectServerImports.ts|]
    pluginFilePathInPluginsDir ValidateEnv = [relfile|validateEnv.ts|]
    pluginFilePathInPluginsDir Wasp = [relfile|wasp.ts|]
    pluginFilePathInPluginsDir ViteIndex = [relfile|index.ts|]

genVitePlugin :: AppSpec -> VitePlugin -> Generator FileDraft
genVitePlugin _ (DetectServerImports, tmplFile) = genDetectServerImportsPlugin tmplFile
genVitePlugin _ (ValidateEnv, tmplFile) = genValidateEnvPlugin tmplFile
genVitePlugin spec (Wasp, tmplFile) = genWaspPlugin spec tmplFile
genVitePlugin _ (ViteIndex, tmplFile) = genViteIndexPlugin tmplFile

genDetectServerImportsPlugin :: Path' (Rel SDK.SdkTemplatesDir) File' -> Generator FileDraft
genDetectServerImportsPlugin tmplFile = return $ SDK.mkTmplFdWithData tmplFile tmplData
  where
    tmplData =
      object
        [ "waspProjectDirFromWebAppDir" .= SP.fromRelDir waspProjectDirFromWebAppDir,
          "srcDirInWaspProjectDir" .= SP.fromRelDir srcDirInWaspProjectDir
        ]

    waspProjectDirFromWebAppDir = waspProjectDirFromAppComponentDir :: Path' (Rel WebAppRootDir) (Dir WaspProjectDir)

genValidateEnvPlugin :: Path' (Rel SDK.SdkTemplatesDir) File' -> Generator FileDraft
genValidateEnvPlugin tmplFile = return $ SDK.mkTmplFd tmplFile

genViteIndexPlugin :: Path' (Rel SDK.SdkTemplatesDir) File' -> Generator FileDraft
genViteIndexPlugin tmplFile = return $ SDK.mkTmplFd tmplFile

genWaspPlugin :: AppSpec -> Path' (Rel SDK.SdkTemplatesDir) File' -> Generator FileDraft
genWaspPlugin spec tmplFile = return $ SDK.mkTmplFdWithData tmplFile tmplData
  where
    tmplData =
      object
        [ "baseDir" .= SP.fromAbsDirP (C.getBaseDir spec),
          "projectDir" .= SP.fromRelDirP relPathFromWebAppRootDirWaspProjectDir,
          "defaultClientPort" .= C.defaultClientPort,
          "buildOutputDir" .= (".wasp/out/web-app/build" :: String),
          "vitest"
            .= object
              [ "excludeWaspArtefactsPattern" .= (SP.fromRelDirP (fromJust $ SP.relDirToPosix dotWaspDirInWaspProjectDir) FP.Posix.</> "**" FP.Posix.</> "*")
              ]
        ]

    relPathFromWebAppRootDirWaspProjectDir :: Path Posix (Rel C.WebAppRootDir) (Dir WaspProjectDir)
    relPathFromWebAppRootDirWaspProjectDir =
      fromJust $
        SP.parseRelDirP $
          FP.Extra.reversePosixPath $
            SP.fromRelDir (dotWaspDirInWaspProjectDir </> generatedCodeDirInDotWaspDir </> webAppRootDirInProjectRootDir)
