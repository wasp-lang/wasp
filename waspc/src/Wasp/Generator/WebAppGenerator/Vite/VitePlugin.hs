module Wasp.Generator.WebAppGenerator.Vite.VitePlugin
  ( genVitePlugins,
    vitePlugins,
  )
where

import Data.Aeson (object, (.=))
import StrongPath (Dir, File', Path', Rel, reldir, relfile, (</>))
import qualified StrongPath as SP
import Wasp.Generator.Common (WebAppRootDir)
import Wasp.Generator.FileDraft (FileDraft)
import Wasp.Generator.Monad (Generator)
import Wasp.Generator.WebAppGenerator.Common (WebAppTemplatesDir)
import qualified Wasp.Generator.WebAppGenerator.Common as C
import Wasp.Project.Common
  ( WaspProjectDir,
    srcDirInWaspProjectDir,
    waspProjectDirFromAppComponentDir,
  )

data VitePluginName = DetectServerImports
  deriving (Enum, Bounded)

type TmplFilePath = Path' (Rel WebAppTemplatesDir) File'

-- We define it like this because we need a list of plugin
-- paths which we will use in the tsconfig.vite.json "include" section
type VitePlugin = (VitePluginName, TmplFilePath)

genVitePlugins :: Generator [FileDraft]
genVitePlugins = mapM genVitePlugin vitePlugins

vitePlugins :: [VitePlugin]
vitePlugins =
  map
    (\name -> (name, getTmplFilePathForVitePlugin name))
    vitePluginNames
  where
    vitePluginNames = [minBound .. maxBound]

data WebAppVitePluginsDir

vitePluginsDirInWebAppDir :: Path' (Rel WebAppRootDir) (Dir WebAppVitePluginsDir)
vitePluginsDirInWebAppDir = [reldir|vite|]

getTmplFilePathForVitePlugin :: VitePluginName -> TmplFilePath
getTmplFilePathForVitePlugin DetectServerImports = C.asTmplFile $ vitePluginsDirInWebAppDir </> [relfile|detectServerImports.ts|]

genVitePlugin :: VitePlugin -> Generator FileDraft
genVitePlugin (DetectServerImports, tmplFile) = genDetectServerImportsPlugin tmplFile

genDetectServerImportsPlugin :: Path' (Rel WebAppTemplatesDir) File' -> Generator FileDraft
genDetectServerImportsPlugin tmplFile = return $ C.mkTmplFdWithData tmplFile tmplData
  where
    tmplData =
      object
        [ "waspProjectDirFromWebAppDir" .= SP.fromRelDir waspProjectDirFromWebAppDir,
          "srcDirInWaspProjectDir" .= SP.fromRelDir srcDirInWaspProjectDir
        ]

    waspProjectDirFromWebAppDir = waspProjectDirFromAppComponentDir :: Path' (Rel WebAppRootDir) (Dir WaspProjectDir)
