module Wasp.Generator.WebAppGenerator.Vite
  ( genVite,
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
import qualified Wasp.AppSpec as AS
import Wasp.Generator.Common (WebAppRootDir, makeJsArrayFromHaskellList)
import Wasp.Generator.FileDraft (FileDraft)
import Wasp.Generator.JsImport (jsImportToImportJson)
import Wasp.Generator.Monad (Generator)
import Wasp.Generator.WebAppGenerator.Common (WebAppTemplatesDir, webAppRootDirInProjectRootDir, webAppSrcDirInWebAppRootDir)
import qualified Wasp.Generator.WebAppGenerator.Common as C
import Wasp.JsImport (JsImport, JsImportName (JsImportModule), JsImportPath (RelativeImportPath), makeJsImport)
import Wasp.Project.Common
  ( WaspProjectDir,
    dotWaspDirInWaspProjectDir,
    generatedCodeDirInDotWaspDir,
    srcDirInWaspProjectDir,
    waspProjectDirFromAppComponentDir,
  )
import Wasp.Util ((<++>))

data VitePluginName = DetectServerImports
  deriving (Enum, Bounded)

type TmplFilePath = Path' (Rel WebAppTemplatesDir) File'

-- We define it like this because we need a list of plugin
-- paths which we will use in the tsconfig.node.json "include" section
type VitePlugin = (VitePluginName, TmplFilePath)

data WebAppVitePluginsDir

genVite :: AppSpec -> Generator [FileDraft]
genVite spec =
  sequence [genViteConfig spec]
    <++> genVitePlugins

vitePlugins :: [VitePlugin]
vitePlugins =
  map
    (\name -> (name, getTmplFilePathForVitePlugin name))
    vitePluginNames
  where
    vitePluginNames = [minBound .. maxBound]

genVitePlugins :: Generator [FileDraft]
genVitePlugins = mapM genVitePlugin vitePlugins

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

-- todo(filip): Take care of this as well
genViteConfig :: AppSpec -> Generator FileDraft
genViteConfig spec = return $ C.mkTmplFdWithData tmplFile tmplData
  where
    tmplFile = C.asTmplFile [relfile|vite.config.ts|]
    tmplData =
      object
        [ "customViteConfig" .= jsImportToImportJson (makeCustomViteConfigJsImport <$> AS.customViteConfigPath spec),
          "baseDir" .= SP.fromAbsDirP (C.getBaseDir spec),
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
            </> webAppRootDirInProjectRootDir
            </> webAppSrcDirInWebAppRootDir
            </> [relfile|test/vitest/setup.ts|]
      ]

    makeCustomViteConfigJsImport :: Path' (Rel WaspProjectDir) File' -> JsImport
    makeCustomViteConfigJsImport pathToConfig = makeJsImport (RelativeImportPath importPath) importName
      where
        importPath = SP.castRel $ C.toViteImportPath relPathToConfigInProjectDir
        relPathToConfigInProjectDir = relPathFromWebAppRootDirWaspProjectDir </> (fromJust . SP.relFileToPosix $ pathToConfig)

        relPathFromWebAppRootDirWaspProjectDir :: Path Posix (Rel C.WebAppRootDir) (Dir WaspProjectDir)
        relPathFromWebAppRootDirWaspProjectDir =
          fromJust $
            SP.parseRelDirP $
              FP.Extra.reversePosixPath $
                SP.fromRelDir (dotWaspDirInWaspProjectDir </> generatedCodeDirInDotWaspDir </> C.webAppRootDirInProjectRootDir)

        importName = JsImportModule "customViteConfig"

vitePluginsDirInWebAppDir :: Path' (Rel WebAppRootDir) (Dir WebAppVitePluginsDir)
vitePluginsDirInWebAppDir = [reldir|vite|]
