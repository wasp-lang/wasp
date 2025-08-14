module Wasp.Generator.WebAppGenerator.Vite (genVite) where

import Data.Aeson (object, (.=))
import Data.Maybe (fromJust)
import qualified FilePath.Extra as FP.Extra
import StrongPath (Dir, File', Path, Path', Posix, Rel, relfile, (</>))
import qualified StrongPath as SP
import qualified System.FilePath.Posix as FP.Posix
import Wasp.AppSpec (AppSpec)
import qualified Wasp.AppSpec as AS
import Wasp.Generator.Common (makeJsArrayFromHaskellList)
import Wasp.Generator.FileDraft (FileDraft)
import Wasp.Generator.JsImport (jsImportToImportJson)
import Wasp.Generator.Monad (Generator)
import qualified Wasp.Generator.WaspLibs.WaspLib as WaspLib
import Wasp.Generator.WebAppGenerator.Common
  ( WebAppTemplatesDir,
    webAppRootDirInProjectRootDir,
    webAppSrcDirInWebAppRootDir,
  )
import qualified Wasp.Generator.WebAppGenerator.Common as C
import Wasp.Generator.WebAppGenerator.Vite.VitePlugin (genVitePlugins, vitePlugins)
import Wasp.JsImport
  ( JsImport,
    JsImportName (JsImportModule),
    JsImportPath (RelativeImportPath),
    makeJsImport,
  )
import Wasp.Project.Common
  ( WaspProjectDir,
    dotWaspDirInWaspProjectDir,
    generatedCodeDirInDotWaspDir,
  )
import Wasp.Util ((<++>))

genVite :: AppSpec -> Generator [FileDraft]
genVite spec =
  sequence
    [ genViteConfig spec,
      genViteTsconfigJson
    ]
    <++> genVitePlugins

viteConfigTmplFile :: Path' (Rel WebAppTemplatesDir) File'
viteConfigTmplFile = C.asTmplFile [relfile|vite.config.ts|]

relPathFromWebAppRootDirWaspProjectDir :: Path Posix (Rel C.WebAppRootDir) (Dir WaspProjectDir)
relPathFromWebAppRootDirWaspProjectDir =
  fromJust $
    SP.parseRelDirP $
      FP.Extra.reversePosixPath $
        SP.fromRelDir (dotWaspDirInWaspProjectDir </> generatedCodeDirInDotWaspDir </> C.webAppRootDirInProjectRootDir)

genViteConfig :: AppSpec -> Generator FileDraft
genViteConfig spec = return $ C.mkTmplFdWithData viteConfigTmplFile tmplData
  where
    tmplData =
      object
        [ "customViteConfig" .= jsImportToImportJson (makeCustomViteConfigJsImport <$> AS.customViteConfigPath spec),
          "baseDir" .= SP.fromAbsDirP (C.getBaseDir spec),
          "projectDir" .= SP.fromRelDirP relPathFromWebAppRootDirWaspProjectDir,
          "defaultClientPort" .= C.defaultClientPort,
          "depsExcludedFromOptimization" .= makeJsArrayFromHaskellList depsExcludedFromOptimization,
          "vitest"
            .= object
              [ "setupFilesArray" .= makeJsArrayFromHaskellList vitestSetupFiles,
                "excludeWaspArtefactsPattern" .= (SP.fromRelDirP (fromJust $ SP.relDirToPosix dotWaspDirInWaspProjectDir) FP.Posix.</> "**" FP.Posix.</> "*")
              ]
        ]
    depsExcludedFromOptimization = "wasp" : map WaspLib.packageName (AS.waspLibs spec)
    vitestSetupFiles =
      [ SP.fromRelFile $
          dotWaspDirInWaspProjectDir
            </> generatedCodeDirInDotWaspDir
            </> webAppRootDirInProjectRootDir
            </> webAppSrcDirInWebAppRootDir
            </> [relfile|test/vitest/setup.ts|]
      ]

-- | Creates an import of user defined Vite config file relative to the web-app root directory e.g. import customViteConfig from '../../../vite.config'
makeCustomViteConfigJsImport :: Path' (Rel WaspProjectDir) File' -> JsImport
makeCustomViteConfigJsImport pathToConfig = makeJsImport (RelativeImportPath importPath) importName
  where
    importPath = SP.castRel $ C.toViteImportPath relPathToConfigInProjectDir
    relPathToConfigInProjectDir = relPathFromWebAppRootDirWaspProjectDir </> (fromJust . SP.relFileToPosix $ pathToConfig)
    importName = JsImportModule "customViteConfig"

genViteTsconfigJson :: Generator FileDraft
genViteTsconfigJson = return $ C.mkTmplFdWithData [relfile|tsconfig.vite.json|] tmplData
  where
    tmplData = object ["includePaths" .= includePaths]

    includePaths =
      SP.fromRelFile viteConfigTmplFile : vitePluginPaths

    vitePluginPaths = map (SP.fromRelFile . snd) vitePlugins
