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
import Wasp.Generator.SdkGenerator.Common (sdkPackageName)
import qualified Wasp.Generator.WaspLibs.AvailableLibs as WaspLibs.AvailableLibs
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
genViteConfig spec = return $ C.mkTmplFdWithData viteConfigTmplFile . getTmplData $ WaspLibs.AvailableLibs.waspLibs
  where
    getTmplData waspLibs =
      object
        [ "customViteConfig" .= jsImportToImportJson (makeCustomViteConfigJsImport <$> AS.customViteConfigPath spec),
          "baseDir" .= SP.fromAbsDirP (C.getBaseDir spec),
          "projectDir" .= SP.fromRelDirP relPathFromWebAppRootDirWaspProjectDir,
          "defaultClientPort" .= C.defaultClientPort,
          "depsExcludedFromOptimization" .= makeJsArrayFromHaskellList (getDepsExcludedFromOptimization waspLibs),
          "vitest"
            .= object
              [ "setupFilesArray" .= makeJsArrayFromHaskellList vitestSetupFiles,
                "excludeWaspArtefactsPattern" .= (SP.fromRelDirP (fromJust $ SP.relDirToPosix dotWaspDirInWaspProjectDir) FP.Posix.</> "**" FP.Posix.</> "*")
              ]
        ]
    getDepsExcludedFromOptimization waspLibs =
      -- Why do we exclude Wasp SDK from optimization?
      -- - Wasp SDK is a dep that's regenerated over time and we don't want Vite to optimize it
      --   and cache it (which would break hot module reloading).
      -- - Accidentally, we don't need to do this because Wasp SDK is symlinked and Vite would
      --   exclude it anyways - but we are keeping it here because we want to be explicit.
      --   Read more: https://vite.dev/guide/dep-pre-bundling#monorepos-and-linked-dependencies
      sdkPackageName
        :
        -- Wasp libs are excluded from optimization because they are internal npm packages that
        -- have a dummy `0.0.0` version which means once they are cached by Vite, they aren't
        -- updated even though the lib changes.
        -- Read more about libs versioning in `waspc/libs/README.md`.
        map WaspLib.packageName waspLibs
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
