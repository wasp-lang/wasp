module Wasp.Generator.WebAppGenerator.Vite (genVite) where

import Data.Aeson (object, (.=))
import Data.Maybe (fromJust)
import qualified FilePath.Extra as FP.Extra
import StrongPath (Dir, File', Path, Path', Posix, Rel, relfile, (</>))
import qualified StrongPath as SP
import Wasp.AppSpec (AppSpec)
import qualified Wasp.AppSpec as AS
import Wasp.Generator.FileDraft (FileDraft)
import Wasp.Generator.JsImport (jsImportToImportJson)
import Wasp.Generator.Monad (Generator)
import Wasp.Generator.WebAppGenerator.Common
  ( WebAppTemplatesDir,
  )
import qualified Wasp.Generator.WebAppGenerator.Common as C
import Wasp.Generator.WebAppGenerator.Vite.VitePlugin (genVitePlugins, vitePluginsGlob)
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
    <++> genVitePlugins spec

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
        [ "customViteConfig" .= jsImportToImportJson (makeCustomViteConfigJsImport <$> AS.customViteConfigPath spec)
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
      SP.fromRelFile viteConfigTmplFile : [vitePluginsGlob]
