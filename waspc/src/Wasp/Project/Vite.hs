module Wasp.Project.Vite where

import Data.List (find)
import Data.Maybe (fromJust)
import StrongPath (File', Path', Rel, reldir, relfile, (</>))
import qualified StrongPath.Operations as SP
import Wasp.AppSpec.ExternalCode (SourceExternalCodeDir)
import qualified Wasp.AppSpec.ExternalCode as ExternalCode
import qualified Wasp.Generator.ExternalCodeGenerator.Common as ECC
import qualified Wasp.Generator.WebAppGenerator.Common as C
import qualified Wasp.Generator.WebAppGenerator.ExternalCodeGenerator as EC
import Wasp.JsImport (JsImport, JsImportName (JsImportModule), makeJsImport)

findCustomViteConfigPath :: [ExternalCode.File] -> Maybe (Path' (Rel SourceExternalCodeDir) File')
findCustomViteConfigPath externalClientCodeFiles = ExternalCode._pathInExtCodeDir <$> maybeCustomViteConfigPath
  where
    maybeCustomViteConfigPath = find isCustomViteConfig externalClientCodeFiles

    isCustomViteConfig :: ExternalCode.File -> Bool
    isCustomViteConfig
      ExternalCode.File
        { _pathInExtCodeDir = path
        } = path == pathToViteTsConfig || path == pathToViteJsConfig

    pathToViteTsConfig :: Path' (Rel SourceExternalCodeDir) File'
    pathToViteTsConfig = [relfile|vite.config.ts|]

    pathToViteJsConfig :: Path' (Rel SourceExternalCodeDir) File'
    pathToViteJsConfig = [relfile|vite.config.js|]

makeCustomViteConfigJsImport :: Path' (Rel SourceExternalCodeDir) File' -> JsImport
makeCustomViteConfigJsImport pathToConfig = makeJsImport importPath importName
  where
    importPath = C.toViteImportPath $ fromJust $ SP.relFileToPosix pathToConfigInSrc
    pathToConfigInSrc = [reldir|src|] </> EC.extClientCodeDirInWebAppSrcDir </> ECC.castRelPathFromSrcToGenExtCodeDir pathToConfig

    importName = JsImportModule "customViteConfig"
