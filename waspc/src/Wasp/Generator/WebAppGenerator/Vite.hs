module Wasp.Generator.WebAppGenerator.Vite where

import StrongPath (File', Path', Rel, relfile)
import Wasp.AppSpec.ExternalCode (SourceExternalCodeDir)
import qualified Wasp.AppSpec.ExternalCode as ExternalCode

checkIfCustomViteConfigUsed :: [ExternalCode.File] -> Bool
checkIfCustomViteConfigUsed externalClientCodeFiles = any isCustomViteConfig externalClientCodeFiles
  where
    isCustomViteConfig :: ExternalCode.File -> Bool
    isCustomViteConfig
      ExternalCode.File
        { _pathInExtCodeDir = path
        } = path == pathToViteTsConfig || path == pathToViteJsConfig

    pathToViteTsConfig :: (Path' (Rel SourceExternalCodeDir) File')
    pathToViteTsConfig = [relfile|vite.config.ts|]

    pathToViteJsConfig :: (Path' (Rel SourceExternalCodeDir) File')
    pathToViteJsConfig = [relfile|vite.config.js|]
