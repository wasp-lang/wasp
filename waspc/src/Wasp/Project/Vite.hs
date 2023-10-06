module Wasp.Project.Vite where

import Data.List (find)
import StrongPath (File', Path', Rel, relfile)
import Wasp.AppSpec.ExternalCode (SourceExternalCodeDir)
import qualified Wasp.AppSpec.ExternalCode as ExternalCode

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
