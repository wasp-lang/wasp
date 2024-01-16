module Wasp.Project.Vite where

import Data.List (find)
import StrongPath (File', Path', Rel, relfile)
import Wasp.AppSpec.ExternalFiles (SourceExternalCodeDir)
import qualified Wasp.AppSpec.ExternalFiles as ExternalFiles

-- todo(filip): move this
findCustomViteConfigPath :: [ExternalFiles.CodeFile] -> Maybe (Path' (Rel SourceExternalCodeDir) File')
findCustomViteConfigPath externalClientCodeFiles = ExternalFiles._pathInExtCodeDir <$> maybeCustomViteConfigPath
  where
    maybeCustomViteConfigPath = find isCustomViteConfig externalClientCodeFiles

    isCustomViteConfig :: ExternalFiles.CodeFile -> Bool
    isCustomViteConfig
      ExternalFiles.CodeFile
        { _pathInExtCodeDir = path
        } = path == pathToViteTsConfig || path == pathToViteJsConfig

    pathToViteTsConfig :: Path' (Rel SourceExternalCodeDir) File'
    pathToViteTsConfig = [relfile|vite.config.ts|]

    pathToViteJsConfig :: Path' (Rel SourceExternalCodeDir) File'
    pathToViteJsConfig = [relfile|vite.config.js|]
