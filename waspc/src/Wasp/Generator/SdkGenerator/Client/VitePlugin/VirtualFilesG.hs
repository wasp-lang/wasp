{-# LANGUAGE TypeApplications #-}

module Wasp.Generator.SdkGenerator.Client.VitePlugin.VirtualFilesG
  ( genVirtualFiles,
  )
where

import Data.Aeson (Value, object, (.=))
import StrongPath (relfile, (</>))
import qualified StrongPath as SP
import Wasp.AppSpec (AppSpec)
import Wasp.Generator.FileDraft (FileDraft)
import Wasp.Generator.JsImport (jsImportToImportJson)
import Wasp.Generator.Monad (Generator)
import Wasp.Generator.SdkGenerator.Client.VitePlugin.Common (routesEntryPointPath, spaFallbackFile, virtualFilesFilesDirInViteDir)
import Wasp.Generator.SdkGenerator.Client.VitePlugin.VirtualFiles.VirtualRoutesG (genVirtualRoutesTsx)
import qualified Wasp.Generator.SdkGenerator.Common as C
import qualified Wasp.Generator.WebAppGenerator.Common as WebApp
import Wasp.JsImport (JsImportName (JsImportField), JsImportPath (RawImportName), makeValueJsImport)
import Wasp.Util.Js (makeJsStringLiteral)

-- | The entry points that the Vite plugin serves as virtual files.
genVirtualFiles :: AppSpec -> Generator [FileDraft]
genVirtualFiles spec =
  sequence
    [ genVirtualClientEntryTsx spec,
      genVirtualSsrEntryTsx spec,
      genVirtualRoutesTsx spec
    ]

genVirtualClientEntryTsx :: AppSpec -> Generator FileDraft
genVirtualClientEntryTsx spec =
  return $
    C.mkTmplFdWithData tmplPath tmplData
  where
    tmplPath = C.viteDirInSdkTemplatesDir </> virtualFilesFilesDirInViteDir </> [relfile|client-entry.tsx|]
    tmplData =
      object
        [ "routeObjects" .= routeObjectsImportJson,
          "baseDir" .= makeJsStringLiteral (SP.fromAbsDirP (WebApp.getBaseDir spec))
        ]

genVirtualSsrEntryTsx :: AppSpec -> Generator FileDraft
genVirtualSsrEntryTsx spec =
  return $
    C.mkTmplFdWithData tmplPath tmplData
  where
    tmplPath = C.viteDirInSdkTemplatesDir </> virtualFilesFilesDirInViteDir </> [relfile|ssr-entry.tsx|]
    tmplData =
      object
        [ "routeObjects" .= routeObjectsImportJson,
          "spaFallbackFile" .= SP.fromRelFileP spaFallbackFile,
          "baseDir" .= makeJsStringLiteral (SP.fromAbsDirP (WebApp.getBaseDir spec))
        ]

routeObjectsImportJson :: Value
routeObjectsImportJson =
  jsImportToImportJson
    $ Just
    $ makeValueJsImport (RawImportName routesEntryPointPath) (JsImportField "routeObjects")
