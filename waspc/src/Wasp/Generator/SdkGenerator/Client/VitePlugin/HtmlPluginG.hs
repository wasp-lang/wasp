{-# LANGUAGE TypeApplications #-}

module Wasp.Generator.SdkGenerator.Client.VitePlugin.HtmlPluginG
  ( genHtmlPlugin,
  )
where

import Data.Aeson (object, (.=))
import Data.List (intercalate)
import StrongPath (relfile, (</>))
import Wasp.AppSpec (AppSpec)
import qualified Wasp.AppSpec.App as AS.App
import Wasp.AppSpec.Valid (getApp)
import Wasp.Generator.FileDraft (FileDraft)
import Wasp.Generator.Monad (Generator)
import Wasp.Generator.SdkGenerator.Client.VitePlugin.Common (indexTsxVirtualFileName, virtualFilesDirInVitePluginsDir)
import qualified Wasp.Generator.SdkGenerator.Common as C

genHtmlPlugin :: AppSpec -> Generator [FileDraft]
genHtmlPlugin spec =
  sequence
    [ genHtmlTs,
      genVirtualIndexHtml spec
    ]

genHtmlTs :: Generator FileDraft
genHtmlTs =
  return $
    C.mkTmplFdWithData tmplPath tmplData
  where
    tmplPath = C.vitePluginsDirInSdkTemplatesDir </> [relfile|html.ts|]
    tmplData = object ["clientEntryPointPath" .= clientEntryPointPath]

genVirtualIndexHtml :: AppSpec -> Generator FileDraft
genVirtualIndexHtml spec =
  return $
    C.mkTmplFdWithData tmplPath tmplData
  where
    tmplPath = C.vitePluginsDirInSdkTemplatesDir </> virtualFilesDirInVitePluginsDir </> [relfile|indexHtml.virtual.ts|]
    tmplData =
      object
        [ "title" .= (AS.App.title (snd $ getApp spec) :: String),
          "head" .= (maybe "" (intercalate "\n") (AS.App.head $ snd $ getApp spec) :: String),
          "clientEntryPointPath" .= clientEntryPointPath
        ]

clientEntryPointPath :: String
clientEntryPointPath = "/" ++ indexTsxVirtualFileName
