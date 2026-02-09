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
import Wasp.Generator.SdkGenerator.Client.VitePlugin.Common
  ( clientEntryPointPath,
    virtualFilesFilesDirInSdkTemplatesUserCoreDir,
    vitePluginsDirInSdkTemplatesUserCoreDir,
  )
import Wasp.Generator.SdkGenerator.UserCore.Common
  ( genFileCopy,
    mkTmplFdWithData,
  )

genHtmlPlugin :: AppSpec -> Generator [FileDraft]
genHtmlPlugin spec =
  sequence
    [ genHtmlDevTs,
      genHtmlBuildTs,
      genVirtualIndexHtml spec
    ]

genHtmlDevTs :: Generator FileDraft
genHtmlDevTs = genFileCopy $ vitePluginsDirInSdkTemplatesUserCoreDir </> [relfile|html/dev.ts|]

genHtmlBuildTs :: Generator FileDraft
genHtmlBuildTs = genFileCopy $ vitePluginsDirInSdkTemplatesUserCoreDir </> [relfile|html/build.ts|]

genVirtualIndexHtml :: AppSpec -> Generator FileDraft
genVirtualIndexHtml spec =
  return $
    mkTmplFdWithData
      (virtualFilesFilesDirInSdkTemplatesUserCoreDir </> [relfile|index.html|])
      tmplData
  where
    tmplData =
      object
        [ "title" .= (AS.App.title (snd $ getApp spec) :: String),
          "head" .= (maybe "" (intercalate "\n") (AS.App.head $ snd $ getApp spec) :: String),
          "clientEntryPointPath" .= clientEntryPointPath
        ]
