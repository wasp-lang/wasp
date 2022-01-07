module Wasp.Generator.ServerGenerator.ConfigG
  ( genConfigFile,
    configFileInSrcDir,
  )
where

import Data.Aeson (object, (.=))
import Data.Maybe (isJust)
import StrongPath (File', Path', Rel, relfile, (</>))
import qualified StrongPath as SP
import Wasp.AppSpec (AppSpec, getApp)
import qualified Wasp.AppSpec.App as AS.App
import Wasp.Generator.FileDraft (FileDraft)
import qualified Wasp.Generator.ServerGenerator.Common as C

genConfigFile :: AppSpec -> FileDraft
genConfigFile spec = C.makeTemplateFD tmplFile dstFile (Just tmplData)
  where
    tmplFile = C.srcDirInServerTemplatesDir </> SP.castRel configFileInSrcDir
    dstFile = C.serverSrcDirInServerRootDir </> configFileInSrcDir
    tmplData =
      object
        [ "isAuthEnabled" .= isJust (AS.App.auth $ snd $ getApp spec)
        ]

configFileInSrcDir :: Path' (Rel C.ServerSrcDir) File'
configFileInSrcDir = [relfile|config.js|]
