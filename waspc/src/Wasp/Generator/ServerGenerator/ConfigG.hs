module Wasp.Generator.ServerGenerator.ConfigG
  ( genConfigFile,
  )
where

import Data.Aeson (object, (.=))
import StrongPath (File', Path', Rel, relfile, (</>))
import qualified StrongPath as SP
import Wasp.AppSpec (AppSpec)
import Wasp.AppSpec.Valid (isAuthEnabled)
import Wasp.Generator.FileDraft (FileDraft)
import Wasp.Generator.Monad (Generator)
import qualified Wasp.Generator.ServerGenerator.Common as C
import qualified Wasp.Generator.WebAppGenerator.Common as WebApp
import Wasp.Project.Db (databaseUrlEnvVarName)

genConfigFile :: AppSpec -> Generator FileDraft
genConfigFile spec = return $ C.mkTmplFdWithDstAndData tmplFile dstFile (Just tmplData)
  where
    tmplFile = C.srcDirInServerTemplatesDir </> SP.castRel configFileInSrcDir
    dstFile = C.serverSrcDirInServerRootDir </> configFileInSrcDir
    tmplData =
      object
        [ "isAuthEnabled" .= isAuthEnabled spec,
          "databaseUrlEnvVarName" .= databaseUrlEnvVarName,
          "defaultClientUrl" .= WebApp.getDefaultClientUrl spec
        ]

configFileInSrcDir :: Path' (Rel C.ServerSrcDir) File'
configFileInSrcDir = [relfile|config.js|]
