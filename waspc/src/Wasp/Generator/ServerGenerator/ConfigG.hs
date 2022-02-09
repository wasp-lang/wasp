module Wasp.Generator.ServerGenerator.ConfigG
  ( genConfigFile,
    configFileInSrcDir,
  )
where

import Data.Aeson (object, (.=))
import StrongPath (File', Path', Rel, relfile, (</>))
import qualified StrongPath as SP
import Wasp.AppSpec (AppSpec)
import qualified Wasp.AppSpec as AS
import Wasp.AppSpec.Valid (Valid)
import qualified Wasp.AppSpec.Valid.AppSpec as VAS
import Wasp.Generator.FileDraft (FileDraft)
import Wasp.Generator.Monad (Generator)
import qualified Wasp.Generator.ServerGenerator.Common as C

genConfigFile :: Valid AppSpec -> Generator FileDraft
genConfigFile spec = return $ C.mkTmplFdWithDstAndData tmplFile dstFile (Just tmplData)
  where
    tmplFile = C.srcDirInServerTemplatesDir </> SP.castRel configFileInSrcDir
    dstFile = C.serverSrcDirInServerRootDir </> configFileInSrcDir
    tmplData =
      object
        [ "isAuthEnabled" .= (VAS.isAuthEnabled spec :: Bool)
        ]

configFileInSrcDir :: Path' (Rel C.ServerSrcDir) File'
configFileInSrcDir = [relfile|config.js|]
