module Generator.ServerGenerator.ConfigG
  ( genConfigFile,
    configFileInSrcDir,
  )
where

import Data.Aeson (object, (.=))
import Data.Maybe (isJust)
import Generator.FileDraft (FileDraft)
import qualified Generator.ServerGenerator.Common as C
import StrongPath (File', Path', Rel, relfile, (</>))
import qualified StrongPath as SP
import Wasp (Wasp, getAuth)

genConfigFile :: Wasp -> FileDraft
genConfigFile wasp = C.makeTemplateFD tmplFile dstFile (Just tmplData)
  where
    tmplFile = C.srcDirInServerTemplatesDir </> SP.castRel configFileInSrcDir
    dstFile = C.serverSrcDirInServerRootDir </> configFileInSrcDir
    tmplData =
      object
        [ "isAuthEnabled" .= isJust (getAuth wasp)
        ]

configFileInSrcDir :: Path' (Rel C.ServerSrcDir) File'
configFileInSrcDir = [relfile|config.js|]
