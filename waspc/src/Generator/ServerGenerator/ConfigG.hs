module Generator.ServerGenerator.ConfigG
    ( genConfigFile
    , configFileInSrcDir
    ) where

import           Data.Aeson                       (object, (.=))
import           Data.Maybe                       (isJust)
import qualified Path                             as P
import           StrongPath                       (File, Path, Rel, (</>))
import qualified StrongPath                       as SP

import           Generator.FileDraft              (FileDraft)
import qualified Generator.ServerGenerator.Common as C
import           Wasp                             (Wasp, getAuth)


genConfigFile :: Wasp -> FileDraft
genConfigFile wasp = C.makeTemplateFD tmplFile dstFile (Just tmplData)
  where
    tmplFile = C.srcDirInServerTemplatesDir </> SP.castRel configFileInSrcDir
    dstFile = C.serverSrcDirInServerRootDir </> configFileInSrcDir
    tmplData = object
        [ "isAuthEnabled" .= isJust (getAuth wasp)
        ]

configFileInSrcDir :: Path (Rel C.ServerSrcDir) File
configFileInSrcDir = SP.fromPathRelFile [P.relfile|config.js|]
