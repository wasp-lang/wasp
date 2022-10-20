module Wasp.ConfigFiles
  ( discoverConfigFiles,
    ConfigFile (..),
  )
where

import Data.List (isSuffixOf)
import StrongPath (Abs, Dir, File', Path', Rel)
import qualified StrongPath as SP
import Wasp.Common (WaspProjectDir)
import Wasp.Generator.ServerGenerator.Common (ServerRootDir)
import Wasp.Generator.WebAppGenerator.Common (WebAppRootDir)
import qualified Wasp.Generator.WebAppGenerator.Common as C
import qualified Wasp.Util.IO as Util.IO

data ConfigFile = ConfigFile
  { _pathInWaspDir :: Path' Abs File',
    webAppDstPath :: Maybe (Path' (Rel WebAppRootDir) File'),
    serverDstPath :: Maybe (Path' (Rel ServerRootDir) File')
  }

-- | Discovers config files in the wasp project dir.
-- NOTE: In the future, we could allow devs to configure what files we look for and where we copy them to.
-- For now, we just assume they all go in the web app dir.
discoverConfigFiles :: Path' Abs (Dir WaspProjectDir) -> IO [ConfigFile]
discoverConfigFiles waspDir = do
  files <- fst <$> Util.IO.listDirectory waspDir
  let configFiles = (waspDir SP.</>) <$> filter isConfigFile files
  return $
    map
      ( \f -> do
          ConfigFile
            { _pathInWaspDir = f,
              webAppDstPath = Just $ C.asWebAppFile $ SP.basename f,
              serverDstPath = Nothing
            }
      )
      configFiles
  where
    -- TODO: Make this configurable.
    isConfigFile path = ".config.js" `isSuffixOf` SP.toFilePath path
