module Wasp.ConfigFiles
  ( discoverConfigFiles,
    isTailwindUsed,
  )
where

import Data.List (find, isSuffixOf)
import Data.Maybe (isJust)
import StrongPath (Abs, Dir, File', Path')
import qualified StrongPath as SP
import Wasp.AppSpec (AppSpec)
import qualified Wasp.AppSpec as AS
import Wasp.Common (WaspProjectDir)
import qualified Wasp.Util.IO as Util.IO

-- | Discovers config files in the wasp project dir.
-- NOTE: In the future, we could allow devs to configure what files we look for and where we copy them to.
discoverConfigFiles :: Path' Abs (Dir WaspProjectDir) -> IO [Path' Abs File']
discoverConfigFiles waspDir = do
  files <- fst <$> Util.IO.listDirectory waspDir
  return $ (waspDir SP.</>) <$> filter isConfigFile files
  where
    -- TODO: Make this configurable.
    isConfigFile path = ".config.js" `isSuffixOf` SP.toFilePath path

isTailwindUsed :: AppSpec -> Bool
isTailwindUsed spec = configExists "tailwind.config.js" && configExists "postcss.config.js"
  where
    configExists filename = isJust $ find (\f -> filename `isSuffixOf` SP.toFilePath f) (AS.configFiles spec)
