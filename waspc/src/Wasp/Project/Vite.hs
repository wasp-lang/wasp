module Wasp.Project.Vite where

import Data.List (find)
import StrongPath (Abs, Dir, File', Path', Rel, relfile)
import Wasp.Project.Common (WaspProjectDir)
import qualified Wasp.Util.IO as Util.IO

findCustomViteConfigPath :: Path' Abs (Dir WaspProjectDir) -> IO (Maybe (Path' (Rel WaspProjectDir) File'))
findCustomViteConfigPath waspProjectDir = do
  waspProjectDirFiles <- fst <$> Util.IO.listDirectory waspProjectDir

  return $ find isCustomViteConfig waspProjectDirFiles
  where
    isCustomViteConfig path = path == pathToViteTsConfig || path == pathToViteJsConfig

    pathToViteTsConfig :: Path' (Rel WaspProjectDir) File'
    pathToViteTsConfig = [relfile|vite.config.ts|]

    pathToViteJsConfig :: Path' (Rel WaspProjectDir) File'
    pathToViteJsConfig = [relfile|vite.config.js|]
