module Wasp.Project.WebApp where

import Data.Maybe (fromMaybe)
import StrongPath (Dir, Path', Rel, reldir)
import Wasp.AppSpec (AppSpec)
import qualified Wasp.AppSpec.App as AS.App
import qualified Wasp.AppSpec.App.Client as AS.App.Client
import Wasp.AppSpec.ExternalCode (SourceExternalCodeDir)
import Wasp.AppSpec.Valid (getApp)

data StaticAssetsDir

staticAssetsDirInExtClientCodeDir :: Path' (Rel SourceExternalCodeDir) (Dir StaticAssetsDir)
staticAssetsDirInExtClientCodeDir = [reldir|public|]

getBaseDir :: AppSpec -> String
getBaseDir spec = fromMaybe "/" maybeBaseDir
  where
    maybeBaseDir = AS.App.Client.baseDir =<< AS.App.client (snd $ getApp spec)

defaultClientPort :: Int
defaultClientPort = 3000

getDefaultClientUrl :: AppSpec -> String
getDefaultClientUrl spec = "http://localhost:" ++ show defaultClientPort ++ getBaseDir spec
