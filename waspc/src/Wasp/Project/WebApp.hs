module Wasp.Project.WebApp where

import StrongPath (Dir, Path', Rel, reldir)
import Wasp.AppSpec.ExternalCode (SourceExternalCodeDir)

data StaticAssetsDir

staticAssetsDirInExtClientCodeDir :: Path' (Rel SourceExternalCodeDir) (Dir StaticAssetsDir)
staticAssetsDirInExtClientCodeDir = [reldir|public|]
