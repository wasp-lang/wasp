module Wasp.Project.WebApp where

import StrongPath (Abs, Dir, Path', Rel, fromAbsDir, reldir, (</>))
import System.Directory (doesDirectoryExist)
import Wasp.AppSpec.ExternalCode (SourceExternalCodeDir)

data StaticAssetsDir

staticAssetsDirInExtClientCodeDir :: Path' (Rel SourceExternalCodeDir) (Dir StaticAssetsDir)
staticAssetsDirInExtClientCodeDir = [reldir|public|]

findStaticAssetsDir ::
  Path' Abs (Dir SourceExternalCodeDir) ->
  IO (Maybe (Path' Abs (Dir StaticAssetsDir)))
findStaticAssetsDir externalClientCodeDirPath = do
  let staticAssetsDirPath = externalClientCodeDirPath </> staticAssetsDirInExtClientCodeDir
  staticAssetsDirExists <- doesDirectoryExist $ fromAbsDir staticAssetsDirPath
  return $ if staticAssetsDirExists then Just staticAssetsDirPath else Nothing
