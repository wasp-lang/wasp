module Wasp.Generator.NpmInstall.PackageLock
  ( removeStaleWaspLibIntegritiesFromPackageLock,
    removeStaleWaspLibIntegrities,
    getWaspLibTarballPaths,
  )
where

import Control.Monad (unless)
import Crypto.Hash (SHA512 (..), hashWith)
import Data.Aeson (Value (..), eitherDecode, encode)
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteArray.Encoding as BAE
import qualified Data.ByteString.Lazy as BL
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import StrongPath (Abs, Dir, Path', fromAbsDir, fromRelDir, (</>))
import qualified System.Directory as SD
import qualified System.FilePath as FP
import Wasp.Generator.WaspLibs.Common (libsRootDirInGeneratedAppDir)
import Wasp.Project.Common (WaspProjectDir, generatedAppDirInWaspProjectDir, packageLockJsonInWaspProjectDir)
import qualified Wasp.Util.IO as IOUtil

-- | In a dev build of Wasp, a lib tarball can change without a version bump.
-- npm then fails with EINTEGRITY because of the stale `integrity` in the
-- lockfile. Without the field, npm installs the tarball and doesn't write the
-- field back.
removeStaleWaspLibIntegritiesFromPackageLock :: Path' Abs (Dir WaspProjectDir) -> IO (Either String ())
removeStaleWaspLibIntegritiesFromPackageLock waspProjectDir = do
  isPackageLockPresent <- IOUtil.doesFileExist packageLockPath
  if not isPackageLockPresent
    then return $ Right ()
    else do
      packageLockOrError <- eitherDecode <$> IOUtil.readFileBytes packageLockPath
      case packageLockOrError of
        Left err -> return $ Left err
        Right packageLock -> do
          currentIntegrities <- getCurrentIntegrities $ getWaspLibTarballPaths packageLock
          let updatedPackageLock = removeStaleWaspLibIntegrities currentIntegrities packageLock
          unless (updatedPackageLock == packageLock)
            $ IOUtil.writeFileBytes packageLockPath
            $ encode updatedPackageLock
          return $ Right ()
  where
    packageLockPath = waspProjectDir </> packageLockJsonInWaspProjectDir

    getCurrentIntegrities :: [Text] -> IO (Map Text Text)
    getCurrentIntegrities tarballPaths =
      Map.fromList . concat <$> mapM getCurrentIntegrity tarballPaths

    getCurrentIntegrity tarballPath = do
      let tarballAbsPath = fromAbsDir waspProjectDir FP.</> T.unpack tarballPath
      tarballExists <- SD.doesFileExist tarballAbsPath
      if tarballExists
        then (\tarball -> [(tarballPath, calculateIntegrity tarball)]) <$> BL.readFile tarballAbsPath
        else return []

-- | Takes the current integrities of Wasp lib tarballs, keyed by their path in
-- the project, and removes the lockfile integrities that don't match them.
removeStaleWaspLibIntegrities :: Map Text Text -> Value -> Value
removeStaleWaspLibIntegrities currentIntegrities = mapPackages removeIntegrityIfStale
  where
    removeIntegrityIfStale package = case getWaspLibTarballPath package of
      Just tarballPath
        | KeyMap.lookup "integrity" package /= (String <$> Map.lookup tarballPath currentIntegrities) ->
            KeyMap.delete "integrity" package
      _ -> package

getWaspLibTarballPaths :: Value -> [Text]
getWaspLibTarballPaths (Object packageLock) = case KeyMap.lookup "packages" packageLock of
  Just (Object packages) -> mapMaybe getPackageTarballPath $ KeyMap.elems packages
  _ -> []
  where
    getPackageTarballPath (Object package) = getWaspLibTarballPath package
    getPackageTarballPath _ = Nothing
getWaspLibTarballPaths _ = []

getWaspLibTarballPath :: KeyMap.KeyMap Value -> Maybe Text
getWaspLibTarballPath package = case KeyMap.lookup "resolved" package of
  Just (String resolved)
    | waspLibsResolvedPrefix `T.isPrefixOf` resolved -> T.stripPrefix "file:" resolved
  _ -> Nothing
  where
    waspLibsResolvedPrefix =
      T.pack $ "file:" ++ fromRelDir (generatedAppDirInWaspProjectDir </> libsRootDirInGeneratedAppDir)

mapPackages :: (KeyMap.KeyMap Value -> KeyMap.KeyMap Value) -> Value -> Value
mapPackages updatePackage (Object packageLock) = case KeyMap.lookup "packages" packageLock of
  Just packages -> Object $ KeyMap.insert "packages" (updatePackages packages) packageLock
  Nothing -> Object packageLock
  where
    updatePackages (Object packages) = Object $ KeyMap.map updatePackageObject packages
    updatePackages packages = packages

    updatePackageObject (Object package) = Object $ updatePackage package
    updatePackageObject package = package
mapPackages _ packageLock = packageLock

-- | Same format as npm's `integrity` field (Subresource Integrity).
calculateIntegrity :: BL.ByteString -> Text
calculateIntegrity tarball =
  "sha512-" <> TE.decodeUtf8 (BAE.convertToBase BAE.Base64 $ hashWith SHA512 $ BL.toStrict tarball)
