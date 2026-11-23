-- | Checks the exports of the SDK package that Wasp generates.
module Tests.SnapshotTests.SdkPackageExports (assertSdkPackageExportsExist) where

import Control.Monad (filterM, unless)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as AesonKey
import qualified Data.Aeson.KeyMap as AesonKeyMap
import qualified Data.ByteString as BS
import Data.List (isInfixOf, isSuffixOf, stripPrefix)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import E2E
import qualified StrongPath as SP
import System.Directory (doesDirectoryExist, doesFileExist)
import System.FilePath (dropTrailingPathSeparator, (</>))
import Wasp.Generator.SdkGenerator.Common (sdkRootDirInGeneratedAppDir)
import Wasp.Project.Common (generatedAppDirInWaspProjectDir)

-- | Checks that every export of the generated SDK package points at one of its
-- generated source files. Runs in the Wasp project, after it's compiled.
assertSdkPackageExportsExist :: TestM ()
assertSdkPackageExportsExist = do
  sdkPackageDir <- resolvePath . SP.fromRelDir $ generatedAppDirInWaspProjectDir SP.</> sdkRootDirInGeneratedAppDir
  packageJson <- liftIO . BS.readFile $ sdkPackageDir </> "package.json"
  packageExports <- either (failTest . ("Unexpected SDK package.json: " ++)) return $ parsePackageExports packageJson
  missingExportTargets <- liftIO $ filterM (fmap not . exportTargetExists sdkPackageDir) packageExports
  unless (null missingExportTargets)
    $ failTest
    $ "Broken SDK package exports:\n" ++ unlines (map formatPackageExport missingExportTargets)

data PackageExport = PackageExport
  { exportName :: String,
    exportTarget :: String
  }

-- | Fails on exports that aren't a plain path (e.g. conditional exports), so
-- that the check doesn't skip them: it would need to learn about them first.
parsePackageExports :: BS.ByteString -> Either String [PackageExport]
parsePackageExports packageJson =
  Aeson.eitherDecodeStrict packageJson >>= \case
    Aeson.Object packageJsonObject -> case AesonKeyMap.lookup "exports" packageJsonObject of
      Just (Aeson.Object exportsObject) -> mapM toPackageExport $ AesonKeyMap.toList exportsObject
      Just _ -> Left "expected \"exports\" to be an object"
      Nothing -> Left "expected it to define \"exports\""
    _ -> Left "expected it to be an object"
  where
    toPackageExport (name, Aeson.String target) = Right $ PackageExport (AesonKey.toString name) (T.unpack target)
    toPackageExport (name, _) = Left $ "expected the export " ++ show (AesonKey.toString name) ++ " to be a path"

exportTargetExists :: FilePath -> PackageExport -> IO Bool
exportTargetExists sdkPackageDir PackageExport {exportTarget} =
  if "*" `isInfixOf` generatedSourcePath
    then doesDirectoryExist $ sdkPackageDir </> wildcardParentDir generatedSourcePath
    else or <$> mapM (doesFileExist . (sdkPackageDir </>)) (generatedSourcePathCandidates generatedSourcePath)
  where
    generatedSourcePath = exportTargetToGeneratedSourcePath exportTarget

formatPackageExport :: PackageExport -> String
formatPackageExport PackageExport {exportName, exportTarget} = exportName ++ " -> " ++ exportTarget

exportTargetToGeneratedSourcePath :: String -> FilePath
exportTargetToGeneratedSourcePath = dropPrefix "dist/" . dropPrefix "./"

generatedSourcePathCandidates :: FilePath -> [FilePath]
generatedSourcePathCandidates target
  | ".jsx" `isSuffixOf` target = [target, replaceSuffix ".jsx" ".tsx" target]
  | ".js" `isSuffixOf` target = [target, replaceSuffix ".js" ".ts" target]
  | otherwise = [target]

wildcardParentDir :: FilePath -> FilePath
wildcardParentDir target = dropTrailingPathSeparator $ takeWhile (/= '*') target

dropPrefix :: String -> String -> String
dropPrefix prefix value = fromMaybe value $ stripPrefix prefix value

replaceSuffix :: String -> String -> String -> String
replaceSuffix oldSuffix newSuffix value =
  take (length value - length oldSuffix) value ++ newSuffix
