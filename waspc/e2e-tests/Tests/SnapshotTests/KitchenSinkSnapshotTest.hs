module Tests.SnapshotTests.KitchenSinkSnapshotTest (kitchenSinkSnapshotTest) where

import Control.Monad (filterM, unless)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as AesonKey
import qualified Data.Aeson.KeyMap as AesonKeyMap
import qualified Data.ByteString as BS
import Data.List (isInfixOf, isSuffixOf, stripPrefix)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import E2e
import System.Directory (doesDirectoryExist, doesFileExist)
import System.FilePath ((</>))
import qualified System.FilePath as FP

kitchenSinkSnapshotTest :: E2eTest
kitchenSinkSnapshotTest =
  snapshotTest "kitchen-sink" $ do
    copyGitTrackedDir "examples/kitchen-sink" waspAppDirName
    inDirectory waspAppDirName $ do
      copyFile ".env.server.example" ".env.server"
      appendToFile ".env.server" "\nDATABASE_URL=mock-database-url"
      wasp ["install"]
      wasp ["compile"]
      assertSdkPackageExportsExist

-- | Checks that every export of the generated SDK package points at one of its
-- generated source files.
assertSdkPackageExportsExist :: TestM ()
assertSdkPackageExportsExist = do
  sdkPackageDir <- resolvePath ".wasp/out/sdk/wasp"
  missingExportTargets <- liftIO $ do
    packageExports <- readPackageExports sdkPackageDir
    filterM (fmap not . exportTargetExists sdkPackageDir) packageExports
  unless (null missingExportTargets)
    $ failTest
    $ "Broken SDK package exports:\n" ++ unlines (map formatPackageExport missingExportTargets)

data PackageExport = PackageExport
  { exportName :: String,
    exportTarget :: String
  }

readPackageExports :: FilePath -> IO [PackageExport]
readPackageExports sdkPackageDir = do
  packageJson <- BS.readFile $ sdkPackageDir </> "package.json"
  case Aeson.eitherDecodeStrict packageJson of
    Left err -> fail $ "Failed to parse SDK package.json: " ++ err
    Right (Aeson.Object packageJsonObject) -> do
      case AesonKeyMap.lookup "exports" packageJsonObject of
        Just (Aeson.Object exportsObject) -> return $ collectStringExports $ AesonKeyMap.toList exportsObject
        Just _ -> fail "Expected SDK package.json#exports to be an object"
        Nothing -> fail "Expected SDK package.json to define exports"
    Right _ -> fail "Expected SDK package.json to be an object"

exportTargetExists :: FilePath -> PackageExport -> IO Bool
exportTargetExists sdkPackageDir PackageExport {exportTarget} =
  if "*" `isInfixOf` generatedSourcePath
    then doesDirectoryExist $ sdkPackageDir </> wildcardParentDir generatedSourcePath
    else or <$> mapM (doesFileExist . (sdkPackageDir </>)) (generatedSourcePathCandidates generatedSourcePath)
  where
    generatedSourcePath = exportTargetToGeneratedSourcePath exportTarget

formatPackageExport :: PackageExport -> String
formatPackageExport PackageExport {exportName, exportTarget} = exportName ++ " -> " ++ exportTarget

collectStringExports :: [(AesonKey.Key, Aeson.Value)] -> [PackageExport]
collectStringExports = foldr go []
  where
    go (name, Aeson.String target) acc =
      PackageExport (AesonKey.toString name) (T.unpack target) : acc
    go _ acc = acc

exportTargetToGeneratedSourcePath :: String -> FilePath
exportTargetToGeneratedSourcePath = dropPrefix "dist/" . dropPrefix "./"

generatedSourcePathCandidates :: FilePath -> [FilePath]
generatedSourcePathCandidates target
  | ".jsx" `isSuffixOf` target = [target, replaceSuffix ".jsx" ".tsx" target]
  | ".js" `isSuffixOf` target = [target, replaceSuffix ".js" ".ts" target]
  | otherwise = [target]

wildcardParentDir :: FilePath -> FilePath
wildcardParentDir target = FP.dropTrailingPathSeparator $ takeWhile (/= '*') target

dropPrefix :: String -> String -> String
dropPrefix prefix value = fromMaybe value $ stripPrefix prefix value

replaceSuffix :: String -> String -> String -> String
replaceSuffix oldSuffix newSuffix value =
  take (length value - length oldSuffix) value ++ newSuffix
