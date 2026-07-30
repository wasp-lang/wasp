module Tests.SdkPackageExportsTest (assertSdkPackageExports) where

import Context (WaspProjectContext (..))
import Control.Monad (filterM, unless)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ask)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as AesonKey
import qualified Data.Aeson.KeyMap as AesonKeyMap
import qualified Data.ByteString as BS
import Data.List (isInfixOf, isSuffixOf, stripPrefix)
import qualified Data.Text as T
import qualified StrongPath as SP
import System.Directory (doesDirectoryExist, doesFileExist)
import qualified System.FilePath as FP
import Test.Tasty.HUnit (assertFailure)
import TestAction (TestAction)

-- | Asserts that the SDK package exports point at generated source files.
assertSdkPackageExports :: TestAction WaspProjectContext ()
assertSdkPackageExports = do
  context <- ask
  liftIO $ do
    let sdkPackageDir = getGeneratedSdkPackageDir context
    packageExports <- readPackageExports sdkPackageDir
    missingExportTargets <- filterM (fmap not . exportTargetExists sdkPackageDir) packageExports

    unless (null missingExportTargets) $
      assertFailure $
        "Broken SDK package exports:\n" ++ unlines (map formatPackageExport missingExportTargets)

data PackageExport = PackageExport
  { exportName :: String,
    exportTarget :: String
  }

getGeneratedSdkPackageDir :: WaspProjectContext -> FilePath
getGeneratedSdkPackageDir context =
  SP.fromAbsDir context.waspProjectDir
    FP.</> ".wasp"
    FP.</> "out"
    FP.</> "sdk"
    FP.</> "wasp"

readPackageExports :: FilePath -> IO [PackageExport]
readPackageExports sdkPackageDir = do
  packageJson <- BS.readFile $ sdkPackageDir FP.</> "package.json"
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
    then doesDirectoryExist $ sdkPackageDir FP.</> wildcardParentDir generatedSourcePath
    else or <$> mapM (doesFileExist . (sdkPackageDir FP.</>)) (generatedSourcePathCandidates generatedSourcePath)
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
dropPrefix prefix value = maybe value id $ stripPrefix prefix value

replaceSuffix :: String -> String -> String -> String
replaceSuffix oldSuffix newSuffix value =
  take (length value - length oldSuffix) value ++ newSuffix
