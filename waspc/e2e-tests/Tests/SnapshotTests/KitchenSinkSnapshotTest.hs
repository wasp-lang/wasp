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
import E2E
import qualified StrongPath as SP
import System.Directory (doesDirectoryExist, doesFileExist)
import System.FilePath (dropTrailingPathSeparator, (</>))
import Wasp.Generator.SdkGenerator.Common (sdkRootDirInGeneratedAppDir)
import Wasp.Project.Common (generatedAppDirInWaspProjectDir)
import Wasp.Project.Db (databaseUrlEnvVarName)

kitchenSinkSnapshotTest :: E2ETest
kitchenSinkSnapshotTest =
  snapshotTest "kitchen-sink" $ do
    copyGitTrackedDir "examples/kitchen-sink" waspAppDirName
    inDirectory waspAppDirName $ do
      copyFile ".env.server.example" ".env.server"
      appendToFile ".env.server" ("\n" <> T.pack databaseUrlEnvVarName <> "=mock-database-url")
      wasp ["install"]
      wasp ["compile"]
      assertSdkPackageExportsExist

-- | Checks that every export of the generated SDK package points at one of its
-- generated source files. Runs in the Wasp project, after it's compiled.
assertSdkPackageExportsExist :: TestM ()
assertSdkPackageExportsExist = do
  sdkPackageDir <- resolvePath . SP.fromRelDir $ generatedAppDirInWaspProjectDir SP.</> sdkRootDirInGeneratedAppDir
  let packageJsonFile = sdkPackageDir </> "package.json"
  packageJson <- liftIO $ BS.readFile packageJsonFile
  packageExports <- either (failTest . (("Unexpected " ++ packageJsonFile ++ ": ") ++)) return $ parsePackageExports packageJson
  missingExportTargets <- liftIO $ filterM (fmap not . exportTargetExists sdkPackageDir) packageExports
  unless (null missingExportTargets)
    $ failTest
    $ "Broken SDK package exports:\n"
      ++ unlines [exportName ++ " -> " ++ exportTarget | PackageExport {exportName, exportTarget} <- missingExportTargets]

data PackageExport = PackageExport
  { exportName :: String,
    exportTarget :: String
  }

-- | Fails on exports that aren't a plain path (e.g. conditional exports), so
-- that the check doesn't skip them: it would need to learn about them first.
-- Fails on an empty @exports@ too, since there would be nothing to check.
parsePackageExports :: BS.ByteString -> Either String [PackageExport]
parsePackageExports packageJson =
  Aeson.eitherDecodeStrict packageJson >>= \case
    Aeson.Object packageJsonObject -> case AesonKeyMap.lookup "exports" packageJsonObject of
      Just (Aeson.Object exportsObject)
        | AesonKeyMap.null exportsObject -> Left "expected \"exports\" to have at least one export"
        | otherwise -> mapM toPackageExport $ AesonKeyMap.toList exportsObject
      Just _ -> Left "expected \"exports\" to be an object"
      Nothing -> Left "expected it to define \"exports\""
    _ -> Left "expected it to be an object"
  where
    toPackageExport (name, Aeson.String target) = Right $ PackageExport (AesonKey.toString name) (T.unpack target)
    toPackageExport (name, _) = Left $ "expected the export " ++ show (AesonKey.toString name) ++ " to be a path"

exportTargetExists :: FilePath -> PackageExport -> IO Bool
exportTargetExists sdkPackageDir PackageExport {exportTarget} =
  if "*" `isInfixOf` generatedSourcePath
    then doesDirectoryExist $ sdkPackageDir </> wildcardParentDir
    else or <$> mapM (doesFileExist . (sdkPackageDir </>)) (generatedSourcePathCandidates generatedSourcePath)
  where
    -- Exports point at the built files in `dist/`. The sources they're built
    -- from sit at the same path in the package (see
    -- 'generatedSourcePathCandidates').
    generatedSourcePath = dropPrefix "dist/" $ dropPrefix "./" exportTarget
    -- A wildcard export (e.g. @./foo/*@) is checked by its directory.
    wildcardParentDir = dropTrailingPathSeparator $ takeWhile (/= '*') generatedSourcePath

-- | The source of a built @.js@ file can be JavaScript or TypeScript.
generatedSourcePathCandidates :: FilePath -> [FilePath]
generatedSourcePathCandidates target
  | ".jsx" `isSuffixOf` target = [target, replaceSuffix ".jsx" ".tsx" target]
  | ".js" `isSuffixOf` target = [target, replaceSuffix ".js" ".ts" target]
  | otherwise = [target]

dropPrefix :: String -> String -> String
dropPrefix prefix value = fromMaybe value $ stripPrefix prefix value

replaceSuffix :: String -> String -> String -> String
replaceSuffix oldSuffix newSuffix value =
  take (length value - length oldSuffix) value ++ newSuffix
