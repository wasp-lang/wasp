module Tests.SnapshotTests.KitchenSinkSnapshotTest (kitchenSinkSnapshotTest) where

import Control.Monad (filterM, unless)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as AesonKey
import qualified Data.Aeson.KeyMap as AesonKeyMap
import Data.List (isInfixOf, isSuffixOf, stripPrefix)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import E2e
import System.Directory (doesDirectoryExist, doesFileExist)
import qualified System.FilePath as FP

kitchenSinkSnapshotTest :: E2eTest
kitchenSinkSnapshotTest =
  snapshotTest "kitchen-sink" $ do
    copyGitTrackedDir "examples/kitchen-sink" waspAppDirName
    inWaspProject $ do
      copyFile ".env.server.example" ".env.server"
      -- Normalizes the Postgres connection string.
      appendToFile ".env.server" "\nDATABASE_URL=mock-database-url"
      wasp ["install"]
      wasp ["compile"]
      assertSdkPackageExportsExist

-- | Checks that every export in the generated SDK's package.json points at a
-- generated source file.
assertSdkPackageExportsExist :: TestM ()
assertSdkPackageExportsExist = do
  sdkPackageDir <- resolvePath ".wasp/out/sdk/wasp"
  packageJson <- readFromFile ".wasp/out/sdk/wasp/package.json" >>= assertValidJson
  packageExports <- case packageJson of
    Aeson.Object packageJsonObject
      | Just (Aeson.Object exportsObject) <- AesonKeyMap.lookup "exports" packageJsonObject ->
          return [(AesonKey.toString name, T.unpack target) | (name, Aeson.String target) <- AesonKeyMap.toList exportsObject]
    _ -> failTest "Expected the SDK package.json to define an exports object."
  brokenExports <- liftIO $ filterM (fmap not . exportTargetExists sdkPackageDir . snd) packageExports
  unless (null brokenExports)
    $ failTest
    $ "Broken SDK package exports:\n" ++ unlines [name ++ " -> " ++ target | (name, target) <- brokenExports]

exportTargetExists :: FilePath -> String -> IO Bool
exportTargetExists sdkPackageDir exportTarget
  | "*" `isInfixOf` sourcePath = doesDirectoryExist $ sdkPackageDir FP.</> wildcardParentDir sourcePath
  | otherwise = or <$> mapM (doesFileExist . (sdkPackageDir FP.</>)) (sourcePathCandidates sourcePath)
  where
    sourcePath = dropPrefix "dist/" . dropPrefix "./" $ exportTarget

    wildcardParentDir = FP.dropTrailingPathSeparator . takeWhile (/= '*')

    sourcePathCandidates path
      | ".jsx" `isSuffixOf` path = [path, replaceSuffix ".jsx" ".tsx" path]
      | ".js" `isSuffixOf` path = [path, replaceSuffix ".js" ".ts" path]
      | otherwise = [path]

    dropPrefix :: String -> String -> String
    dropPrefix prefix value = fromMaybe value $ stripPrefix prefix value

    replaceSuffix :: String -> String -> String -> String
    replaceSuffix oldSuffix newSuffix value = take (length value - length oldSuffix) value ++ newSuffix
