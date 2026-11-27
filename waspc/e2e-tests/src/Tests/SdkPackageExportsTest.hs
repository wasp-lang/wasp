{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Tests.SdkPackageExportsTest (test_sdkPackageExports) where

import Control.Monad (filterM)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (FromJSON)
import Data.List (isInfixOf, isSuffixOf, stripPrefix)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import E2E
import GHC.Generics (Generic)
import qualified StrongPath as SP
import System.Directory (doesDirectoryExist, doesFileExist)
import System.FilePath (dropTrailingPathSeparator, (</>))
import Tests.Fixtures (withKitchenSinkProject)
import Wasp.Generator.SdkGenerator.Common (sdkRootDirInGeneratedAppDir)
import Wasp.Project.Common (generatedAppDirInWaspProjectDir)

-- | Checks that every export of the generated SDK package points at one of its
-- generated source files.
--
-- The SDK package always declares all of its exports, but only generates the
-- sources of the features the app uses, so the check needs an app that uses
-- all of them: kitchen-sink.
test_sdkPackageExports :: TestTree
test_sdkPackageExports =
  testGroup
    "sdk-package-exports"
    [ test "point-at-generated-source-files" $
        withKitchenSinkProject $ do
          wasp ["install"]
          wasp ["compile"]
          assertSdkPackageExportsExist
    ]

assertSdkPackageExportsExist :: TestM ()
assertSdkPackageExportsExist = do
  sdkPackageDir <- resolvePath . SP.fromRelDir $ generatedAppDirInWaspProjectDir SP.</> sdkRootDirInGeneratedAppDir
  packageJson :: SdkPackageJson <- readJsonFile $ sdkPackageDir </> "package.json"
  packageJson.exports `shouldNotBe` Map.empty -- Otherwise there would be nothing to check.
  brokenExports <- liftIO $ filterM (fmap not . exportTargetExists sdkPackageDir . snd) $ Map.toList packageJson.exports
  brokenExports `shouldBe` []

newtype SdkPackageJson = SdkPackageJson {exports :: Map.Map String FilePath}
  deriving (Generic, FromJSON)

exportTargetExists :: FilePath -> FilePath -> IO Bool
exportTargetExists sdkPackageDir exportTarget =
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
