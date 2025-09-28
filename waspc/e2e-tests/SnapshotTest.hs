module SnapshotTest
  ( SnapshotTest,
    makeSnapshotTest,
    runSnapshotTest,
  )
where

import Control.Monad (filterM)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encode.Pretty as AesonPretty
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.Functor ((<&>))
import Data.List (sort)
import Data.Maybe (fromJust)
import Data.Text (pack, replace, unpack)
import ShellCommands
  ( ShellCommand,
    ShellCommandBuilder,
    buildShellCommand,
    (~&&),
  )
import SnapshotTest.FileSystem
  ( SnapshotDir,
    SnapshotFile,
    SnapshotFileListManifestFile,
    SnapshotType (..),
    asSnapshotFile,
    getSnapshotsDir,
    snapshotDirInSnapshotsDir,
    snapshotFileListManifestFileInSnapshotDir,
    snapshotWaspAppDirInSnapshotDir,
  )
import SnapshotTest.ShellCommands (SnapshotTestContext (..))
import StrongPath (Abs, Dir, File, Path')
import qualified StrongPath as SP
import System.Directory (doesFileExist)
import System.Directory.Recursive (getDirFiltered)
import System.FilePath (equalFilePath, makeRelative, takeFileName)
import System.Process (callCommand)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Golden (goldenVsFileDiff)

data SnapshotTest = SnapshotTest
  { _snapshotTestName :: String,
    _snapshotTestCommandsBuilder :: ShellCommandBuilder SnapshotTestContext [ShellCommand]
  }

makeSnapshotTest :: String -> [ShellCommandBuilder SnapshotTestContext ShellCommand] -> SnapshotTest
makeSnapshotTest snapshotTestName snapshotTestCommandBuilders =
  SnapshotTest
    { _snapshotTestName = snapshotTestName,
      _snapshotTestCommandsBuilder = sequence snapshotTestCommandBuilders
    }

-- | Runs a snapshot test by executing snapshot test's shell commands and then
--  comparing the generated files to the previous "golden" (expected) version of those files.
runSnapshotTest :: SnapshotTest -> IO TestTree
runSnapshotTest snapshotTest = do
  snapshotsAbsDir <- getSnapshotsDir

  let goldenSnapshotDir = snapshotsAbsDir SP.</> snapshotDirInSnapshotsDir (_snapshotTestName snapshotTest) Golden
  let currentSnapshotDir = snapshotsAbsDir SP.</> snapshotDirInSnapshotsDir (_snapshotTestName snapshotTest) Current
  let currentSnapshotFileListManifestFile = currentSnapshotDir SP.</> snapshotFileListManifestFileInSnapshotDir

  -- Remove existing current snapshot files from a prior test run.
  callCommand $ "rm -rf " ++ SP.fromAbsDir currentSnapshotDir

  -- Create current snapshot dir as well as the golden snapshot dir, if missing.
  callCommand $ "mkdir " ++ SP.fromAbsDir currentSnapshotDir
  callCommand $ "mkdir -p " ++ SP.fromAbsDir goldenSnapshotDir

  let snapshotTestContext =
        SnapshotTestContext
          { _snapshotDir = currentSnapshotDir,
            _snapshotWaspAppDirInSnapshotDir = snapshotWaspAppDirInSnapshotDir "wasp-app",
            _snapshotWaspAppName = "wasp-app"
          }
  let snapshotTestCommand = foldr1 (~&&) $ buildShellCommand snapshotTestContext (_snapshotTestCommandsBuilder snapshotTest)
  let cdIntoCurrentSnapshotDirCommand = "cd " ++ SP.fromAbsDir currentSnapshotDir

  putStrLn $ "Running the following command: " ++ snapshotTestCommand
  -- TODO: Save stdout/error as log file for "contains" checks.
  callCommand $ cdIntoCurrentSnapshotDirCommand ~&& snapshotTestCommand

  filesForCheckingExistenceAbsFps <- (asSnapshotFile currentSnapshotFileListManifestFile :) <$> getFilesForCheckingExistence currentSnapshotDir
  filesForCheckingContentAbsFps <- (asSnapshotFile currentSnapshotFileListManifestFile :) <$> getFilesForCheckingContent currentSnapshotDir

  writeSnapshotFileListManifest currentSnapshotDir currentSnapshotFileListManifestFile filesForCheckingExistenceAbsFps
  formatPackageJsonFiles filesForCheckingContentAbsFps

  let toGoldenSnapshotFile currentSnapshotFile = fromJust $ SP.parseAbsFile $ unpack $ replace (pack $ SP.fromAbsDir currentSnapshotDir) (pack $ SP.fromAbsDir goldenSnapshotDir) (pack $ SP.fromAbsFile currentSnapshotFile)

  return $
    testGroup
      (_snapshotTestName snapshotTest)
      [defineSnapshotTest currentSnapshotFile (toGoldenSnapshotFile currentSnapshotFile) | currentSnapshotFile <- filesForCheckingContentAbsFps]
  where
    getFilesForCheckingExistence :: Path' Abs (Dir SnapshotDir) -> IO [Path' Abs (File SnapshotFile)]
    getFilesForCheckingExistence snapshotDir =
      let shouldCheckFileExistence fp =
            takeFileName fp
              `notElem` [ ".DS_Store",
                          "node_modules"
                        ]
       in getDirFiltered (return <$> shouldCheckFileExistence) (SP.fromAbsDir snapshotDir) >>= filterM doesFileExist >>= mapM SP.parseAbsFile

    getFilesForCheckingContent :: Path' Abs (Dir SnapshotDir) -> IO [Path' Abs (File SnapshotFile)]
    getFilesForCheckingContent snapshotDir =
      let shouldCheckFileContents fp =
            takeFileName fp
              `notElem` [ ".DS_Store",
                          "node_modules",
                          "dev.db",
                          "dev.db-journal",
                          ".gitignore",
                          ".waspinfo",
                          "package-lock.json",
                          "tsconfig.tsbuildinfo",
                          "dist"
                        ]
       in getDirFiltered (return <$> shouldCheckFileContents) (SP.fromAbsDir snapshotDir) >>= filterM doesFileExist >>= mapM SP.parseAbsFile

    -- Writes a deterministic manifest of files that should exist in the snapshot.
    -- File paths are normalized to relative paths and sorted.
    writeSnapshotFileListManifest :: Path' Abs (Dir SnapshotDir) -> Path' Abs (File SnapshotFileListManifestFile) -> [Path' Abs (File SnapshotFile)] -> IO ()
    writeSnapshotFileListManifest snapshotDir snapshotFileListManifestFile filesForCheckingExistenceAbsFps =
      writeFile (SP.fromAbsFile snapshotFileListManifestFile) sortedSnapshotFilePathsRelativeToSnapshotDir
      where
        sortedSnapshotFilePathsRelativeToSnapshotDir = unlines . sort . map (makeRelative $ SP.fromAbsDir snapshotDir) $ filesForCheckingExistenceAbsFps <&> SP.fromAbsFile

    defineSnapshotTest :: Path' Abs (File SnapshotFile) -> Path' Abs (File SnapshotFile) -> TestTree
    defineSnapshotTest currentSnapshotFile goldenSnapshotFile =
      goldenVsFileDiff
        (SP.fromAbsFile currentSnapshotFile) -- The test name.
        (\ref new -> ["diff", "-u", ref, new])
        (SP.fromAbsFile goldenSnapshotFile)
        (SP.fromAbsFile currentSnapshotFile)
        (return ()) -- IO action to generate the current file. No-op since files are pre-generated in bulk.

-- Normalizes @package.json@ files into deterministic format for snapshot comparison.
-- Ref: https://github.com/wasp-lang/wasp/issues/482
formatPackageJsonFiles :: [Path' Abs (File file)] -> IO ()
formatPackageJsonFiles = mapM_ formatPackageJsonFile . filter isPackageJsonFile
  where
    formatPackageJsonFile :: Path' Abs (File file) -> IO ()
    formatPackageJsonFile packageJsonFile = do
      readJson packageJsonFile >>= writeJson packageJsonFile . formatJson . unsafeDecodeJson

    isPackageJsonFile :: Path' Abs (File file) -> Bool
    isPackageJsonFile = equalFilePath "package.json" . takeFileName . SP.fromAbsFile

    readJson :: Path' Abs (File file) -> IO BS.ByteString
    readJson = BS.readFile . SP.fromAbsFile

    unsafeDecodeJson :: BS.ByteString -> Aeson.Value
    unsafeDecodeJson = fromJust . Aeson.decodeStrict

    formatJson :: Aeson.Value -> BSL.ByteString
    formatJson =
      AesonPretty.encodePretty'
        AesonPretty.Config
          { AesonPretty.confIndent = AesonPretty.Spaces 2,
            AesonPretty.confCompare = compare,
            AesonPretty.confNumFormat = AesonPretty.Generic,
            AesonPretty.confTrailingNewline = True
          }

    writeJson :: Path' Abs (File file) -> BSL.ByteString -> IO ()
    writeJson snapshotPackageJsonFile = BSL.writeFile (SP.fromAbsFile snapshotPackageJsonFile)
