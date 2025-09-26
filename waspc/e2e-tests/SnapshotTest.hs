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
  ( SnapshotType (..),
    getSnapshotsDir,
    snapshotDirInSnapshotsDir,
    snapshotFileListManifestFileInSnapshotDir,
    snapshotWaspAppDirInSnapshotDir,
  )
import SnapshotTest.ShellCommands (SnapshotTestContext (..))
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
  let snapshotTestName = _snapshotTestName snapshotTest
  snapshotsAbsDir <- getSnapshotsDir

  let goldenSnapshotDir = snapshotsAbsDir SP.</> snapshotDirInSnapshotsDir snapshotTestName Golden
  let currentSnapshotDir = snapshotsAbsDir SP.</> snapshotDirInSnapshotsDir snapshotTestName Current
  let currentSnapshotFileListManifestFile = currentSnapshotDir SP.</> snapshotFileListManifestFileInSnapshotDir

  -- Remove existing current snapshot files from a prior test run.
  callCommand $ "rm -rf " ++ SP.fromAbsDir currentSnapshotDir ++ "/*"

  -- Create current snapshot dir as well as the golden snapshot dir, if missing.
  callCommand $ "mkdir " ++ SP.fromAbsDir currentSnapshotDir ++ "/*"
  callCommand $ "mkdir -p " ++ SP.fromAbsDir goldenSnapshotDir

  let snapshotTestContext =
        SnapshotTestContext
          { _snapshotDir = currentSnapshotDir,
            _snapshotWaspAppDirInSnapshotDir = snapshotWaspAppDirInSnapshotDir "wasp-app",
            _snapshotWaspAppName = "wasp-app"
          }
  let snapshotTestCommand = foldr1 (~&&) $ buildShellCommand snapshotTestContext (_snapshotTestCommandsBuilder snapshotTest)
  let cdIntoCurrentSnapshotDirCommand = "cd " ++ SP.fromAbsDir currentSnapshotDir ++ "/*"

  putStrLn $ "Running the following command: " ++ snapshotTestCommand
  -- TODO: Save stdout/error as log file for "contains" checks.
  callCommand $ cdIntoCurrentSnapshotDirCommand ~&& snapshotTestCommand

  filesForCheckingExistenceAbsFps <- getFilesForCheckingExistence $ SP.fromAbsDir currentSnapshotDir ++ "/*"
  filesForCheckingContentAbsFps <- (SP.fromAbsFile currentSnapshotFileListManifestFile :) <$> getFilesForCheckingContent (SP.fromAbsDir currentSnapshotDir ++ "/*")

  writeSnapshotFileListManifest (SP.fromAbsDir currentSnapshotDir ++ "/*") filesForCheckingExistenceAbsFps (SP.fromAbsFile currentSnapshotFileListManifestFile)
  reformatPackageJsonFiles filesForCheckingContentAbsFps

  let remapCurrentToGoldenFp fp = unpack $ replace (pack $ SP.fromAbsDir currentSnapshotDir ++ "/*") (pack $ SP.fromAbsDir goldenSnapshotDir) (pack fp)

  return $
    testGroup
      snapshotTestName
      [defineSnapshotTest currentFp (remapCurrentToGoldenFp currentFp) | currentFp <- filesForCheckingContentAbsFps]
  where
    getFilesForCheckingExistence :: FilePath -> IO [FilePath]
    getFilesForCheckingExistence dirToFilterAbsFp =
      let shouldCheckFileExistence fp =
            takeFileName fp
              `notElem` [ ".DS_Store",
                          "node_modules"
                        ]
       in getDirFiltered (return <$> shouldCheckFileExistence) dirToFilterAbsFp >>= filterM doesFileExist

    getFilesForCheckingContent :: FilePath -> IO [FilePath]
    getFilesForCheckingContent dirToFilterAbsFp =
      let shouldCheckFileContents fp =
            takeFileName fp
              `notElem` [ ".DS_Store",
                          "dev.db",
                          "dev.db-journal",
                          ".gitignore",
                          ".waspinfo",
                          "package-lock.json",
                          "node_modules",
                          "tsconfig.tsbuildinfo",
                          "dist"
                        ]
       in getDirFiltered (return <$> shouldCheckFileContents) dirToFilterAbsFp >>= filterM doesFileExist

    -- Writes a deterministic manifest of files that should exist in the snapshot.
    -- File paths are normalized to relative paths and sorted.
    writeSnapshotFileListManifest :: String -> [FilePath] -> FilePath -> IO ()
    writeSnapshotFileListManifest baseAbsFp filesForCheckingExistenceAbsFps expectedFilesListAbsFp = do
      let sortedRelativeFps = unlines . sort . map (makeRelative baseAbsFp) $ filesForCheckingExistenceAbsFps
      writeFile expectedFilesListAbsFp sortedRelativeFps

    -- Normalizes @package.json@ files into deterministic format for snapshot comparison.
    -- Ref: https://github.com/wasp-lang/wasp/issues/482
    reformatPackageJsonFiles :: [FilePath] -> IO ()
    reformatPackageJsonFiles = mapM_ reformatJson . filter isPackageJson
      where
        isPackageJson :: FilePath -> Bool
        isPackageJson = equalFilePath "package.json" . takeFileName

        reformatJson :: FilePath -> IO ()
        reformatJson jsonFp =
          BS.readFile jsonFp >>= BSL.writeFile jsonFp . formatJson . unsafeDecodeJson

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

    defineSnapshotTest :: FilePath -> FilePath -> TestTree
    defineSnapshotTest currentFileAbsFp goldenFileAbsFp =
      goldenVsFileDiff
        currentFileAbsFp -- The test name.
        (\ref new -> ["diff", "-u", ref, new])
        goldenFileAbsFp
        currentFileAbsFp
        (return ()) -- IO action to generate the current file. No-op since files are pre-generated in bulk.
