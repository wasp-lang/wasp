{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}

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
import FileSystem
  ( SnapshotDir,
    SnapshotFile,
    SnapshotFileListManifestFile,
    SnapshotType (..),
    SnapshotsDir,
    getSnapshotsDir,
    snapshotDirInSnapshotsDir,
    snapshotFileListManifestFileInSnapshotDir,
  )
import ShellCommands (ShellCommand, ShellCommandBuilder, SnapshotTestContext (..), WaspProjectContext (..), buildShellCommand, (~&&))
import StrongPath (Abs, Dir, File, Path', parseRelDir, (</>))
import qualified StrongPath as SP
import System.Directory (doesFileExist)
import System.Directory.Recursive (getDirFiltered)
import System.FilePath (equalFilePath, isExtensionOf, makeRelative, takeFileName)
import System.Process (callCommand)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Golden (goldenVsFileDiff)

data SnapshotTest = SnapshotTest
  { name :: String,
    shellCommandBuilder :: ShellCommandBuilder SnapshotTestContext [ShellCommand]
  }

makeSnapshotTest :: String -> [ShellCommandBuilder SnapshotTestContext ShellCommand] -> SnapshotTest
makeSnapshotTest name shellCommandBuilders =
  SnapshotTest
    { name,
      shellCommandBuilder = sequence shellCommandBuilders
    }

-- | Runs a 'SnapshotTest' by executing snapshot test's shell commands and then
--  comparing the generated files to the previous "golden" (expected) version of those files.
runSnapshotTest :: SnapshotTest -> IO TestTree
runSnapshotTest snapshotTest = getSnapshotsDir >>= executeSnapshotTestWorkflow
  where
    executeSnapshotTestWorkflow :: Path' Abs (Dir SnapshotsDir) -> IO TestTree
    executeSnapshotTestWorkflow snapshotsDir = do
      setupSnapshotTestEnvironment currentSnapshotDir goldenSnapshotDir
      executeSnapshotTestCommand snapshotTest currentSnapshotDir
      generateSnapshotFileListManifest currentSnapshotDir currentSnapshotFileListManifestFile
      currentSnapshotFilesForContentCheck <- getNormalizedSnapshotFilesForContentCheck currentSnapshotDir
      return $
        testGroup
          snapshotTest.name
          (defineSnapshotTestCases currentSnapshotDir goldenSnapshotDir currentSnapshotFilesForContentCheck)
      where
        goldenSnapshotDir = snapshotsDir </> snapshotDirInSnapshotsDir snapshotTest.name Golden
        currentSnapshotDir = snapshotsDir </> snapshotDirInSnapshotsDir snapshotTest.name Current
        currentSnapshotFileListManifestFile = currentSnapshotDir </> snapshotFileListManifestFileInSnapshotDir

-- | Sets up the snapshot test environment by:
-- 1. Removing any existing files in the current snapshot directory from a prior test run.
-- 2. Ensuring the current and golden snapshot directories exist.
setupSnapshotTestEnvironment :: Path' Abs (Dir SnapshotDir) -> Path' Abs (Dir SnapshotDir) -> IO ()
setupSnapshotTestEnvironment currentSnapshotDir goldenSnapshotDir = do
  callCommand $ "rm -rf " ++ SP.fromAbsDir currentSnapshotDir

  callCommand $ "mkdir " ++ SP.fromAbsDir currentSnapshotDir
  callCommand $ "mkdir -p " ++ SP.fromAbsDir goldenSnapshotDir

executeSnapshotTestCommand :: SnapshotTest -> Path' Abs (Dir SnapshotDir) -> IO ()
executeSnapshotTestCommand snapshotTest snapshotDir = do
  putStrLn $ "Executing snapshot test: " ++ snapshotTest.name
  putStrLn $ "Running the following command: " ++ snapshotTestCommand
  callCommand $ "cd " ++ SP.fromAbsDir snapshotDir ~&& snapshotTestCommand
  where
    snapshotTestCommand :: ShellCommand
    snapshotTestCommand = foldr1 (~&&) $ buildShellCommand snapshotTestContext snapshotTest.shellCommandBuilder

    snapshotTestContext :: SnapshotTestContext
    snapshotTestContext = SnapshotTestContext {snapshotDir, waspProjectContext}

    waspProjectContext :: WaspProjectContext
    waspProjectContext =
      WaspProjectContext
        { waspProjectDir = snapshotDir </> (fromJust . parseRelDir $ "wasp-app"),
          waspProjectName = "wasp-app"
        }

generateSnapshotFileListManifest :: Path' Abs (Dir SnapshotDir) -> Path' Abs (File SnapshotFileListManifestFile) -> IO ()
generateSnapshotFileListManifest snapshotDir snapshotFileListManifestFile =
  getSnapshotFilesForExistenceCheck >>= writeSnapshotFileListManifest
  where
    getSnapshotFilesForExistenceCheck :: IO [Path' Abs (File SnapshotFile)]
    getSnapshotFilesForExistenceCheck =
      getDirFiltered (return . filterIgnoredFileNames) (SP.fromAbsDir snapshotDir)
        >>= filterM doesFileExist -- only files, no directories
        >>= mapM SP.parseAbsFile
      where
        filterIgnoredFileNames = createFilenameFilter [flip notElem ignoredFileNames, not . isTgzFile]
        ignoredFileNames =
          [ ".DS_Store",
            "node_modules"
          ]

    -- Creates a deterministic manifest of files that should exist in the snapshot.
    -- File paths are normalized to relative paths and sorted.
    writeSnapshotFileListManifest :: [Path' Abs (File SnapshotFile)] -> IO ()
    writeSnapshotFileListManifest =
      writeFile (SP.fromAbsFile snapshotFileListManifestFile) . unlines . sort . map normalizeFile
      where
        normalizeFile = makeRelative (SP.fromAbsDir snapshotDir) . SP.fromAbsFile

getNormalizedSnapshotFilesForContentCheck :: Path' Abs (Dir SnapshotDir) -> IO [Path' Abs (File SnapshotFile)]
getNormalizedSnapshotFilesForContentCheck snapshotDir = do
  snapshotFiles <- getSnapshotFilesForContentCheck
  formatPackageJsonFiles snapshotFiles
  return snapshotFiles
  where
    getSnapshotFilesForContentCheck :: IO [Path' Abs (File SnapshotFile)]
    getSnapshotFilesForContentCheck =
      getDirFiltered (return . filterIgnoredFileNames) (SP.fromAbsDir snapshotDir)
        >>= filterM doesFileExist -- only files, no directories
        >>= mapM SP.parseAbsFile
      where
        filterIgnoredFileNames = createFilenameFilter [flip notElem ignoredFileNames, not . isTgzFile]
        ignoredFileNames =
          [ ".DS_Store",
            "node_modules",
            "dev.db",
            "dev.db-journal",
            ".gitignore",
            ".waspinfo",
            "package-lock.json",
            "tsconfig.tsbuildinfo",
            "dist"
          ]

    -- Normalizes @package.json@ files into deterministic format for snapshot comparison.
    -- Ref: https://github.com/wasp-lang/wasp/issues/482
    formatPackageJsonFiles :: [Path' Abs (File file)] -> IO ()
    formatPackageJsonFiles = mapM_ formatPackageJsonFile . filter isPackageJsonFile
      where
        formatPackageJsonFile :: Path' Abs (File file) -> IO ()
        formatPackageJsonFile packageJsonFile =
          BS.readFile (SP.fromAbsFile packageJsonFile)
            >>= BSL.writeFile (SP.fromAbsFile packageJsonFile) . formatJson . unsafeDecodeJson

        isPackageJsonFile :: Path' Abs (File file) -> Bool
        isPackageJsonFile = equalFilePath "package.json" . takeFileName . SP.fromAbsFile

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

defineSnapshotTestCases ::
  Path' Abs (Dir SnapshotDir) ->
  Path' Abs (Dir SnapshotDir) ->
  [Path' Abs (File SnapshotFile)] ->
  [TestTree]
defineSnapshotTestCases currentSnapshotDir goldenSnapshotDir currentSnapshotFiles =
  map defineSnapshotTestCase currentSnapshotFiles
  where
    defineSnapshotTestCase :: Path' Abs (File SnapshotFile) -> TestTree
    defineSnapshotTestCase currentSnapshotFile =
      goldenVsFileDiff
        (SP.fromAbsFile currentSnapshotFile)
        (\goldenFilePath currentFilePath -> ["diff", "-u", goldenFilePath, currentFilePath])
        (SP.fromAbsFile $ mapCurrentToGoldenSnapshotFile currentSnapshotFile)
        (SP.fromAbsFile currentSnapshotFile)
        (return ())

    mapCurrentToGoldenSnapshotFile :: Path' Abs (File SnapshotFile) -> Path' Abs (File SnapshotFile)
    mapCurrentToGoldenSnapshotFile currentSnapshotFile =
      goldenSnapshotDir </> fromJust (SP.parseRelFile $ makeRelative (SP.fromAbsDir currentSnapshotDir) (SP.fromAbsFile currentSnapshotFile))

isTgzFile :: FilePath -> Bool
isTgzFile = (".tgz" `isExtensionOf`)

type FileName = String

createFilenameFilter :: [FileName -> Bool] -> FilePath -> Bool
createFilenameFilter predicates filePath = all ($ takeFileName filePath) predicates
