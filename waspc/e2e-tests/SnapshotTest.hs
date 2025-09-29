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
    getSnapshotsDir,
    snapshotDirInSnapshotsDir,
    snapshotFileListManifestFileInSnapshotDir,
    snapshotWaspAppDirInSnapshotDir,
  )
import SnapshotTest.ShellCommands (SnapshotTestContext (..))
import StrongPath (Abs, Dir, File, Path', (</>))
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
  snapshotsDir <- getSnapshotsDir
  let snapshotTestName = _snapshotTestName snapshotTest

  let goldenSnapshotDir = snapshotsDir </> snapshotDirInSnapshotsDir snapshotTestName Golden
  let currentSnapshotDir = snapshotsDir </> snapshotDirInSnapshotsDir snapshotTestName Current
  let currentSnapshotFileListManifestFile = currentSnapshotDir </> snapshotFileListManifestFileInSnapshotDir

  setupSnapshotTestEnvironment currentSnapshotDir goldenSnapshotDir
  executeSnapshotTestCommand (_snapshotTestCommandsBuilder snapshotTest) (createSnapshotTestContext currentSnapshotDir)

  getFilesForSnapshotFileListManifest currentSnapshotDir >>= createSnapshotFileListManifest currentSnapshotDir currentSnapshotFileListManifestFile

  currentSnapshotFilesForContentComparison <- getFilesForSnapshotContentComparison currentSnapshotDir
  formatPackageJsonFiles currentSnapshotFilesForContentComparison

  return $
    testGroup
      snapshotTestName
      [ defineSnapshotTest currentSnapshotFile (goldenSnapshotDir </> currentSnapshotFileFromCurrentSnapshotDir)
        | currentSnapshotFile <- currentSnapshotFilesForContentComparison,
          let currentSnapshotFileFromCurrentSnapshotDir = fromJust $ SP.parseRelFile $ makeRelative (SP.fromAbsDir currentSnapshotDir) (SP.fromAbsFile currentSnapshotFile)
      ]

-- | Sets up the snapshot test environment by:
-- 1. Removing any existing files in the current snapshot directory from a prior test run.
-- 2. Ensuring the current and golden snapshot directories exist.
setupSnapshotTestEnvironment :: Path' Abs (Dir SnapshotDir) -> Path' Abs (Dir SnapshotDir) -> IO ()
setupSnapshotTestEnvironment currentSnapshotDir goldenSnapshotDir = do
  callCommand $ "rm -rf " ++ SP.fromAbsDir currentSnapshotDir

  callCommand $ "mkdir " ++ SP.fromAbsDir currentSnapshotDir
  callCommand $ "mkdir -p " ++ SP.fromAbsDir goldenSnapshotDir

executeSnapshotTestCommand ::
  ShellCommandBuilder SnapshotTestContext [ShellCommand] ->
  SnapshotTestContext ->
  IO ()
executeSnapshotTestCommand snapshotTestCommandBuilder snapshotTestContext = do
  putStrLn $ "Running the following command: " ++ snapshotTestCommand
  callCommand $ cdIntoCurrentSnapshotDirCommand ~&& snapshotTestCommand
  where
    snapshotTestCommand :: ShellCommand
    snapshotTestCommand = foldr1 (~&&) $ buildShellCommand snapshotTestContext snapshotTestCommandBuilder

    cdIntoCurrentSnapshotDirCommand :: ShellCommand
    cdIntoCurrentSnapshotDirCommand = "cd " ++ SP.fromAbsDir (_snapshotDir snapshotTestContext)

createSnapshotTestContext :: Path' Abs (Dir SnapshotDir) -> SnapshotTestContext
createSnapshotTestContext currentSnapshotDir =
  SnapshotTestContext
    { _snapshotDir = currentSnapshotDir,
      _snapshotWaspAppDirInSnapshotDir = snapshotWaspAppDirInSnapshotDir "wasp-app",
      _snapshotWaspAppName = "wasp-app"
    }

getFilesForSnapshotFileListManifest :: Path' Abs (Dir SnapshotDir) -> IO [Path' Abs (File SnapshotFile)]
getFilesForSnapshotFileListManifest snapshotDir =
  getDirFiltered (return . flip notElem ignoredFiles . takeFileName) (SP.fromAbsDir snapshotDir)
    >>= filterM doesFileExist
    >>= mapM SP.parseAbsFile
  where
    ignoredFiles =
      [ ".DS_Store",
        "node_modules"
      ]

-- | Creates a deterministic manifest of files that should exist in the snapshot.
-- File paths are normalized to relative paths and sorted.
createSnapshotFileListManifest :: Path' Abs (Dir SnapshotDir) -> Path' Abs (File SnapshotFileListManifestFile) -> [Path' Abs (File SnapshotFile)] -> IO ()
createSnapshotFileListManifest snapshotDir snapshotFileListManifestFile = do
  writeFile (SP.fromAbsFile snapshotFileListManifestFile) . unlines . sort . map (makeRelative (SP.fromAbsDir snapshotDir) . SP.fromAbsFile)

getFilesForSnapshotContentComparison :: Path' Abs (Dir SnapshotDir) -> IO [Path' Abs (File SnapshotFile)]
getFilesForSnapshotContentComparison snapshotDir = do
  getDirFiltered (return . flip notElem ignoredFiles . takeFileName) (SP.fromAbsDir snapshotDir)
    >>= filterM doesFileExist
    >>= mapM SP.parseAbsFile
  where
    ignoredFiles =
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

-- | Normalizes @package.json@ files into deterministic format for snapshot comparison.
-- Ref: https://github.com/wasp-lang/wasp/issues/482
formatPackageJsonFiles :: [Path' Abs (File file)] -> IO ()
formatPackageJsonFiles = mapM_ formatPackageJsonFile . filter isPackageJsonFile
  where
    formatPackageJsonFile :: Path' Abs (File file) -> IO ()
    formatPackageJsonFile packageJsonFile = do
      BS.readFile (SP.fromAbsFile packageJsonFile) >>= BSL.writeFile (SP.fromAbsFile packageJsonFile) . formatJson . unsafeDecodeJson

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

defineSnapshotTest :: Path' Abs (File SnapshotFile) -> Path' Abs (File SnapshotFile) -> TestTree
defineSnapshotTest currentSnapshotFile goldenSnapshotFile =
  goldenVsFileDiff
    (SP.fromAbsFile currentSnapshotFile) -- The test name.
    (\ref new -> ["diff", "-u", ref, new])
    (SP.fromAbsFile goldenSnapshotFile)
    (SP.fromAbsFile currentSnapshotFile)
    (return ()) -- IO action to generate the current file. No-op since files are pre-generated by the snapshot test command.
