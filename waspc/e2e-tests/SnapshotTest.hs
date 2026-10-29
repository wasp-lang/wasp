{-# LANGUAGE DuplicateRecordFields #-}

module SnapshotTest
  ( SnapshotTest,
    makeSnapshotTest,
    testTreeFromSnapshotTest,
  )
where

import Control.Exception (bracket)
import Control.Monad (filterM)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encode.Pretty as AesonPretty
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.List (isInfixOf, sort)
import Data.Maybe (fromJust)
import qualified Data.Text as T
import FileSystem
  ( SnapshotDir,
    SnapshotFile,
    SnapshotFileListManifestFile,
    SnapshotType (..),
    TestLogFile,
    getSnapshotsDir,
    snapshotDirInSnapshotsDir,
    snapshotFileListManifestFileInSnapshotDir,
    snapshotLogFileInSnapshotsDir,
  )
import ShellCommands (ShellCommand, ShellCommandBuilder, SnapshotTestContext (..), WaspProjectContext (..), buildShellCommand, (~&&))
import StrongPath (Abs, Dir, File, Path', parseRelDir, (</>))
import qualified StrongPath as SP
import System.Directory (doesFileExist)
import System.Directory.Recursive (getDirFiltered)
import System.Exit (ExitCode (..))
import System.FilePath (equalFilePath, isExtensionOf, makeRelative, splitDirectories, takeFileName)
import System.Process (CreateProcess (..), StdStream (..), callCommand, createProcess, interruptProcessGroupOf, shell, waitForProcess)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Golden (goldenVsFileDiff)
import TestLogging (formatCommandFailure, openLogForCommand)

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

-- | Prepares a 'SnapshotTest' (executing shell commands and generating snapshots),
--  and then creates a test tree for comparing the generated files to the "golden" (expected) versions.
testTreeFromSnapshotTest :: SnapshotTest -> IO TestTree
testTreeFromSnapshotTest = fmap createSnapshotTestTree . prepareSnapshotTestData

-- | Data needed to create a snapshot test tree.
data SnapshotTestData = SnapshotTestData
  { name :: String,
    currentSnapshotDir :: Path' Abs (Dir SnapshotDir),
    goldenSnapshotDir :: Path' Abs (Dir SnapshotDir),
    currentSnapshotFilesForContentCheck :: [Path' Abs (File SnapshotFile)]
  }

prepareSnapshotTestData :: SnapshotTest -> IO SnapshotTestData
prepareSnapshotTestData snapshotTest = do
  snapshotsDir <- getSnapshotsDir
  let goldenSnapshotDir = snapshotsDir </> snapshotDirInSnapshotsDir snapshotTest.name Golden
      currentSnapshotDir = snapshotsDir </> snapshotDirInSnapshotsDir snapshotTest.name Current
      currentSnapshotFileListManifestFile = currentSnapshotDir </> snapshotFileListManifestFileInSnapshotDir
      logFile = snapshotsDir </> snapshotLogFileInSnapshotsDir snapshotTest.name

  setupSnapshotTestEnvironment currentSnapshotDir goldenSnapshotDir
  executeSnapshotTestCommand snapshotTest currentSnapshotDir logFile
  generateSnapshotFileListManifest currentSnapshotDir currentSnapshotFileListManifestFile
  currentSnapshotFilesForContentCheck <- getNormalizedSnapshotFilesForContentCheck currentSnapshotDir

  return
    SnapshotTestData
      { name = snapshotTest.name,
        currentSnapshotDir,
        goldenSnapshotDir,
        currentSnapshotFilesForContentCheck
      }

createSnapshotTestTree :: SnapshotTestData -> TestTree
createSnapshotTestTree snapshotTestData =
  testGroup
    snapshotTestData.name
    ( defineSnapshotTestCases
        snapshotTestData.currentSnapshotDir
        snapshotTestData.goldenSnapshotDir
        snapshotTestData.currentSnapshotFilesForContentCheck
    )

-- | Sets up the snapshot test environment by:
-- 1. Removing any existing files in the current snapshot directory from a prior test run.
-- 2. Ensuring the current and golden snapshot directories exist.
setupSnapshotTestEnvironment :: Path' Abs (Dir SnapshotDir) -> Path' Abs (Dir SnapshotDir) -> IO ()
setupSnapshotTestEnvironment currentSnapshotDir goldenSnapshotDir = do
  callCommand $ "rm -rf " ++ SP.fromAbsDir currentSnapshotDir

  callCommand $ "mkdir " ++ SP.fromAbsDir currentSnapshotDir
  callCommand $ "mkdir -p " ++ SP.fromAbsDir goldenSnapshotDir

executeSnapshotTestCommand :: SnapshotTest -> Path' Abs (Dir SnapshotDir) -> Path' Abs (File TestLogFile) -> IO ()
executeSnapshotTestCommand snapshotTest snapshotDir logFile =
  callCommandInProcessGroup fullCommand logFile snapshotTest.name
  where
    fullCommand :: ShellCommand
    fullCommand = "cd " ++ SP.fromAbsDir snapshotDir ~&& snapshotTestCommand

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
      getDirFiltered (return . filterIgnoredFilePaths) (SP.fromAbsDir snapshotDir)
        >>= filterM doesFileExist -- only files, no directories
        >>= mapM SP.parseAbsFile
      where
        filterIgnoredFilePaths =
          keepUnlessMatched
            ( map isBasenameOf [".DS_Store", "node_modules"]
                ++ [ -- The project lock file persists by design and contains the PID
                     -- of the last Wasp command, which is not deterministic.
                     isSubpathOf ".wasp/.projectlock",
                     isExtensionOf ".tgz"
                   ]
            )

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
  normalizeJsonFiles snapshotFiles
  return snapshotFiles
  where
    getSnapshotFilesForContentCheck :: IO [Path' Abs (File SnapshotFile)]
    getSnapshotFilesForContentCheck =
      getDirFiltered (return . filterIgnoredFilePaths) (SP.fromAbsDir snapshotDir)
        >>= filterM doesFileExist -- only files, no directories
        >>= mapM SP.parseAbsFile
      where
        filterIgnoredFilePaths =
          keepUnlessMatched
            ( map
                isBasenameOf
                [ ".DS_Store",
                  "CLAUDE.md",
                  "node_modules",
                  "dev.db",
                  "dev.db-journal",
                  ".gitignore",
                  ".waspinfo",
                  "package-lock.json",
                  "tsconfig.wasp.tsbuildinfo",
                  "tsconfig.src.tsbuildinfo",
                  "dist"
                ]
                ++ [ -- The project lock file persists by design and contains the PID
                     -- of the last Wasp command, which is not deterministic.
                     isSubpathOf ".wasp/.projectlock",
                     isExtensionOf ".tgz"
                   ]
            )

    -- Normalizes the JSON files that are not deterministic across machines:
    -- @package.json@ files are reformatted (ref: https://github.com/wasp-lang/wasp/issues/482)
    -- and both they and the installed npm deps log have the dev tarball paths hidden.
    normalizeJsonFiles :: [Path' Abs (File file)] -> IO ()
    normalizeJsonFiles snapshotFiles = do
      mapM_ (normalizeJsonFile formatPackageJson) $ filter isPackageJsonFile snapshotFiles
      mapM_ (normalizeJsonFile formatInstalledNpmDepsLog) $ filter isInstalledNpmDepsLogFile snapshotFiles
      where
        normalizeJsonFile :: (Aeson.Value -> BSL.ByteString) -> Path' Abs (File file) -> IO ()
        normalizeJsonFile format jsonFile =
          BS.readFile (SP.fromAbsFile jsonFile)
            >>= BSL.writeFile (SP.fromAbsFile jsonFile) . format . hideDevWaspNpmPackageTarballs . unsafeDecodeJson

        -- In development, projects depend on the Wasp npm packages through
        -- `file:` tarballs whose absolute paths depend on where the repo is
        -- checked out (see "Wasp.Project.WaspNpmPackage"), so we replace those
        -- paths with a placeholder. The lib tarballs are referenced relatively
        -- from inside the project, so they stay as they are.
        hideDevWaspNpmPackageTarballs :: Aeson.Value -> Aeson.Value
        hideDevWaspNpmPackageTarballs (Aeson.Object object) = Aeson.Object $ fmap hideDevWaspNpmPackageTarballs object
        hideDevWaspNpmPackageTarballs (Aeson.Array array) = Aeson.Array $ fmap hideDevWaspNpmPackageTarballs array
        hideDevWaspNpmPackageTarballs (Aeson.String specifier)
          | "file:/" `T.isPrefixOf` specifier && ".tgz" `T.isSuffixOf` specifier = Aeson.String "file:<dev-tarball>"
        hideDevWaspNpmPackageTarballs other = other

        isPackageJsonFile :: Path' Abs (File file) -> Bool
        isPackageJsonFile = equalFilePath "package.json" . takeFileName . SP.fromAbsFile

        isInstalledNpmDepsLogFile :: Path' Abs (File file) -> Bool
        isInstalledNpmDepsLogFile = equalFilePath "installedNpmDepsLog.json" . takeFileName . SP.fromAbsFile

        unsafeDecodeJson :: BS.ByteString -> Aeson.Value
        unsafeDecodeJson = fromJust . Aeson.decodeStrict

        formatPackageJson :: Aeson.Value -> BSL.ByteString
        formatPackageJson =
          AesonPretty.encodePretty'
            AesonPretty.Config
              { AesonPretty.confIndent = AesonPretty.Spaces 2,
                AesonPretty.confCompare = compare,
                AesonPretty.confNumFormat = AesonPretty.Generic,
                AesonPretty.confTrailingNewline = True
              }

        -- Same encoding "Wasp.Generator.NpmInstall.InstalledNpmDepsLog" uses to write the file.
        formatInstalledNpmDepsLog :: Aeson.Value -> BSL.ByteString
        formatInstalledNpmDepsLog = AesonPretty.encodePretty

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
      goldenSnapshotDir </> fromJust (SP.parseRelFile relSnapshotFilePath)
      where
        relSnapshotFilePath = makeRelative (SP.fromAbsDir currentSnapshotDir) (SP.fromAbsFile currentSnapshotFile)

type FilePathFilter = FilePath -> Bool

keepUnlessMatched :: [FilePathFilter] -> FilePathFilter
keepUnlessMatched filters filePath = not $ any ($ filePath) filters

isBasenameOf :: FilePath -> FilePathFilter
isBasenameOf basename filePath = basename == takeFileName filePath

isSubpathOf :: FilePath -> FilePathFilter
isSubpathOf subPath filePath = splitDirectories subPath `isInfixOf` splitDirectories filePath

-- | Interruptible version of callCommand that terminates the entire process tree on async exception.
-- Uses process groups so that when a thread is cancelled (e.g., when another concurrent test fails),
-- all child processes are also terminated rather than continuing to run.
callCommandInProcessGroup :: String -> Path' Abs (File TestLogFile) -> String -> IO ()
callCommandInProcessGroup cmd logFile testName = do
  (logOut, logErr) <- openLogForCommand logFile testName cmd
  bracket
    ( createProcess
        (shell cmd)
          { create_group = True,
            std_out = UseHandle logOut,
            std_err = UseHandle logErr
          }
    )
    (\(_, _, _, ph) -> interruptProcessGroupOf ph)
    ( \(_, _, _, ph) -> do
        exitCode <- waitForProcess ph
        case exitCode of
          ExitSuccess -> return ()
          ExitFailure code -> fail =<< formatCommandFailure code logFile
    )
