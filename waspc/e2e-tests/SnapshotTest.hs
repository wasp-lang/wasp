{-# LANGUAGE DuplicateRecordFields #-}

module SnapshotTest
  ( SnapshotTest,
    makeSnapshotTest,
    testTreeFromSnapshotTest,
  )
where

import Context (SnapshotTestContext (..), WaspProjectContext (..))
import Control.Exception (Exception, throwIO)
import Control.Monad (filterM, forM, forM_, unless)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encode.Pretty as AesonPretty
import Data.Algorithm.Diff (getGroupedDiff)
import Data.Algorithm.DiffOutput (ppDiff)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.List (isInfixOf, sort)
import Data.Maybe (catMaybes, fromJust)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import FileSystem
  ( SnapshotDir,
    SnapshotFile,
    SnapshotFileListManifestFile,
    SnapshotType (..),
    getSnapshotsDir,
    snapshotDirInSnapshotsDir,
    snapshotFileListManifestFileInSnapshotDir,
    snapshotLogFileInSnapshotsDir,
  )
import Step (Step, runSteps)
import StrongPath (Abs, Dir, File, Path', parseRelDir, (</>))
import qualified StrongPath as SP
import System.Directory (copyFile, createDirectoryIfMissing, doesFileExist, removePathForcibly)
import System.Directory.Recursive (getDirFiltered)
import System.FilePath (equalFilePath, isExtensionOf, makeRelative, splitDirectories, takeDirectory, takeFileName)
import System.IO.Error (doesNotExistErrorType, mkIOError)
import Test.Tasty (TestTree)
import Test.Tasty.Golden.Advanced (goldenTest)

data SnapshotTest = SnapshotTest
  { name :: String,
    steps :: Step SnapshotTestContext [()]
  }

makeSnapshotTest :: String -> [Step SnapshotTestContext ()] -> SnapshotTest
makeSnapshotTest name steps = SnapshotTest {name, steps = sequence steps}

-- | A snapshot test is a single golden test: it runs the test's steps in the
-- "current" snapshot dir and then compares the current snapshot against the
-- "golden" (expected) snapshot dir, all at test runtime.
--
-- Golden semantics (see 'Test.Tasty.Golden.Internal.runGolden'):
--
-- * When the golden snapshot does not exist, it is created from the current
--   snapshot. This is what `./run test:waspc:e2e:accept-all` relies on
--   (it deletes the golden snapshots and re-runs the tests).
-- * With @--accept@, a golden snapshot that differs from the current one is
--   updated to match it.
testTreeFromSnapshotTest :: SnapshotTest -> TestTree
testTreeFromSnapshotTest snapshotTest =
  goldenTest
    snapshotTest.name
    getGoldenSnapshotDir
    runStepsAndGetCurrentSnapshotDir
    compareSnapshotDirs
    (updateGoldenSnapshotDir snapshotTest.name)
  where
    -- The golden value is the golden snapshot dir. Its manifest file is used as
    -- the marker of the golden snapshot's existence: when it is missing, we
    -- signal "golden does not exist" to tasty-golden (which then creates it).
    getGoldenSnapshotDir :: IO (Path' Abs (Dir SnapshotDir))
    getGoldenSnapshotDir = do
      goldenSnapshotDir <- getSnapshotDir snapshotTest.name Golden
      let goldenManifestFilePath =
            SP.fromAbsFile $ goldenSnapshotDir </> snapshotFileListManifestFileInSnapshotDir
      manifestExists <- doesFileExist goldenManifestFilePath
      unless manifestExists $
        throwIO $
          mkIOError doesNotExistErrorType "golden snapshot manifest" Nothing (Just goldenManifestFilePath)
      return goldenSnapshotDir

    -- The tested value is the current snapshot dir, produced by running the
    -- snapshot test's steps in it.
    runStepsAndGetCurrentSnapshotDir :: IO (Path' Abs (Dir SnapshotDir))
    runStepsAndGetCurrentSnapshotDir = do
      snapshotsDir <- getSnapshotsDir
      currentSnapshotDir <- getSnapshotDir snapshotTest.name Current
      let logFile = snapshotsDir </> snapshotLogFileInSnapshotsDir snapshotTest.name

      -- Remove any leftovers of a previous run of this snapshot test.
      removePathForcibly $ SP.fromAbsDir currentSnapshotDir
      createDirectoryIfMissing True $ SP.fromAbsDir currentSnapshotDir

      let snapshotTestContext =
            SnapshotTestContext
              { snapshotDir = currentSnapshotDir,
                waspProjectContext =
                  WaspProjectContext
                    { waspProjectDir = currentSnapshotDir </> (fromJust . parseRelDir $ "wasp-app"),
                      waspProjectName = "wasp-app"
                    }
              }

      result <- runSteps snapshotTest.name logFile snapshotTestContext snapshotTest.steps
      either (throwIO . SnapshotTestStepsFailure) return result

      generateSnapshotFileListManifest
        currentSnapshotDir
        (currentSnapshotDir </> snapshotFileListManifestFileInSnapshotDir)
      normalizePackageJsonFiles currentSnapshotDir

      return currentSnapshotDir

    getSnapshotDir :: String -> SnapshotType -> IO (Path' Abs (Dir SnapshotDir))
    getSnapshotDir snapshotTestName snapshotType = do
      snapshotsDir <- getSnapshotsDir
      return $ snapshotsDir </> snapshotDirInSnapshotsDir snapshotTestName snapshotType

-- | Thrown when the steps of a snapshot test fail: the snapshot comparison is
-- skipped and the test fails with the (already formatted) step failure message.
newtype SnapshotTestStepsFailure = SnapshotTestStepsFailure String

instance Show SnapshotTestStepsFailure where
  show (SnapshotTestStepsFailure message) = message

instance Exception SnapshotTestStepsFailure

-- Snapshot comparison

-- | Compares the files of the golden and the current snapshot dir (existence
-- and content), reporting all mismatches at once.
compareSnapshotDirs ::
  Path' Abs (Dir SnapshotDir) ->
  Path' Abs (Dir SnapshotDir) ->
  IO (Maybe String)
compareSnapshotDirs goldenSnapshotDir currentSnapshotDir = do
  goldenFiles <- getRelSnapshotFilesForContentCheck goldenSnapshotDir
  currentFiles <- getRelSnapshotFilesForContentCheck currentSnapshotDir

  let filesOnlyInGolden = filter (`notElem` currentFiles) goldenFiles
      filesOnlyInCurrent = filter (`notElem` goldenFiles) currentFiles
      filesInBoth = filter (`elem` goldenFiles) currentFiles

  fileContentMismatches <- catMaybes <$> forM filesInBoth compareFileContents

  let mismatchReports =
        concat
          [ map ("file missing in current snapshot: " ++) filesOnlyInGolden,
            map ("file not present in golden snapshot: " ++) filesOnlyInCurrent,
            fileContentMismatches
          ]

  return $
    if null mismatchReports
      then Nothing
      else
        Just $
          unlines $
            ("Current snapshot does not match the golden snapshot (" ++ SP.fromAbsDir goldenSnapshotDir ++ "):")
              : mismatchReports
  where
    compareFileContents :: FilePath -> IO (Maybe String)
    compareFileContents relFilePath = do
      goldenContent <- BS.readFile (SP.fromAbsDir goldenSnapshotDir ++ relFilePath)
      currentContent <- BS.readFile (SP.fromAbsDir currentSnapshotDir ++ relFilePath)
      return $
        if goldenContent == currentContent
          then Nothing
          else Just $ "file contents differ: " ++ relFilePath ++ "\n" ++ renderContentDiff goldenContent currentContent

    renderContentDiff :: BS.ByteString -> BS.ByteString -> String
    renderContentDiff goldenContent currentContent =
      case (TE.decodeUtf8' goldenContent, TE.decodeUtf8' currentContent) of
        (Right goldenText, Right currentText) ->
          truncateDiff $ ppDiff $ getGroupedDiff (textToLines goldenText) (textToLines currentText)
        _ -> "(binary files differ)"
      where
        textToLines = map T.unpack . T.lines

    truncateDiff :: String -> String
    truncateDiff diff
      | length diff <= maxDiffLength = diff
      | otherwise = take maxDiffLength diff ++ "\n... (diff truncated)\n"
      where
        maxDiffLength = 10000

-- | Replaces the golden snapshot dir with the (filtered) contents of the
-- current snapshot dir. Called by tasty-golden when the golden snapshot does
-- not exist, or when it differs from the current one and @--accept@ is passed.
updateGoldenSnapshotDir :: String -> Path' Abs (Dir SnapshotDir) -> IO ()
updateGoldenSnapshotDir snapshotTestName currentSnapshotDir = do
  snapshotsDir <- getSnapshotsDir
  let goldenSnapshotDir = snapshotsDir </> snapshotDirInSnapshotsDir snapshotTestName Golden

  removePathForcibly $ SP.fromAbsDir goldenSnapshotDir

  -- Copy only the files that take part in the snapshot comparison, so that
  -- ignored files (e.g. node_modules) don't end up in version control.
  relFilesToCopy <- getRelSnapshotFilesForContentCheck currentSnapshotDir
  forM_ relFilesToCopy $ \relFilePath -> do
    let dstFilePath = SP.fromAbsDir goldenSnapshotDir ++ relFilePath
    createDirectoryIfMissing True $ takeDirectory dstFilePath
    copyFile (SP.fromAbsDir currentSnapshotDir ++ relFilePath) dstFilePath

-- Snapshot file listing

-- | Lists the files of a snapshot dir that take part in the snapshot content
-- comparison, as relative paths (with a leading path separator stripped off by
-- 'makeRelative'), sorted.
getRelSnapshotFilesForContentCheck :: Path' Abs (Dir SnapshotDir) -> IO [FilePath]
getRelSnapshotFilesForContentCheck snapshotDir =
  map (makeRelative (SP.fromAbsDir snapshotDir) . SP.fromAbsFile)
    <$> getSnapshotFilesForContentCheck snapshotDir

getSnapshotFilesForContentCheck :: Path' Abs (Dir SnapshotDir) -> IO [Path' Abs (File SnapshotFile)]
getSnapshotFilesForContentCheck snapshotDir =
  sort <$> getSnapshotFiles snapshotDir filterIgnoredFilePaths
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
            ++ [ -- The @wasp.sh/spec package copied into .wasp/spec is identical to
                 -- what we ship in waspc/data/packages/spec, so we skip it.
                 isSubpathOf ".wasp/spec",
                 isExtensionOf ".tgz"
               ]
        )

generateSnapshotFileListManifest :: Path' Abs (Dir SnapshotDir) -> Path' Abs (File SnapshotFileListManifestFile) -> IO ()
generateSnapshotFileListManifest snapshotDir snapshotFileListManifestFile =
  getSnapshotFilesForExistenceCheck >>= writeSnapshotFileListManifest
  where
    getSnapshotFilesForExistenceCheck :: IO [Path' Abs (File SnapshotFile)]
    getSnapshotFilesForExistenceCheck = getSnapshotFiles snapshotDir filterIgnoredFilePaths
      where
        filterIgnoredFilePaths =
          keepUnlessMatched
            ( map isBasenameOf [".DS_Store", "node_modules"]
                ++ [
                     -- The @wasp.sh/spec package copied into .wasp/spec is identical to
                     -- what we ship in waspc/data/packages/spec.
                     -- It is only copied into .wasp because we need to reach it with `npm install`.
                     -- If there are errors in this package, they will surface either during package tests or
                     -- manifest in the project snapshot. We can therefore skip it.
                     isSubpathOf ".wasp/spec",
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

getSnapshotFiles :: Path' Abs (Dir SnapshotDir) -> FilePathFilter -> IO [Path' Abs (File SnapshotFile)]
getSnapshotFiles snapshotDir filePathFilter =
  getDirFiltered (return . filePathFilter) (SP.fromAbsDir snapshotDir)
    >>= filterM doesFileExist -- only files, no directories
    >>= mapM SP.parseAbsFile

-- | Normalizes @package.json@ files of the snapshot into deterministic format
-- for snapshot comparison.
-- Ref: https://github.com/wasp-lang/wasp/issues/482
normalizePackageJsonFiles :: Path' Abs (Dir SnapshotDir) -> IO ()
normalizePackageJsonFiles snapshotDir =
  getSnapshotFilesForContentCheck snapshotDir >>= mapM_ formatPackageJsonFile . filter isPackageJsonFile
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

type FilePathFilter = FilePath -> Bool

keepUnlessMatched :: [FilePathFilter] -> FilePathFilter
keepUnlessMatched filters filePath = not $ any ($ filePath) filters

isBasenameOf :: FilePath -> FilePathFilter
isBasenameOf basename filePath = basename == takeFileName filePath

isSubpathOf :: FilePath -> FilePathFilter
isSubpathOf subPath filePath = splitDirectories subPath `isInfixOf` splitDirectories filePath
