{-# LANGUAGE TupleSections #-}

-- | Snapshot tests compare the files their steps produce (the current snapshot)
-- to the files they're expected to produce (the golden snapshot).
--
-- Snapshots are compared in two ways:
--
--   1. By existence: the manifest file lists every file in the snapshot and
--      is itself part of the snapshot.
--   2. By content: files that are deterministic must be identical.
module E2e.Snapshot
  ( snapshotTest,
  )
where

import Control.Exception (throwIO)
import Control.Monad (filterM, forM_, unless)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encode.Pretty as AesonPretty
import Data.Algorithm.Diff (getGroupedDiff)
import Data.Algorithm.DiffOutput (ppDiff)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.List (isInfixOf, sort)
import Data.Maybe (fromJust)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import E2e.Paths (getSnapshotsDir)
import E2e.Test (E2eTest (..), runTestSteps)
import E2e.TestM (TestM)
import System.Directory (createDirectoryIfMissing, doesFileExist, removePathForcibly)
import System.Directory.Recursive (getDirFiltered)
import System.FilePath (isExtensionOf, makeRelative, splitDirectories, takeDirectory, takeFileName, (</>))
import System.IO.Error (doesNotExistErrorType, mkIOError)
import Test.Tasty.Golden.Advanced (goldenTest)

-- | A test whose steps run in the @<name>-current@ snapshot dir. When they
-- succeed, everything they leave behind is compared to the @<name>-golden@
-- snapshot dir.
--
-- The golden snapshot is created when it doesn't exist, and replaced with the
-- current one when they differ and the tests run with @--accept@.
--
-- The whole snapshot is a single test, since we only know which files it has
-- after its steps run.
snapshotTest :: String -> TestM a -> E2eTest
snapshotTest name steps = E2eTest $ \_ ->
  goldenTest
    name
    (getSnapshotDir Golden >>= readGoldenSnapshot)
    (runStepsAndReadCurrentSnapshot name steps)
    compareSnapshots
    (\snapshot -> getSnapshotDir Golden >>= writeGoldenSnapshot snapshot)
  where
    getSnapshotDir snapshotType = (</> name ++ "-" ++ show snapshotType) <$> getSnapshotsDir

data SnapshotType = Golden | Current

instance Show SnapshotType where
  show Golden = "golden"
  show Current = "current"

-- | The bytes of each content-checked file, keyed by its path relative to the
-- snapshot dir, sorted by path.
type Snapshot = [(FilePath, BS.ByteString)]

runStepsAndReadCurrentSnapshot :: String -> TestM a -> IO Snapshot
runStepsAndReadCurrentSnapshot name steps = do
  snapshotsDir <- getSnapshotsDir
  let currentSnapshotDir = snapshotsDir </> name ++ "-" ++ show Current
  removePathForcibly currentSnapshotDir
  createDirectoryIfMissing True currentSnapshotDir
  -- The log is next to the snapshot dir, so it isn't part of the snapshot.
  _ <- runTestSteps name currentSnapshotDir (snapshotsDir </> name ++ ".log") steps
  writeManifest currentSnapshotDir
  normalizePackageJsonFiles currentSnapshotDir
  readSnapshot currentSnapshotDir

-- | The manifest file is the marker of the golden snapshot's existence. When
-- it's missing, we tell tasty-golden the golden snapshot doesn't exist, so it
-- creates it.
readGoldenSnapshot :: FilePath -> IO Snapshot
readGoldenSnapshot goldenSnapshotDir = do
  let manifestFile = goldenSnapshotDir </> manifestFileName
  manifestExists <- doesFileExist manifestFile
  unless manifestExists
    $ throwIO
    $ mkIOError doesNotExistErrorType "golden snapshot manifest" Nothing (Just manifestFile)
  readSnapshot goldenSnapshotDir

-- | Reads strictly, since a golden snapshot read may be followed by its update.
readSnapshot :: FilePath -> IO Snapshot
readSnapshot snapshotDir =
  getSnapshotFiles isContentCheckIgnored snapshotDir
    >>= mapM (\file -> (file,) <$> BS.readFile (snapshotDir </> file))

-- | Only writes the compared files, so ignored files (e.g. @node_modules@)
-- never end up in version control.
writeGoldenSnapshot :: Snapshot -> FilePath -> IO ()
writeGoldenSnapshot snapshot goldenSnapshotDir = do
  removePathForcibly goldenSnapshotDir
  forM_ snapshot $ \(file, content) -> do
    let goldenFile = goldenSnapshotDir </> file
    createDirectoryIfMissing True $ takeDirectory goldenFile
    BS.writeFile goldenFile content

-- | Reports all differences at once.
compareSnapshots :: Snapshot -> Snapshot -> IO (Maybe String)
compareSnapshots goldenSnapshot currentSnapshot =
  return $
    if null differences
      then Nothing
      else Just $ unlines $ "Current snapshot does not match the golden snapshot:" : differences
  where
    goldenFiles = map fst goldenSnapshot
    currentFiles = map fst currentSnapshot

    differences =
      concat
        [ map ("file missing in current snapshot: " ++) $ filter (`notElem` currentFiles) goldenFiles,
          map ("file not present in golden snapshot: " ++) $ filter (`notElem` goldenFiles) currentFiles,
          [ "file contents differ: " ++ file ++ "\n" ++ showContentDiff goldenContent currentContent
          | (file, goldenContent) <- goldenSnapshot,
            Just currentContent <- [lookup file currentSnapshot],
            goldenContent /= currentContent
          ]
        ]

showContentDiff :: BS.ByteString -> BS.ByteString -> String
showContentDiff goldenContent currentContent =
  case (TE.decodeUtf8' goldenContent, TE.decodeUtf8' currentContent) of
    (Right goldenText, Right currentText) ->
      truncateDiff $ ppDiff $ getGroupedDiff (textLines goldenText) (textLines currentText)
    _ -> "(binary files differ)"
  where
    textLines = map T.unpack . T.lines

    truncateDiff diff
      | length diff <= maxDiffLength = diff
      | otherwise = take maxDiffLength diff ++ "\n... (diff truncated)\n"
    maxDiffLength = 10000

-- | Lists the files whose existence we check, so we notice when files appear
-- or disappear. That includes files whose content we don't compare.
writeManifest :: FilePath -> IO ()
writeManifest snapshotDir =
  getSnapshotFiles isExistenceCheckIgnored snapshotDir
    >>= writeFile (snapshotDir </> manifestFileName) . unlines

manifestFileName :: FilePath
manifestFileName = "snapshot-file-list.manifest"

isExistenceCheckIgnored :: FilePath -> Bool
isExistenceCheckIgnored =
  matchesAny $
    map isBasenameOf [".DS_Store", "node_modules"]
      ++ alwaysIgnoredPaths

isContentCheckIgnored :: FilePath -> Bool
isContentCheckIgnored =
  matchesAny $
    map
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
      ++ alwaysIgnoredPaths

alwaysIgnoredPaths :: [FilePath -> Bool]
alwaysIgnoredPaths =
  [ -- The @wasp/spec package copied into .wasp/spec is identical to what we
    -- ship in waspc/data/packages/spec (built from packages/spec). It's only
    -- copied so `npm install` can reach it. If there are errors in this
    -- package, they will surface either during package tests or in the project
    -- snapshot.
    isSubpathOf ".wasp/spec",
    -- The project lock file persists by design and contains the PID of the
    -- last Wasp command, which is not deterministic.
    isSubpathOf ".wasp/.projectlock",
    isExtensionOf ".tgz"
  ]

matchesAny :: [FilePath -> Bool] -> FilePath -> Bool
matchesAny predicates path = any ($ path) predicates

isBasenameOf :: FilePath -> FilePath -> Bool
isBasenameOf basename path = basename == takeFileName path

isSubpathOf :: FilePath -> FilePath -> Bool
isSubpathOf subPath path = splitDirectories subPath `isInfixOf` splitDirectories path

-- | Files (not directories) relative to the snapshot dir, sorted.
getSnapshotFiles :: (FilePath -> Bool) -> FilePath -> IO [FilePath]
getSnapshotFiles isIgnored snapshotDir =
  sort . map (makeRelative snapshotDir)
    <$> (getDirFiltered (return . not . isIgnored) snapshotDir >>= filterM doesFileExist)

-- | Rewrites @package.json@ files in a deterministic format.
-- Ref: https://github.com/wasp-lang/wasp/issues/482
normalizePackageJsonFiles :: FilePath -> IO ()
normalizePackageJsonFiles snapshotDir =
  getSnapshotFiles isContentCheckIgnored snapshotDir
    >>= mapM_ (normalizePackageJson . (snapshotDir </>)) . filter ((== "package.json") . takeFileName)
  where
    normalizePackageJson file =
      BS.readFile file >>= BSL.writeFile file . formatJson . fromJust . Aeson.decodeStrict

    formatJson :: Aeson.Value -> BSL.ByteString
    formatJson =
      AesonPretty.encodePretty'
        AesonPretty.Config
          { AesonPretty.confIndent = AesonPretty.Spaces 2,
            AesonPretty.confCompare = compare,
            AesonPretty.confNumFormat = AesonPretty.Generic,
            AesonPretty.confTrailingNewline = True
          }
