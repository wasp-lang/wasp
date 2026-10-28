-- | Compares what a snapshot test produced (the current snapshot) to what it
-- is expected to produce (the golden snapshot).
--
-- Snapshots are compared in two ways:
--   1. By existence: the manifest file lists every file in the snapshot and
--      is itself part of the snapshot.
--   2. By content: files that are deterministic must be identical.
module E2e.Snapshot
  ( SnapshotMode (..),
    SnapshotOutcome (..),
    finalizeSnapshot,
  )
where

import Control.Monad (filterM, forM, forM_, when)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encode.Pretty as AesonPretty
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.List (isInfixOf, sort, (\\))
import qualified Data.Set as Set
import E2e.Paths (SnapshotDir)
import StrongPath (Abs, Dir, Path')
import qualified StrongPath as SP
import System.Directory
  ( copyFile,
    createDirectoryIfMissing,
    doesDirectoryExist,
    doesFileExist,
    doesPathExist,
    listDirectory,
    pathIsSymbolicLink,
    removeDirectoryRecursive,
  )
import System.Exit (ExitCode (..))
import System.FilePath (isExtensionOf, splitDirectories, takeDirectory, takeFileName, (</>))
import System.Process (proc, readCreateProcessWithExitCode)

data SnapshotMode
  = CompareToGolden
  | -- | Replaces the golden snapshot with the current one.
    AcceptCurrent

data SnapshotOutcome
  = SnapshotMatches
  | SnapshotAccepted
  | SnapshotDiffers String

finalizeSnapshot :: SnapshotMode -> Path' Abs (Dir SnapshotDir) -> Path' Abs (Dir SnapshotDir) -> IO SnapshotOutcome
finalizeSnapshot mode currentSnapshotDir goldenSnapshotDir = do
  writeManifest currentDir
  currentFiles <- listFilesRecursively isContentCheckIgnored currentDir
  mapM_ (normalizePackageJson . (currentDir </>)) (filter ((== "package.json") . takeFileName) currentFiles)
  case mode of
    AcceptCurrent -> do
      acceptSnapshot currentFiles
      return SnapshotAccepted
    CompareToGolden -> compareToGolden currentFiles
  where
    currentDir = SP.fromAbsDir currentSnapshotDir
    goldenDir = SP.fromAbsDir goldenSnapshotDir

    acceptSnapshot currentFiles = do
      goldenExists <- doesPathExist goldenDir
      when goldenExists $ removeDirectoryRecursive goldenDir
      forM_ currentFiles $ \file -> do
        createDirectoryIfMissing True (takeDirectory $ goldenDir </> file)
        copyFile (currentDir </> file) (goldenDir </> file)

    compareToGolden currentFiles = do
      goldenExists <- doesDirectoryExist goldenDir
      if not goldenExists
        then return $ SnapshotDiffers $ "Golden snapshot " ++ goldenDir ++ " doesn't exist. Run the tests with --accept to create it."
        else do
          -- Filtered like the current files, so e.g. a .DS_Store in the golden dir doesn't count as missing.
          goldenFiles <- listFilesRecursively isContentCheckIgnored goldenDir
          let missingFiles = goldenFiles \\ currentFiles
              extraFiles = currentFiles \\ goldenFiles
              commonFiles = Set.toList $ Set.intersection (Set.fromList goldenFiles) (Set.fromList currentFiles)
          changedFiles <- filterM hasChanged commonFiles
          if null missingFiles && null extraFiles && null changedFiles
            then return SnapshotMatches
            else SnapshotDiffers <$> describeDifferences missingFiles extraFiles changedFiles

    hasChanged file = (/=) <$> BS.readFile (goldenDir </> file) <*> BS.readFile (currentDir </> file)

    describeDifferences missingFiles extraFiles changedFiles = do
      diffs <- forM (take maxShownDiffs changedFiles) $ \file -> diffFiles (goldenDir </> file) (currentDir </> file)
      return . unlines $
        ["Current snapshot differs from golden snapshot " ++ goldenDir ++ "."]
          ++ section "Missing files (in golden, not in current):" missingFiles
          ++ section "Extra files (in current, not in golden):" extraFiles
          ++ section "Changed files:" changedFiles
          ++ diffs
          ++ ["(Showing diffs of the first " ++ show maxShownDiffs ++ " changed files.)" | length changedFiles > maxShownDiffs]
          ++ ["If the changes are expected, run the tests with --accept."]

    section _ [] = []
    section title files = title : map ("  " ++) files

    maxShownDiffs = 5

-- | Lists every file in the snapshot dir, so we notice when files appear or disappear.
-- That includes files that are excluded from the content comparison.
writeManifest :: FilePath -> IO ()
writeManifest snapshotDir = do
  files <- listFilesRecursively isExistenceCheckIgnored snapshotDir
  writeFile (snapshotDir </> manifestFileName) (unlines $ sort files)

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
  [ -- The @wasp.sh/spec package copied into .wasp/spec is identical to what we
    -- ship in waspc/data/packages/spec. It's only copied so `npm install` can reach it.
    isSubpathOf ".wasp/spec",
    -- The project lock file contains the PID of the last Wasp command.
    isSubpathOf ".wasp/.projectlock",
    isExtensionOf ".tgz"
  ]

matchesAny :: [FilePath -> Bool] -> FilePath -> Bool
matchesAny predicates path = any ($ path) predicates

isBasenameOf :: FilePath -> FilePath -> Bool
isBasenameOf basename path = basename == takeFileName path

isSubpathOf :: FilePath -> FilePath -> Bool
isSubpathOf subPath path = splitDirectories subPath `isInfixOf` splitDirectories path

-- | Lists files (not directories) relative to the dir, sorted.
-- Doesn't descend into ignored or symlinked directories.
listFilesRecursively :: (FilePath -> Bool) -> FilePath -> IO [FilePath]
listFilesRecursively isIgnored rootDir = sort <$> go ""
  where
    go relDir = do
      entries <- listDirectory (rootDir </> relDir)
      concat
        <$> forM
          [relDir </> entry | entry <- entries, not (isIgnored (relDir </> entry))]
          ( \relPath -> do
              let absPath = rootDir </> relPath
              isSymlink <- pathIsSymbolicLink absPath
              isDir <- doesDirectoryExist absPath
              if isDir && not isSymlink
                then go relPath
                else do
                  isFile <- doesFileExist absPath
                  return [relPath | isFile]
          )

-- | Rewrites package.json files in a deterministic format.
-- Ref: https://github.com/wasp-lang/wasp/issues/482
normalizePackageJson :: FilePath -> IO ()
normalizePackageJson file = do
  content <- BS.readFile file
  case Aeson.decodeStrict content :: Maybe Aeson.Value of
    Nothing -> return ()
    Just json -> BSL.writeFile file (AesonPretty.encodePretty' prettyConfig json)
  where
    prettyConfig =
      AesonPretty.Config
        { AesonPretty.confIndent = AesonPretty.Spaces 2,
          AesonPretty.confCompare = compare,
          AesonPretty.confNumFormat = AesonPretty.Generic,
          AesonPretty.confTrailingNewline = True
        }

-- | A unified diff of two files, using git so we don't have to implement diffing.
diffFiles :: FilePath -> FilePath -> IO String
diffFiles goldenFile currentFile = do
  (exitCode, out, err) <-
    readCreateProcessWithExitCode (proc "git" ["diff", "--no-index", "--no-color", "--", goldenFile, currentFile]) ""
  return $ case exitCode of
    ExitFailure 1 -> truncateLines 200 out
    _ -> "Failed to diff " ++ currentFile ++ ": " ++ err
  where
    truncateLines maxLines text =
      let textLines = lines text
       in unlines (take maxLines textLines) ++ (if length textLines > maxLines then "... (diff truncated)\n" else "")
