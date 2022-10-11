{-# LANGUAGE TupleSections #-}

module Wasp.Generator.WriteFileDrafts
  ( synchronizeFileDraftsWithDisk,
    fileDraftsToWriteAndFilesToDelete, -- Exported for testing.
    removeFromChecksumFile,
  )
where

import Control.Arrow (Arrow (first))
import Control.Monad (unless)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encode.Pretty as AesonPretty
import qualified Data.ByteString.Lazy as BSL
import Data.Either (lefts, rights)
import qualified Data.HashMap.Strict as Map
import qualified Data.HashSet as Set
import Data.List (sortBy, sort, group)
import StrongPath (Abs, Dir, File', Path', Rel, relfile, (</>))
import qualified StrongPath as SP
import System.Directory (removeDirectoryRecursive, removeFile)
import System.IO.Error (isDoesNotExistError)
import UnliftIO.Exception (catch, throwIO)
import Wasp.Generator.Common (ProjectRootDir)
import Wasp.Generator.FileDraft (FileDraft, write)
import Wasp.Generator.FileDraft.Writeable (FileOrDirPathRelativeTo, Writeable (getChecksum, getDstPath))
import Wasp.Util (Checksum)

-- | Writes given file drafts to disk, in the provided destination directory.
-- Also makes sure to remove any redundant file drafts that have been left on the disk from before.
-- It is smart when writing, so it doesn't write file drafts that are already written on the disk from before.
synchronizeFileDraftsWithDisk :: Path' Abs (Dir ProjectRootDir) -> [FileDraft] -> IO ()
synchronizeFileDraftsWithDisk dstDir fileDrafts = do

  return $! assertDstPathsAreUnique fileDrafts

  maybePathsToChecksums <- readChecksumFile dstDir
  case maybePathsToChecksums of
    -- If checksums file is missing/corrupted, we delete all of the generated code
    -- since we are not sure what is the state on the disk and don't want redundant files
    -- to remain after we write the new file drafts.
    -- Normally we delete redundant left-over files based on the checksum file, but since we don't have it,
    -- the only way to be sure is just to delete everything.
    Nothing -> do
      removeDirectoryRecursive (SP.fromAbsDir dstDir)
        `catch` \e -> unless (isDoesNotExistError e) $ throwIO e
    -- We remove checksum file even if it is valid, so in case we fail in the middle writing on the disk
    -- and don't get an opportunity to successfully create a new checksum file (reflecting new state on the disk),
    -- it is clear that something went wrong. Otherwise old checksums file could be accidentally used
    -- while state on the disk might not be as it says anymore.
    Just _ -> removeFile $ SP.fromAbsFile $ dstDir </> checksumFileInProjectRoot

  fileDraftsWithChecksums <- mapM (\fd -> (fd,) <$> getChecksum fd) fileDrafts

  let (fileDraftsToWrite, filesToDelete) = fileDraftsToWriteAndFilesToDelete maybePathsToChecksums fileDraftsWithChecksums
  mapM_ (write dstDir) fileDraftsToWrite
  deleteFilesAndDirs dstDir filesToDelete

  let relativePathsToChecksums = map (first getDstPath) fileDraftsWithChecksums
  writeChecksumFile dstDir relativePathsToChecksums

type RelPathsToChecksums = [(FileOrDirPathRelativeTo ProjectRootDir, Checksum)]

type RelPathsToChecksumsMap = Map.HashMap (FileOrDirPathRelativeTo ProjectRootDir) Checksum

-- | Takes file drafts and verifies if the destination paths are unique.
assertDstPathsAreUnique :: [FileDraft] -> ()
assertDstPathsAreUnique fileDrafts =
  let fdDstPaths = map getDstPath fileDrafts
      duplicateFdDstPaths = map head $ filter ((> 1) . length) (group . sort $ fdDstPaths)
      errMessage = unlines $ "FileDraft destination paths are not unique! Duplicates include: " : map show duplicateFdDstPaths
    in if null duplicateFdDstPaths then () else error errMessage

-- | This file stores all checksums for files and directories that were written to disk
-- on the last project generation.
checksumFileInProjectRoot :: Path' (Rel ProjectRootDir) File'
checksumFileInProjectRoot = [relfile|.waspchecksums|]

-- | Takes (possibly) existing written file paths and their checksums, and the current
-- FileDraft and Checksums to be written, and decides which of the new FileDrafts to
-- write and what redundant files on disk to delete.
fileDraftsToWriteAndFilesToDelete ::
  Maybe RelPathsToChecksums ->
  [(FileDraft, Checksum)] ->
  ([FileDraft], [FileOrDirPathRelativeTo ProjectRootDir])
fileDraftsToWriteAndFilesToDelete Nothing fileDraftsWithChecksums =
  (fst <$> fileDraftsWithChecksums, [])
fileDraftsToWriteAndFilesToDelete (Just existingFilePathsToChecksums) fileDraftsWithChecksums =
  let fileDrafts = fst <$> fileDraftsWithChecksums
      existingFilePathsToChecksumsMap = Map.fromList existingFilePathsToChecksums
   in ( getNewFileDrafts existingFilePathsToChecksumsMap fileDrafts
          ++ getChangedFileDrafts existingFilePathsToChecksumsMap fileDraftsWithChecksums,
        getRedundantGeneratedFiles existingFilePathsToChecksums fileDrafts
      )

getNewFileDrafts :: RelPathsToChecksumsMap -> [FileDraft] -> [FileDraft]
getNewFileDrafts existingFilePathsToChecksumsMap fileDrafts =
  filter (\draft -> not $ Map.member (getDstPath draft) existingFilePathsToChecksumsMap) fileDrafts

getChangedFileDrafts :: RelPathsToChecksumsMap -> [(FileDraft, Checksum)] -> [FileDraft]
getChangedFileDrafts existingFilePathsToChecksumsMap fileDraftsWithChecksums =
  fst <$> filter alreadyExistsWithDifferentChecksum fileDraftsWithChecksums
  where
    alreadyExistsWithDifferentChecksum :: (FileDraft, Checksum) -> Bool
    alreadyExistsWithDifferentChecksum (fd, newChecksum) =
      let newPath = getDstPath fd
          maybeOldChecksum = Map.lookup newPath existingFilePathsToChecksumsMap
       in case maybeOldChecksum of
            Nothing -> False
            Just oldChecksum -> oldChecksum /= newChecksum

getRedundantGeneratedFiles :: RelPathsToChecksums -> [FileDraft] -> [FileOrDirPathRelativeTo ProjectRootDir]
getRedundantGeneratedFiles existingFilePathsToChecksums fileDrafts =
  let fileDraftPathsSet = Set.fromList $ getDstPath <$> fileDrafts
   in filter (not . (`Set.member` fileDraftPathsSet)) (fst <$> existingFilePathsToChecksums)

-- | This function will return Nothing in two cases:
--  1) The checksum file does not exist, or
--  2) The checksum file was not parsable by Aeson.
-- TODO: Extract the readFile and leave mostly pure function to add tests for.
readChecksumFile :: Path' Abs (Dir ProjectRootDir) -> IO (Maybe RelPathsToChecksums)
readChecksumFile dstDir = do
  maybeContents <-
    (Just <$> BSL.readFile checksumFP)
      `catch` (\e -> if isDoesNotExistError e then return Nothing else throwIO e)
  return $ do
    contents <- maybeContents
    typeAndPathAndChecksums <- Aeson.decode contents :: Maybe [((String, FilePath), Checksum)]
    sequence $ (\(typeAndPath, checksum) -> (,checksum) <$> fromTypeAndPathToSp typeAndPath) <$> typeAndPathAndChecksums
  where
    checksumFP = SP.fromAbsFile $ dstDir </> checksumFileInProjectRoot

    fromTypeAndPathToSp :: (String, FilePath) -> Maybe (FileOrDirPathRelativeTo ProjectRootDir)
    fromTypeAndPathToSp (label, fp)
      | label == fileFsEntityLabel = Left <$> SP.parseRelFile fp
      | label == dirFsEntityLabel = Right <$> SP.parseRelDir fp
      | otherwise =
          error $
            "Found different file path type! Expected one of: ["
              ++ fileFsEntityLabel
              ++ ","
              ++ dirFsEntityLabel
              ++ "]. This should never happen!"

writeChecksumFile :: Path' Abs (Dir ProjectRootDir) -> RelPathsToChecksums -> IO ()
writeChecksumFile dstDir relativePathsToChecksums = do
  let typeAndPathAndChecksums = first fromSpToTypeAndPath <$> relativePathsToChecksums
  let sortedTypeAndPathAndChecksums = sortBy (\((_, p1), _) ((_, p2), _) -> compare p1 p2) typeAndPathAndChecksums
  let json = AesonPretty.encodePretty sortedTypeAndPathAndChecksums
  BSL.writeFile (SP.fromAbsFile $ dstDir </> checksumFileInProjectRoot) json
  where
    fromSpToTypeAndPath :: FileOrDirPathRelativeTo ProjectRootDir -> (String, FilePath)
    fromSpToTypeAndPath (Left fileSP) = (fileFsEntityLabel, SP.fromRelFile fileSP)
    fromSpToTypeAndPath (Right dirSP) = (dirFsEntityLabel, SP.fromRelDir dirSP)

removeFromChecksumFile :: Path' Abs (Dir ProjectRootDir) -> [FileOrDirPathRelativeTo ProjectRootDir] -> IO ()
removeFromChecksumFile dstDir pathsToRemove = do
  maybePathsToChecksums <- readChecksumFile dstDir
  case maybePathsToChecksums of
    Nothing -> return ()
    Just pathsToChecksums -> do
      writeChecksumFile dstDir $ filter ((`notElem` pathsToRemove) . fst) pathsToChecksums

fileFsEntityLabel :: String
fileFsEntityLabel = "file"

dirFsEntityLabel :: String
dirFsEntityLabel = "dir"

deleteFilesAndDirs :: Path' Abs (Dir ProjectRootDir) -> [FileOrDirPathRelativeTo ProjectRootDir] -> IO ()
deleteFilesAndDirs dstDir filesAndDirs = do
  let absFileFPs = map (\relSP -> SP.fromAbsFile $ dstDir </> relSP) (lefts filesAndDirs)
  mapM_ removeFile absFileFPs
  let absDirFPs = map (\relSP -> SP.fromAbsDir $ dstDir </> relSP) (rights filesAndDirs)
  mapM_ removeDirectoryRecursive absDirFPs
