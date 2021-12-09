{-# LANGUAGE TupleSections #-}

module Wasp.Generator.WriteFileDrafts
  ( writeFileDrafts,
    fileDraftsToWriteAndFilesToDelete,
  )
where

import Control.Arrow (Arrow (first))
import Control.Monad (unless)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encode.Pretty as AesonPretty
import qualified Data.ByteString.Lazy as BSL
import Data.Either (lefts, rights)
import Data.List (find)
import Data.Maybe (isJust, isNothing)
import qualified Data.Text
import qualified Data.Text.IO
import Data.Time.Clock
import qualified Data.Version
import qualified Paths_waspc
import StrongPath (Abs, Dir, File', Path', Rel, relfile, (</>))
import qualified StrongPath as SP
import System.Directory (removeDirectoryRecursive, removeFile)
import System.IO.Error (isDoesNotExistError)
import UnliftIO.Exception (catch, throwIO)
import Wasp.Generator.Common (ProjectRootDir)
import Wasp.Generator.FileDraft (FileDraft, write)
import Wasp.Generator.FileDraft.Writeable (FileOrDirPathRelativeTo, Writeable (getChecksum, getDstPath))
import Wasp.Util (Checksum)

type RelPathsToChecksums = [(FileOrDirPathRelativeTo ProjectRootDir, Checksum)]

checksumFileInProjectRoot :: Path' (Rel ProjectRootDir) File'
checksumFileInProjectRoot = [relfile|.waspchecksums|]

-- | Writes given file drafts to disk, in the provided destination directory.
-- Also makes sure to remove any redundant file drafts that have been left on the disk from before.
-- It is smart when writing, so it doesn't write file drafts that are already written on the disk from before.
writeFileDrafts :: Path' Abs (Dir ProjectRootDir) -> [FileDraft] -> IO ()
writeFileDrafts dstDir fileDrafts = do
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

  -- TODO: Use these below in writeChecksumFile, so it doesn't have to calculate the checksums again on its own?
  fileDraftsWithChecksums <- mapM (\fd -> (fd,) <$> getChecksum fd) fileDrafts

  let (fileDraftsToWrite, filesToDelete) = fileDraftsToWriteAndFilesToDelete maybePathsToChecksums fileDraftsWithChecksums

  mapM_ (write dstDir) fileDraftsToWrite
  deleteFilesAndDirs dstDir filesToDelete
  writeDotWaspInfo dstDir
  writeChecksumFile dstDir fileDrafts

fileDraftsToWriteAndFilesToDelete ::
  Maybe RelPathsToChecksums ->
  [(FileDraft, Checksum)] ->
  ([FileDraft], [FileOrDirPathRelativeTo ProjectRootDir])
fileDraftsToWriteAndFilesToDelete maybePathsToChecksums fileDraftsWithChecksums =
  let fileDrafts = map fst fileDraftsWithChecksums
   in case maybePathsToChecksums of
        Nothing -> (fileDrafts, [])
        Just pathsToChecksums ->
          ( getNewFileDrafts fileDrafts pathsToChecksums
              ++ getChangedFileDrafts pathsToChecksums fileDraftsWithChecksums,
            getRedundantGeneratedFiles fileDrafts pathsToChecksums
          )

-- TODO: test
getNewFileDrafts :: [FileDraft] -> RelPathsToChecksums -> [FileDraft]
getNewFileDrafts fileDrafts pathsToChecksums =
  filter (\draft -> isNothing $ lookup (getDstPath draft) pathsToChecksums) fileDrafts

-- TODO: test
getChangedFileDrafts :: RelPathsToChecksums -> [(FileDraft, Checksum)] -> [FileDraft]
getChangedFileDrafts oldPathsToChecksums fileDraftsWithChecksums =
  fst <$> filter alreadyExistsWithDifferentChecksum fileDraftsWithChecksums
  where
    alreadyExistsWithDifferentChecksum :: (FileDraft, Checksum) -> Bool
    alreadyExistsWithDifferentChecksum (fd, newChecksum) =
      let newPath = getDstPath fd
       in isJust $
            find
              ( \(oldPath, oldChecksum) ->
                  oldPath == newPath && oldChecksum /= newChecksum
              )
              oldPathsToChecksums

-- TODO: test
getRedundantGeneratedFiles :: [FileDraft] -> RelPathsToChecksums -> [FileOrDirPathRelativeTo ProjectRootDir]
getRedundantGeneratedFiles fileDrafts pathsToChecksums =
  let fileDraftPaths = map getDstPath fileDrafts
   in fst <$> filter (\(path, _) -> path `notElem` fileDraftPaths) pathsToChecksums

-- TODO: Comment on when this function returns Nothing.
-- TODO: Capture the return type (paths, checksums) into a type or newtype?
readChecksumFile :: Path' Abs (Dir ProjectRootDir) -> IO (Maybe RelPathsToChecksums)
readChecksumFile dstDir = do
  maybeContents <-
    (Just <$> BSL.readFile checksumFP)
      `catch` (\e -> if isDoesNotExistError e then return Nothing else throwIO e)
  return $ do
    contents <- maybeContents
    typeAndPathAndChecksums <- Aeson.decode contents :: Maybe [((String, FilePath), Checksum)]
    sequence $ (\(typeAndPath, cs) -> (,cs) <$> fromTypeAndPathToSp typeAndPath) <$> typeAndPathAndChecksums
  where
    checksumFP = SP.fromAbsFile $ dstDir </> checksumFileInProjectRoot

    -- TODO: repeating strings file/dir; extract higher up in this file?
    fromTypeAndPathToSp :: (String, FilePath) -> Maybe (FileOrDirPathRelativeTo ProjectRootDir)
    fromTypeAndPathToSp ("file", fp) = Left <$> SP.parseRelFile fp
    fromTypeAndPathToSp ("dir", fp) = Right <$> SP.parseRelDir fp
    fromTypeAndPathToSp _ = error "Found different file path type! Expected `file` or `dir`. This should never happen!"

writeChecksumFile :: Path' Abs (Dir ProjectRootDir) -> [FileDraft] -> IO ()
writeChecksumFile dstDir drafts = do
  relativePathsToChecksums <- mapM (\fd -> (getDstPath fd,) <$> getChecksum fd) drafts
  let res2 = first fromSpToTypeAndPath <$> relativePathsToChecksums
  let json = AesonPretty.encodePretty res2
  BSL.writeFile (SP.fromAbsFile $ dstDir </> checksumFileInProjectRoot) json
  where
    fromSpToTypeAndPath :: FileOrDirPathRelativeTo ProjectRootDir -> (String, FilePath)
    fromSpToTypeAndPath (Left fileSP) = ("file", SP.fromRelFile fileSP)
    fromSpToTypeAndPath (Right dirSP) = ("dir", SP.fromRelDir dirSP)

deleteFilesAndDirs :: Path' Abs (Dir ProjectRootDir) -> [FileOrDirPathRelativeTo ProjectRootDir] -> IO ()
deleteFilesAndDirs dstDir filesAndDirs = do
  let absFileFPs = map (\relSP -> SP.fromAbsFile $ dstDir </> relSP) (lefts filesAndDirs)
  mapM_ removeFile absFileFPs
  let absDirFPs = map (\relSP -> SP.fromAbsDir $ dstDir </> relSP) (rights filesAndDirs)
  mapM_ removeDirectoryRecursive absDirFPs

-- | Writes .waspinfo, which contains some basic metadata about how/when wasp generated the code.
writeDotWaspInfo :: Path' Abs (Dir ProjectRootDir) -> IO ()
writeDotWaspInfo dstDir = do
  currentTime <- getCurrentTime
  let version = Data.Version.showVersion Paths_waspc.version
  let content = "Generated on " ++ show currentTime ++ " by waspc version " ++ show version ++ " ."
  let dstPath = dstDir </> [relfile|.waspinfo|]
  Data.Text.IO.writeFile (SP.toFilePath dstPath) (Data.Text.pack content)
