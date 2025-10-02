module Wasp.Cli.Archive where

import qualified Codec.Archive.Tar as Tar
import qualified Codec.Compression.GZip as GZip
import Control.Exception (SomeException, try)
import qualified Data.ByteString.Lazy as BL
import Data.Functor ((<&>))
import Data.Maybe (fromJust)
import Network.HTTP.Conduit (simpleHttp)
import Path.IO (copyDirRecur)
import StrongPath (Abs, Dir, File, Path', Rel, reldir, (</>))
import qualified StrongPath as SP
import StrongPath.Path (toPathAbsDir)
import System.FilePath (takeFileName)
import Wasp.Cli.FileSystem (withTempDir)

fetchArchiveAndCopySubdirToDisk ::
  String ->
  Path' (Rel extracted) (Dir subdir) ->
  Path' Abs (Dir d) ->
  IO (Either String ())
fetchArchiveAndCopySubdirToDisk archiveDownloadUrl targetFolder destinationOnDisk = do
  try
    ( withTempDir $ \tempDir -> do
        let archiveName = takeFileName archiveDownloadUrl
            archiveDownloadPath = tempDir </> (fromJust . SP.parseRelFile $ archiveName)
            archiveUnpackPath = tempDir </> extractedContentsDir
            targetFolderInArchivePath = archiveUnpackPath </> targetFolder

        downloadFile archiveDownloadUrl archiveDownloadPath
        unpackArchive archiveDownloadPath archiveUnpackPath
        copyDirRecur (toPathAbsDir targetFolderInArchivePath) (toPathAbsDir destinationOnDisk)
    )
    <&> either showException Right
  where
    extractedContentsDir :: Path' (Rel tempDir) (Dir extracted)
    extractedContentsDir = [reldir|extracted|]

    downloadFile :: String -> Path' Abs (File f) -> IO ()
    downloadFile downloadUrl destinationPath =
      simpleHttp downloadUrl >>= BL.writeFile (SP.fromAbsFile destinationPath)

    unpackArchive :: Path' Abs (File f) -> Path' Abs (Dir d) -> IO ()
    unpackArchive sourceFile destinationDir =
      Tar.unpack (SP.fromAbsDir destinationDir) . Tar.read . GZip.decompress
        =<< BL.readFile (SP.fromAbsFile sourceFile)

    showException :: SomeException -> Either String ()
    showException = Left . show
