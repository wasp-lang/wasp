{-# LANGUAGE OverloadedStrings #-}

module Wasp.Cli.GithubRepo where

import qualified Codec.Archive.Tar as Tar
import qualified Codec.Compression.GZip as GZip
import Control.Exception (SomeException, try)
import Data.Aeson
  ( FromJSON,
    parseJSON,
    withObject,
    (.:),
  )
import qualified Data.ByteString.Lazy as BL
import Data.Functor ((<&>))
import Data.List (intercalate)
import Data.Maybe (fromJust, maybeToList)
import Network.HTTP.Conduit (simpleHttp)
import qualified Network.HTTP.Simple as HTTP
import Path.IO (copyDirRecur, removeDirRecur)
import StrongPath (Abs, Dir, File, Path', (</>))
import qualified StrongPath as SP
import StrongPath.Path (toPathAbsDir)
import System.Directory (createDirectoryIfMissing)
import Wasp.Cli.FileSystem (getUserCacheDir)

type GithubRepo = (GithubRepoOwner, GithubRepoName)

type GithubRepoOwner = String

type GithubRepoName = String

fetchFolderFromGithubRepoToDisk ::
  forall d.
  GithubRepo ->
  String ->
  Path' Abs (Dir d) ->
  IO (Either String ())
fetchFolderFromGithubRepoToDisk (githubOrg, repoName) pathToFolderInRepo destinationOnDisk = do
  try
    (withTempDir ".wasp-templates-temp" downloadFolderToDisk)
    <&> either showException Right
  where
    downloadFolderToDisk :: Path' Abs (Dir d) -> IO ()
    downloadFolderToDisk tempDir = do
      downloadFile githubArchiveUrl tempDir archiveFileName >>= unpackArchive tempDir
      copyRepoFolderToDestination tempDir pathToFolderInRepo destinationOnDisk
      where
        githubArchiveUrl = intercalate "/" ["https://github.com", githubOrg, repoName, "archive", archiveFileName]
        archiveFileName = repoReference ++ ".tar.gz"

    -- Which point in repo history to download (a branch or commit hash)
    repoReference :: String
    repoReference = "main"

    downloadFile :: String -> Path' Abs (Dir d) -> String -> IO (Path' Abs (File f))
    downloadFile downloadUrl destinationDir fileName = do
      let destinationPath = destinationDir </> fromJust (SP.parseRelFile fileName)
      simpleHttp downloadUrl >>= BL.writeFile (SP.fromAbsFile destinationPath)
      return destinationPath

    unpackArchive :: Path' Abs (Dir d) -> Path' Abs (File f) -> IO ()
    unpackArchive destinationDir pathToTheTarFile = do
      Tar.unpack (SP.fromAbsDir destinationDir) . Tar.read . GZip.decompress =<< BL.readFile (SP.fromAbsFile pathToTheTarFile)

    copyRepoFolderToDestination :: Path' Abs (Dir d) -> String -> Path' Abs (Dir d) -> IO ()
    copyRepoFolderToDestination archiveRootDir pathToFolderInRepo' destinationOnDisk' = do
      copyDirRecur (toPathAbsDir pathToDownloadededFolder) (toPathAbsDir destinationOnDisk')
      where
        pathToDownloadededFolder = archiveRootDir </> rootArchiveFolder </> fromJust (SP.parseRelDir pathToFolderInRepo')
        -- Github repo tars have a root folder that is named after the repo name and the reference (branch or tag).
        rootArchiveFolder = fromJust (SP.parseRelDir $ repoName ++ "-" ++ repoReference)

    withTempDir :: FilePath -> (Path' Abs (Dir d) -> IO a) -> IO a
    withTempDir dirName action = do
      tempDir <- getUserCacheDir <&> (</> fromJust (SP.parseRelDir dirName))

      createDirectoryIfMissing True $ SP.fromAbsDir tempDir
      result <- action tempDir
      removeDirRecur (toPathAbsDir tempDir)

      return result

    showException :: SomeException -> Either String ()
    showException = Left . show

fetchRepoRootFolderContents :: GithubRepo -> IO (Either String RepoFolderContents)
fetchRepoRootFolderContents githubRepo = fetchRepoFolderContents githubRepo Nothing

fetchRepoFolderContents :: GithubRepo -> Maybe String -> IO (Either String RepoFolderContents)
fetchRepoFolderContents (org, name) pathToFolderInRepo = do
  try (HTTP.httpJSONEither ghRepoInfoRequest) <&> \case
    Right response -> either (Left . show) Right $ HTTP.getResponseBody response
    Left (e :: HTTP.HttpException) -> Left $ show e
  where
    ghRepoInfoRequest =
      -- Github returns 403 if we don't specify user-agent.
      HTTP.addRequestHeader "User-Agent" "wasp-lang/wasp" $ HTTP.parseRequest_ apiURL
    apiURL = intercalate "/" $ ["https://api.github.com/repos", org, name, "contents"] ++ maybeToList pathToFolderInRepo

type RepoFolderContents = [RepoObject]

data RepoObject = RepoObject
  { _name :: String,
    _type :: RepoObjectType,
    _downloadUrl :: Maybe String
  }
  deriving (Show)

data RepoObjectType = Folder | File
  deriving (Show, Eq)

instance FromJSON RepoObject where
  parseJSON = withObject "RepoObject" $ \o -> do
    name <- o .: "name"
    type_ <- o .: "type"
    downloadUrl <- o .: "download_url"
    return
      RepoObject
        { _name = name,
          _type = parseType type_,
          _downloadUrl = downloadUrl
        }
    where
      parseType :: String -> RepoObjectType
      parseType = \case
        "dir" -> Folder
        "file" -> File
        _ -> error "Unable to parse repo object type."
