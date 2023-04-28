{-# LANGUAGE OverloadedStrings #-}

module Wasp.Cli.GithubRepo where

import Control.Exception (try)
import Data.Aeson
  ( FromJSON,
    parseJSON,
    withObject,
    (.:),
  )
import Data.Functor ((<&>))
import Data.List (intercalate)
import Data.Maybe (fromJust, maybeToList)
import qualified Network.HTTP.Simple as HTTP
import StrongPath (Abs, Dir, Path', Rel, (</>))
import qualified StrongPath as SP
import Wasp.Cli.Archive (fetchArchiveAndCopySubdirToDisk)

data GithubRepoRef = GithubRepoRef
  { _repoOwner :: GithubRepoOwner,
    _repoName :: GithubRepoName,
    -- Which point in repo history to download (a branch or commit hash).
    _repoReferenceName :: GithubRepoReferenceName
  }
  deriving (Show, Eq)

type GithubRepoOwner = String

type GithubRepoName = String

type GithubRepoReferenceName = String

fetchFolderFromGithubRepoToDisk ::
  GithubRepoRef ->
  String ->
  Path' Abs (Dir d) ->
  IO (Either String ())
fetchFolderFromGithubRepoToDisk githubRepoRef targetRepoFolderName destinationOnDisk = do
  let downloadUrl = getGithubRepoArchiveDownloadURL githubRepoRef
      targetRepoFolderPathInArchive = mapFolderInRepoToFolderPathInGithubArchive githubRepoRef $ fromJust . SP.parseRelDir $ targetRepoFolderName

  fetchArchiveAndCopySubdirToDisk downloadUrl targetRepoFolderPathInArchive destinationOnDisk
  where
    getGithubRepoArchiveDownloadURL :: GithubRepoRef -> String
    getGithubRepoArchiveDownloadURL
      GithubRepoRef
        { _repoName = repoName,
          _repoOwner = repoOwner,
          _repoReferenceName = repoReferenceName
        } = intercalate "/" ["https://github.com", repoOwner, repoName, "archive", downloadArchiveName]
        where
          downloadArchiveName = repoReferenceName ++ ".tar.gz"

    mapFolderInRepoToFolderPathInGithubArchive ::
      forall r archiveInnerDir targetDir.
      GithubRepoRef ->
      Path' (Rel archiveInnerDir) (Dir targetDir) ->
      Path' (Rel r) (Dir targetDir)
    mapFolderInRepoToFolderPathInGithubArchive githubRepo targetFolderPath = githubRepoArchiveRootFolderName </> targetFolderPath
      where
        -- Github repo tars have a root folder that is named after the repo
        -- name and the reference (branch or tag).
        githubRepoArchiveRootFolderName :: Path' (Rel r) (Dir archiveInnerDir)
        githubRepoArchiveRootFolderName = fromJust . SP.parseRelDir $ _repoName githubRepo ++ "-" ++ _repoReferenceName githubRepo

fetchRepoRootFolderContents :: GithubRepoRef -> IO (Either String RepoFolderContents)
fetchRepoRootFolderContents githubRepo = fetchRepoFolderContents githubRepo Nothing

fetchRepoFolderContents :: GithubRepoRef -> Maybe String -> IO (Either String RepoFolderContents)
fetchRepoFolderContents githubRepo pathToFolderInRepo = do
  try (HTTP.httpJSONEither ghRepoInfoRequest) <&> \case
    Right response -> either (Left . show) Right $ HTTP.getResponseBody response
    Left (e :: HTTP.HttpException) -> Left $ show e
  where
    ghRepoInfoRequest =
      -- Github returns 403 if we don't specify user-agent.
      HTTP.addRequestHeader "User-Agent" "wasp-lang/wasp" $ HTTP.parseRequest_ apiURL
    apiURL = intercalate "/" $ ["https://api.github.com/repos", _repoOwner githubRepo, _repoName githubRepo, "contents"] ++ maybeToList pathToFolderInRepo

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
