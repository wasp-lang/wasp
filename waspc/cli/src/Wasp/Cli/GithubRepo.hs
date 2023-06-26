{-# LANGUAGE OverloadedStrings #-}

module Wasp.Cli.GithubRepo where

import Control.Exception (try)
import Data.Aeson (FromJSON)
import Data.Functor ((<&>))
import Data.List (intercalate)
import Data.Maybe (fromJust)
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
  Path' (Rel repoRoot) (Dir folderInRepo) ->
  Path' Abs (Dir destinationDir) ->
  IO (Either String ())
fetchFolderFromGithubRepoToDisk githubRepoRef folderInRepoRoot destinationOnDisk = do
  let downloadUrl = getGithubRepoArchiveDownloadURL githubRepoRef
      folderInArchiveRoot = mapFolderPathInRepoToFolderPathInGithubArchive githubRepoRef folderInRepoRoot

  fetchArchiveAndCopySubdirToDisk downloadUrl folderInArchiveRoot destinationOnDisk
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

    mapFolderPathInRepoToFolderPathInGithubArchive ::
      forall archiveInnerDir targetDir archiveRoot.
      GithubRepoRef ->
      Path' (Rel archiveInnerDir) (Dir targetDir) ->
      Path' (Rel archiveRoot) (Dir targetDir)
    mapFolderPathInRepoToFolderPathInGithubArchive
      GithubRepoRef
        { _repoName = repoName,
          _repoReferenceName = repoReferenceName
        }
      targetFolderPath = githubRepoArchiveRootFolderName </> targetFolderPath
        where
          -- Github repo tars have a root folder that is named after the repo
          -- name and the reference (branch or tag).
          githubRepoArchiveRootFolderName :: Path' (Rel archiveRoot) (Dir archiveInnerDir)
          githubRepoArchiveRootFolderName = fromJust . SP.parseRelDir $ repoName ++ "-" ++ repoReferenceName

fetchRepoFileContents :: FromJSON a => GithubRepoRef -> String -> IO (Either String a)
fetchRepoFileContents githubRepo filePath = do
  try (HTTP.httpJSONEither ghRepoInfoRequest) <&> \case
    Right response -> either (Left . show) Right $ HTTP.getResponseBody response
    Left (e :: HTTP.HttpException) -> Left $ show e
  where
    ghRepoInfoRequest = mkGithubApiRequest apiURL
    apiURL = intercalate "/" ["https://raw.githubusercontent.com", _repoOwner githubRepo, _repoName githubRepo, _repoReferenceName githubRepo, filePath]

-- Github returns 403 if we don't specify a user-agent.
mkGithubApiRequest :: String -> HTTP.Request
mkGithubApiRequest url = HTTP.addRequestHeader "User-Agent" "wasp-lang/wasp" $ HTTP.parseRequest_ url
