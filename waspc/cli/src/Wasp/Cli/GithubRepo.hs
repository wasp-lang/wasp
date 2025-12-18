module Wasp.Cli.GithubRepo where

import Data.List (intercalate)
import StrongPath (Abs, Dir, Path', Rel)
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

type GithubReleaseArchiveName = String

fetchFolderFromGithubReleaseArchiveToDisk ::
  GithubRepoRef ->
  GithubReleaseArchiveName ->
  Path' (Rel archiveRoot) (Dir folderInArchive) ->
  Path' Abs (Dir destinationDir) ->
  IO (Either String ())
fetchFolderFromGithubReleaseArchiveToDisk githubRepoRef assetName folderInArchiveRoot destinationOnDisk = do
  let downloadUrl = getGithubReleaseArchiveDownloadURL githubRepoRef assetName

  fetchArchiveAndCopySubdirToDisk downloadUrl folderInArchiveRoot destinationOnDisk
  where
    getGithubReleaseArchiveDownloadURL :: GithubRepoRef -> GithubReleaseArchiveName -> String
    getGithubReleaseArchiveDownloadURL
      GithubRepoRef
        { _repoName = repoName,
          _repoOwner = repoOwner,
          _repoReferenceName = repoReferenceName
        }
      assetName' =
        intercalate "/" ["https://github.com", repoOwner, repoName, "releases", "download", repoReferenceName, assetName']
