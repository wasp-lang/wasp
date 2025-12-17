module Wasp.Cli.GithubRepo where

import Data.List (intercalate)
import StrongPath (Abs, Dir, Path', Rel)
import Wasp.Cli.Archive (fetchArchiveAndCopySubdirToDisk)
import Wasp.Cli.Util.Http (checkUrlExists)

data GithubRepoRef = GithubRepoRef
  { _repoOwner :: GithubRepoOwner,
    _repoName :: GithubRepoName,
    -- Which point in repo history to download (a branch or commit hash).
    _repoTagName :: GithubRepoReferenceName
  }
  deriving (Show, Eq)

type GithubRepoOwner = String

type GithubRepoName = String

type GithubRepoReferenceName = String

type GithubReleaseArchiveName = String

checkGitHubReleaseExists :: GithubRepoRef -> IO Bool
checkGitHubReleaseExists githubRepoRef =
  checkUrlExists $ getGithubReleaseURL githubRepoRef
  where
    getGithubReleaseURL :: GithubRepoRef -> String
    getGithubReleaseURL
      GithubRepoRef
        { _repoName = repoName,
          _repoOwner = repoOwner,
          _repoTagName = repoTagName
        } =
        intercalate "/" ["https://github.com", repoOwner, repoName, "releases", "tag", repoTagName]

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
          _repoTagName = repoTagName
        }
      assetName' =
        intercalate "/" ["https://github.com", repoOwner, repoName, "releases", "download", repoTagName, assetName']
