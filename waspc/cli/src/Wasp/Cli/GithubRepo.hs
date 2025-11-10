module Wasp.Cli.GithubRepo where

import Control.Exception (try)
import Data.Aeson (FromJSON)
import Data.Functor ((<&>))
import Data.List (intercalate)
import qualified Network.HTTP.Simple as HTTP
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

fetchRepoFileContents :: (FromJSON a) => GithubRepoRef -> String -> IO (Either String a)
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
