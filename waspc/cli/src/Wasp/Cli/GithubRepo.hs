module Wasp.Cli.GithubRepo where

import Control.Exception (try)
import Data.Aeson
  ( FromJSON,
    parseJSON,
    withObject,
    (.:),
  )
import Data.Functor ((<&>))
import qualified Network.HTTP.Simple as HTTP
import StrongPath (Abs, Dir, Path')
import Text.Printf (printf)

type GithubRepo = (GithubRepoOwner, GithubRepoName)

type GithubRepoOwner = String

type GithubRepoName = String

fetchFolderFromGithubRepoToDisk :: GithubRepo -> String -> Path' Abs (Dir d) -> IO ()
fetchFolderFromGithubRepoToDisk githubRepo folderInRepo destinationOnDisk = undefined

fetchGithubRepoInfo :: GithubRepo -> IO (Either String RepoInfo)
fetchGithubRepoInfo (org, name) = do
  try (HTTP.httpJSONEither ghRepoInfoRequest) <&> \case
    Right response -> either (Left . show) Right $ HTTP.getResponseBody response
    Left (e :: HTTP.HttpException) -> Left $ show e
  where
    ghRepoInfoRequest =
      -- Github returns 403 if we don't specify user-agent.
      HTTP.addRequestHeader "User-Agent" "wasp-lang/wasp" $
        HTTP.parseRequest_ $
          printf "https://api.github.com/repos/%s/%s/git/trees/main" org name

data RepoInfo = RepoInfo
  { _objects :: [RepoObject]
  }
  deriving (Show)

data RepoObject = RepoObject
  { _path :: Path,
    _type :: RepoObjectType
  }
  deriving (Show)

data RepoObjectType = Folder | File
  deriving (Show, Eq)

type Path = String

instance FromJSON RepoInfo where
  parseJSON = withObject "RepoInfo" $ \o -> do
    objects <- o .: "tree"
    return
      RepoInfo
        { _objects = objects
        }

instance FromJSON RepoObject where
  parseJSON = withObject "RepoObject" $ \o -> do
    path <- o .: "path"
    type_ <- o .: "type"
    return
      RepoObject
        { _path = path,
          _type = parseType type_
        }
    where
      parseType :: String -> RepoObjectType
      parseType = \case
        "tree" -> Folder
        "blob" -> File
        _ -> error "Unable to parse repo object type."
