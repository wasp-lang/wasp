module Wasp.Cli.Command.CreateNewProject.StarterTemplates.Remote.Github where

import Data.Aeson (FromJSON (parseJSON), withObject, (.:))
import Wasp.Cli.GithubRepo (GithubRepoRef (..))
import qualified Wasp.Cli.GithubRepo as GR

starterTemplateGithubRepo :: GithubRepoRef
starterTemplateGithubRepo =
  GithubRepoRef
    { _repoOwner = "wasp-lang",
      _repoName = "starters",
      _repoReferenceName = "main"
    }

starterTemplatesDataGithubFilePath :: String
starterTemplatesDataGithubFilePath = "templates.json"

fetchRemoteTemplatesGithubData :: IO (Either String [RemoteTemplateGithubData])
fetchRemoteTemplatesGithubData = GR.fetchRepoFileContents starterTemplateGithubRepo starterTemplatesDataGithubFilePath

data RemoteTemplateGithubData = RemoteTemplateGithubData
  { _name :: String,
    _description :: String,
    _path :: String
  }
  deriving (Show, Eq)

instance FromJSON RemoteTemplateGithubData where
  parseJSON = withObject "RemoteTemplateGithubData" $ \obj ->
    RemoteTemplateGithubData
      <$> obj
      .: "name"
      <*> obj
      .: "description"
      <*> obj
      .: "path"
