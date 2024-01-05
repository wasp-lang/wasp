module Wasp.Cli.Command.CreateNewProject.StarterTemplates.GhStartersRepo.Github where

import Data.Aeson (FromJSON (parseJSON), withObject, (.:))
import Wasp.Cli.GithubRepo (GithubRepoRef (..))
import qualified Wasp.Cli.GithubRepo as GR

startersRepo :: GithubRepoRef
startersRepo =
  GithubRepoRef
    { _repoOwner = "wasp-lang",
      _repoName = "starters",
      _repoReferenceName = "main"
    }

fetchTemplatesGithubData :: IO (Either String [TemplateGithubData])
fetchTemplatesGithubData = GR.fetchRepoFileContents startersRepo "templates.json"

data TemplateGithubData = TemplateGithubData
  { _name :: String,
    _description :: String,
    _path :: String
  }
  deriving (Show, Eq)

instance FromJSON TemplateGithubData where
  parseJSON = withObject "TemplateGithubData" $ \obj ->
    TemplateGithubData
      <$> obj
      .: "name"
      <*> obj
      .: "description"
      <*> obj
      .: "path"
