module Wasp.Cli.Command.CreateNewProject.StarterTemplates
  ( getStarterTemplates,
    StarterTemplate (..),
    TemplateMetadata (..),
    findTemplateByString,
    defaultStarterTemplate,
  )
where

import Data.Either (fromRight)
import Data.Foldable (find)
import qualified Wasp.Cli.Command.CreateNewProject.StarterTemplates.Remote.Github as Github
import qualified Wasp.Cli.Interactive as Interactive

data StarterTemplate = RemoteStarterTemplate TemplateMetadata | LocalStarterTemplate TemplateMetadata
  deriving (Eq)

data TemplateMetadata = TemplateMetadata
  { _name :: String,
    _path :: String,
    _description :: String
  }
  deriving (Eq, Show)

instance Show StarterTemplate where
  show (RemoteStarterTemplate TemplateMetadata {_name = title}) = title
  show (LocalStarterTemplate TemplateMetadata {_name = title}) = title

instance Interactive.Option StarterTemplate where
  showOption = show
  showOptionDescription (RemoteStarterTemplate TemplateMetadata {_description = description}) = Just description
  showOptionDescription (LocalStarterTemplate TemplateMetadata {_description = description}) = Just description

getStarterTemplates :: IO [StarterTemplate]
getStarterTemplates = do
  remoteTemplates <- fromRight [] <$> fetchRemoteStarterTemplates
  return $ localTemplates ++ remoteTemplates

fetchRemoteStarterTemplates :: IO (Either String [StarterTemplate])
fetchRemoteStarterTemplates = do
  fmap extractTemplateNames <$> Github.fetchRemoteTemplatesGithubData
  where
    extractTemplateNames :: [Github.RemoteTemplateGithubData] -> [StarterTemplate]
    -- Each folder in the repo is a template.
    extractTemplateNames =
      map
        ( \metadata ->
            RemoteStarterTemplate $
              TemplateMetadata
                { _name = Github._name metadata,
                  _path = Github._path metadata,
                  _description = Github._description metadata
                }
        )

localTemplates :: [StarterTemplate]
localTemplates = [defaultStarterTemplate]

defaultStarterTemplate :: StarterTemplate
defaultStarterTemplate = LocalStarterTemplate $ TemplateMetadata {_name = "basic", _path = "basic", _description = "Simple starter template with a single page."}

findTemplateByString :: [StarterTemplate] -> String -> Maybe StarterTemplate
findTemplateByString templates query = find ((== query) . show) templates
