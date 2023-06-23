module Wasp.Cli.Command.CreateNewProject.StarterTemplates
  ( getStarterTemplateInfos,
    StarterTemplateInfo (..),
    TemplateMetadata (..),
    findTemplateNameByString,
    defaultStarterTemplateInfo,
  )
where

import Data.Either (fromRight)
import Data.Foldable (find)
import qualified Wasp.Cli.Command.CreateNewProject.StarterTemplates.Remote.Github as Github
import qualified Wasp.Cli.Interactive as Interactive

data StarterTemplateInfo = RemoteStarterTemplate TemplateMetadata | LocalStarterTemplate TemplateMetadata
  deriving (Eq)

data TemplateMetadata = TemplateMetadata
  { _name :: String,
    _path :: String,
    _description :: String
  }
  deriving (Eq, Show)

instance Show StarterTemplateInfo where
  show (RemoteStarterTemplate TemplateMetadata {_name = title}) = title
  show (LocalStarterTemplate TemplateMetadata {_name = title}) = title

instance Interactive.Option StarterTemplateInfo where
  showOption = show
  showOptionDescription (RemoteStarterTemplate TemplateMetadata {_description = description}) = Just description
  showOptionDescription (LocalStarterTemplate TemplateMetadata {_description = description}) = Just description

getStarterTemplateInfos :: IO [StarterTemplateInfo]
getStarterTemplateInfos = do
  remoteTemplates <- fromRight [] <$> fetchRemoteStarterTemplateInfos
  return $ localTemplates ++ remoteTemplates

fetchRemoteStarterTemplateInfos :: IO (Either String [StarterTemplateInfo])
fetchRemoteStarterTemplateInfos = do
  fmap extractTemplateNames <$> Github.fetchRemoteTemplatesGithubData
  where
    extractTemplateNames :: [Github.RemoteTemplateGithubData] -> [StarterTemplateInfo]
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

localTemplates :: [StarterTemplateInfo]
localTemplates = [defaultStarterTemplateInfo]

defaultStarterTemplateInfo :: StarterTemplateInfo
defaultStarterTemplateInfo = LocalStarterTemplate $ TemplateMetadata {_name = "basic", _path = "basic", _description = "Simple starter template with a single page."}

findTemplateNameByString :: [StarterTemplateInfo] -> String -> Maybe StarterTemplateInfo
findTemplateNameByString templateNames query = find ((== query) . show) templateNames
