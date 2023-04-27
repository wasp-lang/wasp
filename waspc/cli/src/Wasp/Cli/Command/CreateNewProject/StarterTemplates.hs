module Wasp.Cli.Command.CreateNewProject.StarterTemplates
  ( getStarterTemplateNames,
    StarterTemplateName (..),
    findTemplateNameByString,
    defaultStarterTemplateName,
  )
where

import Data.Either (fromRight)
import Data.Foldable (find)
import Wasp.Cli.Command.CreateNewProject.StarterTemplates.Github (starterTemplateGithubRepo)
import Wasp.Cli.GithubRepo (fetchGithubRepoInfo)
import qualified Wasp.Cli.GithubRepo as GR

data StarterTemplateName = RemoteTemplate String | LocalTemplate String
  deriving (Eq)

instance Show StarterTemplateName where
  show (RemoteTemplate templateName) = templateName
  show (LocalTemplate templateName) = templateName

getStarterTemplateNames :: IO [StarterTemplateName]
getStarterTemplateNames = do
  remoteTemplates <- fromRight [] <$> fetchRemoteStarterTemplateNames
  return $ localTemplates ++ remoteTemplates

fetchRemoteStarterTemplateNames :: IO (Either String [StarterTemplateName])
fetchRemoteStarterTemplateNames =
  fmap extractTemplateNames <$> fetchGithubRepoInfo starterTemplateGithubRepo
  where
    extractTemplateNames :: GR.RepoInfo -> [StarterTemplateName]
    -- Each folder in the repo is a template.
    extractTemplateNames = map (RemoteTemplate . GR._path) . filter ((== GR.Folder) . GR._type) . GR._objects

localTemplates :: [StarterTemplateName]
localTemplates = [defaultStarterTemplateName]

defaultStarterTemplateName :: StarterTemplateName
defaultStarterTemplateName = LocalTemplate "basic"

findTemplateNameByString :: [StarterTemplateName] -> String -> Maybe StarterTemplateName
findTemplateNameByString templateNames templateNameString = find (\templateName -> show templateName == templateNameString) templateNames
