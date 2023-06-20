{-# LANGUAGE TupleSections #-}

module Wasp.Cli.Command.CreateNewProject.StarterTemplates
  ( getStarterTemplateNames,
    StarterTemplateName (..),
    findTemplateNameByString,
    defaultStarterTemplateName,
    readWaspProjectSkeletonFiles,
  )
where

import Data.Either (fromRight)
import Data.Foldable (find)
import Data.Text (Text)
import StrongPath (File', Path, Rel, System, reldir, (</>))
import Wasp.Cli.Command.CreateNewProject.StarterTemplates.Remote.Github (starterTemplateGithubRepo)
import Wasp.Cli.Common (WaspProjectDir)
import qualified Wasp.Cli.GithubRepo as GR
import qualified Wasp.Data as Data
import Wasp.Util.IO (listDirectoryDeep, readFileStrict)

data StarterTemplateName
  = RemoteStarterTemplate String
  | LocalStarterTemplate String
  | AiGeneratedStarterTemplate
  deriving (Eq)

instance Show StarterTemplateName where
  show (RemoteStarterTemplate templateName) = templateName
  show (LocalStarterTemplate templateName) = templateName
  show AiGeneratedStarterTemplate = "ai-generated (experimental)"

getStarterTemplateNames :: IO [StarterTemplateName]
getStarterTemplateNames = do
  remoteTemplates <- fromRight [] <$> fetchRemoteStarterTemplateNames
  return $ localTemplates ++ remoteTemplates ++ [AiGeneratedStarterTemplate]

fetchRemoteStarterTemplateNames :: IO (Either String [StarterTemplateName])
fetchRemoteStarterTemplateNames = do
  fmap extractTemplateNames <$> GR.fetchRepoRootFolderContents starterTemplateGithubRepo
  where
    extractTemplateNames :: GR.RepoFolderContents -> [StarterTemplateName]
    -- Each folder in the repo is a template.
    extractTemplateNames = map (RemoteStarterTemplate . GR._name) . filter ((== GR.Folder) . GR._type)

localTemplates :: [StarterTemplateName]
localTemplates = [defaultStarterTemplateName]

defaultStarterTemplateName :: StarterTemplateName
defaultStarterTemplateName = LocalStarterTemplate "basic"

findTemplateNameByString :: [StarterTemplateName] -> String -> Maybe StarterTemplateName
findTemplateNameByString templateNames query = find ((== query) . show) templateNames

readWaspProjectSkeletonFiles :: IO [(Path System (Rel WaspProjectDir) File', Text)]
readWaspProjectSkeletonFiles = do
  skeletonFilesDir <- (</> [reldir|Cli/templates/skeleton|]) <$> Data.getAbsDataDirPath
  skeletonFilePaths <- listDirectoryDeep skeletonFilesDir
  mapM (\path -> (path,) <$> readFileStrict (skeletonFilesDir </> path)) skeletonFilePaths
