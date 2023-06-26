{-# LANGUAGE TupleSections #-}

module Wasp.Cli.Command.CreateNewProject.StarterTemplates
  ( getStarterTemplates,
    StarterTemplate (..),
    DirBasedTemplateMetadata (..),
    findTemplateByString,
    defaultStarterTemplate,
    readWaspProjectSkeletonFiles,
  )
where

import Data.Either (fromRight)
import Data.Foldable (find)
import Data.Text (Text)
import StrongPath (File', Path, Rel, System, reldir, (</>))
import qualified Wasp.Cli.Command.CreateNewProject.StarterTemplates.Remote.Github as Github
import Wasp.Cli.Common (WaspProjectDir)
import qualified Wasp.Cli.Interactive as Interactive
import qualified Wasp.Data as Data
import Wasp.Util.IO (listDirectoryDeep, readFileStrict)

data StarterTemplate
  = RemoteStarterTemplate DirBasedTemplateMetadata
  | LocalStarterTemplate DirBasedTemplateMetadata
  | AiGeneratedStarterTemplate
  deriving (Eq)

data DirBasedTemplateMetadata = DirBasedTemplateMetadata
  { _name :: String,
    _path :: String, -- Path to a directory containing template files.
    _description :: String
  }
  deriving (Eq, Show)

instance Show StarterTemplate where
  show (RemoteStarterTemplate metadata) = _name metadata
  show (LocalStarterTemplate metadata) = _name metadata
  show AiGeneratedStarterTemplate = "ai-generated"

instance Interactive.Option StarterTemplate where
  showOption = show
  showOptionDescription (RemoteStarterTemplate metadata) = Just $ _description metadata
  showOptionDescription (LocalStarterTemplate metadata) = Just $ _description metadata
  showOptionDescription AiGeneratedStarterTemplate =
    Just "[experimental] Describe an app in a couple of sentences and have ChatGPT generate initial code for you."

getStarterTemplates :: IO [StarterTemplate]
getStarterTemplates = do
  remoteTemplates <- fromRight [] <$> fetchRemoteStarterTemplates
  return $ localTemplates ++ remoteTemplates ++ [AiGeneratedStarterTemplate]

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
              DirBasedTemplateMetadata
                { _name = Github._name metadata,
                  _path = Github._path metadata,
                  _description = Github._description metadata
                }
        )

localTemplates :: [StarterTemplate]
localTemplates = [defaultStarterTemplate]

defaultStarterTemplate :: StarterTemplate
defaultStarterTemplate =
  LocalStarterTemplate $
    DirBasedTemplateMetadata
      { _name = "basic",
        _path = "basic",
        _description = "Simple starter template with a single page."
      }

findTemplateByString :: [StarterTemplate] -> String -> Maybe StarterTemplate
findTemplateByString templates query = find ((== query) . show) templates

readWaspProjectSkeletonFiles :: IO [(Path System (Rel WaspProjectDir) File', Text)]
readWaspProjectSkeletonFiles = do
  skeletonFilesDir <- (</> [reldir|Cli/templates/skeleton|]) <$> Data.getAbsDataDirPath
  skeletonFilePaths <- listDirectoryDeep skeletonFilesDir
  mapM (\path -> (path,) <$> readFileStrict (skeletonFilesDir </> path)) skeletonFilePaths
