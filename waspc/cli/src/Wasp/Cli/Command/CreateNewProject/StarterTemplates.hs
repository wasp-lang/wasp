module Wasp.Cli.Command.CreateNewProject.StarterTemplates
  ( getStarterTemplateNames,
    StarterTemplateName (..),
    findTemplateNameByString,
    defaultStarterTemplateName,
  )
where

import Control.Exception (try)
import Data.Aeson
  ( FromJSON,
    parseJSON,
    withObject,
    (.:),
  )
import Data.Either (fromRight)
import Data.Foldable (find)
import Data.Functor ((<&>))
import qualified Network.HTTP.Simple as HTTP
import Text.Printf (printf)
import Wasp.Cli.Command.CreateNewProject.StarterTemplates.Github (GithubRepo, starterTemplateGithubRepo)

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
  fmap extractTemplateNames <$> fetchGhRepoInfo starterTemplateGithubRepo
  where
    extractTemplateNames :: RepoInfo -> [StarterTemplateName]
    -- Each folder in the "wasp-lang/starters" repo is a template.
    extractTemplateNames = map (RemoteTemplate . _path) . filter ((== Folder) . _type) . _objects

localTemplates :: [StarterTemplateName]
localTemplates = [defaultStarterTemplateName]

defaultStarterTemplateName :: StarterTemplateName
defaultStarterTemplateName = LocalTemplate "basic"

findTemplateNameByString :: [StarterTemplateName] -> String -> Maybe StarterTemplateName
findTemplateNameByString templateNames templateNameString = find (\templateName -> show templateName == templateNameString) templateNames

fetchGhRepoInfo :: GithubRepo -> IO (Either String RepoInfo)
fetchGhRepoInfo (org, name) = do
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
