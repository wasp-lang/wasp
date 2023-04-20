module Wasp.Cli.Command.CreateNewProject.Templates
  ( getStarterTemplates,
    StarterTemplateNames,
    templatesToList,
    isValidTemplateName,
  )
where

import Control.Exception (try)
import Data.Aeson (FromJSON, parseJSON, withObject, (.:))
import qualified Network.HTTP.Simple as HTTP

data StarterTemplateNames = StarterTemplateNames [String]

getStarterTemplates :: IO (Maybe StarterTemplateNames)
getStarterTemplates = do
  -- Github returns 403 if we don't specify user-agent.
  let request = HTTP.addRequestHeader "User-Agent" "wasp-lang/wasp" templatesRepoInfoURL
  responseOrException <- try $ HTTP.httpJSONEither request
  return $ case responseOrException of
    Right response -> extractTemplateNames . HTTP.getResponseBody $ response
    Left (_ :: HTTP.HttpException) -> Nothing
  where
    templatesRepoInfoURL :: HTTP.Request
    templatesRepoInfoURL = "https://api.github.com/repos/wasp-lang/starters/git/trees/main"

    -- Each folder in the "wasp-lang/starters" repo is a template.
    extractTemplateNames :: Either HTTP.JSONException RepoInfo -> Maybe StarterTemplateNames
    extractTemplateNames (Left _) = Nothing
    extractTemplateNames (Right body) = Just . StarterTemplateNames . map _path . filter isFolder $ _objects body

    isFolder :: RepoObject -> Bool
    isFolder = (== Folder) . _type

templatesToList :: StarterTemplateNames -> [String]
templatesToList (StarterTemplateNames templates) = templates

isValidTemplateName :: String -> StarterTemplateNames -> Bool
isValidTemplateName templateName (StarterTemplateNames templates) = templateName `elem` templates

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
