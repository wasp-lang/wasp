module Wasp.Cli.Command.CreateNewProject.Templates
  ( getStarterTemplateNames,
    StarterTemplateNames,
    StarterTemplateNamesFetchResult (..),
    templatesToList,
    isOneOfAvailableTemplateNames,
  )
where

import Control.Exception (try)
import Data.Aeson (FromJSON, parseJSON, withObject, (.:))
import qualified Network.HTTP.Simple as HTTP

data StarterTemplateNamesFetchResult = Success StarterTemplateNames | Failure

data StarterTemplateNames = StarterTemplateNames [String]

getStarterTemplateNames :: IO StarterTemplateNamesFetchResult
getStarterTemplateNames = do
  -- Github returns 403 if we don't specify user-agent.
  let request = HTTP.addRequestHeader "User-Agent" "wasp-lang/wasp" templatesRepoInfoURL
  responseOrException <- try $ HTTP.httpJSONEither request
  return $ case responseOrException of
    Right response -> extractTemplateNames . HTTP.getResponseBody $ response
    Left (_ :: HTTP.HttpException) -> Failure
  where
    templatesRepoInfoURL :: HTTP.Request
    templatesRepoInfoURL = "https://api.github.com/repos/wasp-lang/starters/git/trees/main"

    -- Each folder in the "wasp-lang/starters" repo is a template.
    extractTemplateNames :: Either HTTP.JSONException RepoInfo -> StarterTemplateNamesFetchResult
    extractTemplateNames (Left _) = Failure
    extractTemplateNames (Right body) = Success $ StarterTemplateNames . map _path . filter isFolder $ _objects body

    isFolder :: RepoObject -> Bool
    isFolder = (== Folder) . _type

templatesToList :: StarterTemplateNames -> [String]
templatesToList (StarterTemplateNames templates) = templates

isOneOfAvailableTemplateNames :: String -> StarterTemplateNames -> Bool
isOneOfAvailableTemplateNames templateName (StarterTemplateNames templates) = templateName `elem` templates

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
