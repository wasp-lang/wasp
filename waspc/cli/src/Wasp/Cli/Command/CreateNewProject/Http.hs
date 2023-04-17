module Wasp.Cli.Command.CreateNewProject.Http
  ( getTemplatesJson,
  )
where

import Data.Aeson (FromJSON, parseJSON, withObject, (.:))
import qualified Network.HTTP.Simple as HTTP

data Tree = Tree
  { _path :: String,
    _type :: String
  }
  deriving (Show)

data RepoInfo = RepoInfo
  { _tree :: [Tree]
  }
  deriving (Show)

instance FromJSON Tree where
  parseJSON = withObject "Tree" $ \o -> do
    path <- o .: "path"
    type_ <- o .: "type"
    return
      Tree
        { _path = path,
          _type = type_
        }

instance FromJSON RepoInfo where
  parseJSON = withObject "RepoInfo" $ \o -> do
    tree <- o .: "tree"
    return
      RepoInfo
        { _tree = tree
        }

-- Uses Github API to fetch repo info and extract template names. Each folder in the repo is a template.
getTemplatesJson :: IO (Maybe [String])
getTemplatesJson = do
  response <- HTTP.httpJSONEither $ createRequestWithUserAgent urlToStarterTemplates
  return $ extractTemplateNames . HTTP.getResponseBody $ response
  where
    extractTemplateNames :: Either HTTP.JSONException RepoInfo -> Maybe [String]
    extractTemplateNames (Left _) = Nothing
    -- Extract folder names from the top level of the repo
    extractTemplateNames (Right body) = Just . map _path . filter isTree $ _tree body

    -- We are only interested in folder names, which are of type "tree".
    isTree :: Tree -> Bool
    isTree = (== "tree") . _type

    -- Github returns 403 if we don't specify user-agent.
    createRequestWithUserAgent :: HTTP.Request -> HTTP.Request
    createRequestWithUserAgent request = HTTP.addRequestHeader "User-Agent" "wasp-lang/wasp" request

    urlToStarterTemplates :: HTTP.Request
    urlToStarterTemplates = "https://api.github.com/repos/wasp-lang/starters/git/trees/main"
