module Wasp.Cli.Command.CreateNewProject.StarterTemplates.Common
  ( getStarterTemplateNames,
    StarterTemplateName (..),
    StarterTemplateNamesFetchResult (..),
    findTemplateNameByString,
    replaceTemplatePlaceholdersInWaspFile,
  )
where

import Control.Exception (try)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson
  ( FromJSON,
    parseJSON,
    withObject,
    (.:),
  )
import Data.Foldable (find)
import Data.List (foldl')
import qualified Data.Text as T
import qualified Network.HTTP.Simple as HTTP
import StrongPath (Abs, Dir, Path', relfile, (</>))
import Wasp.Cli.Command.CreateNewProject.Common (waspVersionBounds)
import Wasp.Project (WaspProjectDir)
import qualified Wasp.Util.IO as IOUtil

data StarterTemplateName = RemoteTemplate String | LocalTemplate String
  deriving (Eq)

instance Show StarterTemplateName where
  show (RemoteTemplate templateName) = templateName
  show (LocalTemplate templateName) = templateName

getStarterTemplateNames :: IO [StarterTemplateName]
getStarterTemplateNames = do
  fetchResult <- getRemoteStarterTemplateNames
  let remoteTemplates = case fetchResult of
        Success starterTemplateNames -> starterTemplateNames
        Failure -> []

  return $ localTemplates ++ remoteTemplates
  where
    localTemplates :: [StarterTemplateName]
    localTemplates = [LocalTemplate "basic"]

    getRemoteStarterTemplateNames :: IO StarterTemplateNamesFetchResult
    getRemoteStarterTemplateNames = do
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
        extractTemplateNames (Right body) = Success . map (RemoteTemplate . _path) . filter isFolder $ _objects body

        isFolder :: RepoObject -> Bool
        isFolder = (== Folder) . _type

findTemplateNameByString :: [StarterTemplateName] -> String -> Maybe StarterTemplateName
findTemplateNameByString templateNames templateNameString = find (\templateName -> show templateName == templateNameString) templateNames

-- Template file for wasp file has placeholders in it that we want to replace
-- in the .wasp file we have written to the disk.
replaceTemplatePlaceholdersInWaspFile :: String -> String -> Path' Abs (Dir WaspProjectDir) -> IO ()
replaceTemplatePlaceholdersInWaspFile appName projectName projectDir = liftIO $ do
  mainWaspFileContent <- IOUtil.readFileStrict absMainWaspFile

  let replacedContent =
        foldl'
          (\acc (placeholder, value) -> T.replace (T.pack placeholder) (T.pack value) acc)
          mainWaspFileContent
          replacements

  IOUtil.writeFileFromText absMainWaspFile replacedContent
  where
    absMainWaspFile = projectDir </> [relfile|main.wasp|]
    replacements =
      [ ("__waspAppName__", appName),
        ("__waspProjectName__", projectName),
        ("__waspVersion__", waspVersionBounds)
      ]

data StarterTemplateNamesFetchResult = Success [StarterTemplateName] | Failure

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
