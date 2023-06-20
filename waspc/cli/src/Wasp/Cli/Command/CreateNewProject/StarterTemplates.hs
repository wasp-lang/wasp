module Wasp.Cli.Command.CreateNewProject.StarterTemplates
  ( getStarterTemplateNames,
    StarterTemplateName (..),
    findTemplateNameByString,
    defaultStarterTemplateName,
    coreWaspProjectFiles,
    readCoreWaspProjectFiles,
  )
where

import Data.Either (fromRight)
import Data.Foldable (find)
import Data.Text (Text)
import StrongPath (File', Path, Rel, System, reldir, relfile, (</>))
import Wasp.Cli.Command.CreateNewProject.StarterTemplates.Remote.Github (starterTemplateGithubRepo)
import Wasp.Cli.Common (WaspProjectDir)
import qualified Wasp.Cli.GithubRepo as GR
import qualified Wasp.Data as Data
import Wasp.Util.IO (readFileStrict)

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

-- TODO: This is now repeating what is in templates/basic which is not great.
-- TODO: Reorganize Cli/templates/basic into two dirs:
--   1. templates/core
--   2. templates/basic
--   Core would contain only the most neccessary files to get started.
--   Other templates, like basic, would build on top of it.
--   So creating new wasp project from local template would first copy files from "core",
--   then from the actual template (e.g. "basic").
--   Then I wouldn't need this list here, I would just list all the files from Cli/templates/core.
coreWaspProjectFiles :: [Path System (Rel WaspProjectDir) File']
coreWaspProjectFiles =
  [ [relfile|.gitignore|],
    [relfile|.wasproot|],
    [relfile|src/.waspignore|],
    [relfile|src/client/tsconfig.json|],
    [relfile|src/client/vite-env.d.ts|],
    [relfile|src/server/tsconfig.json|],
    [relfile|src/shared/tsconfig.json|]
  ]

readCoreWaspProjectFiles :: IO [(Path System (Rel WaspProjectDir) File', Text)]
readCoreWaspProjectFiles = do
  dataDir <- Data.getAbsDataDirPath
  let templatesNewDir = dataDir </> [reldir|Cli/templates/basic|]
  contents <- mapM (readFileStrict . (templatesNewDir </>)) coreWaspProjectFiles
  return $ zip coreWaspProjectFiles contents
