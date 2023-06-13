module Wasp.Cli.Command.AI.GenerateNewProject
  ( generateNewProject,
    NewProjectDetails (..),
    AuthProvider (..),
  )
where

-- TODO: Probably move this module out of here into general wasp lib.

import Control.Monad (forM)
import Data.Text (Text)
import qualified Data.Text as T
import Wasp.Cli.Command.AI.CodeAgent (CodeAgent, writeNewFile, writeToLog)
import Wasp.Cli.Command.AI.GenerateNewProject.Plan (Plan)
import qualified Wasp.Cli.Command.AI.GenerateNewProject.Plan as P

data NewProjectDetails = NewProjectDetails
  { _projectName :: !String,
    _projectDescription :: !String,
    _projectAuth :: !AuthProvider
  }

type File = (FilePath, Text)

data AuthProvider = Google

-- TODO: Have generateNewProject accept Chan, to which it will stream its progress?
--   It could just stream its output instead of printing it to stdout, so calling function
--   has more control over what to do with it.
--   Yeah, I think we certainly want to have Chan. And one important thing we want to send over it
--   is information about files that we are updating. So each time we create a new file or update existing one,
--   we want to send over information about that, so receiver can either write it to disk, or show it in web app,
--   or something. Such messages could be called "FileWritten" and say which path, what is the new content,
--   and also contain description of what happened (or maybe that is separate message).
generateNewProject :: NewProjectDetails -> CodeAgent ()
generateNewProject newProjectDetails = do
  let waspFile = generateBaseWaspFile newProjectDetails
  let waspFilePath = fst waspFile
  writeNewFile waspFile
  let dotEnvServerFile = generateDotEnvServerFile newProjectDetails
  writeNewFile dotEnvServerFile
  let otherNewProjectFiles = generateOtherNewProjectFiles newProjectDetails
  mapM_ writeNewFile otherNewProjectFiles
  writeToLog "Generated project skeleton."

  writeToLog "Generating plan..."
  plan <- generatePlan newProjectDetails
  -- TODO: Show plan nicer! Maybe just short summary of it: we will create 4 entities, 3 operations, ... .
  writeToLog $ "Plan generated! " <> T.pack (show plan)

  updateWaspFileWithEntities waspFilePath (P.entities plan)
  writeToLog "Added entities to wasp file."

  writeToLog "Generating actions..."
  actions <- forM (P.actions plan) $ \actionPlan -> do
    action <- generateAction newProjectDetails (P.entities plan) actionPlan
    writeActionToFile action
    updateWaspFileWithAction waspFilePath action
    writeToLog $ "Generated action: " <> T.pack (P.actionName actionPlan)
    return action

  writeToLog "Generating queries..."
  queries <- forM (P.queries plan) $ \queryPlan -> do
    query <- generateQuery newProjectDetails (P.entities plan) queryPlan
    writeQueryToFile query
    updateWaspFileWithQuery waspFilePath query
    writeToLog $ "Generated query: " <> T.pack (P.queryName queryPlan)
    return query

  writeToLog "Generating pages..."
  pages <- forM (P.pages plan) $ \pagePlan -> do
    page <- generatePage newProjectDetails (P.entities plan) queries actions pagePlan
    writePageToFile page
    updateWaspFileWithPage waspFilePath page
    writeToLog $ "Generated page: " <> T.pack (P.pageName pagePlan)
    return page

  -- TODO: what about having additional step here that goes through all the files once again and fixes any stuff in them (Wasp, JS files)? REPL?
  writeToLog "Done!"

generateBaseWaspFile :: NewProjectDetails -> File
generateBaseWaspFile = undefined

generateDotEnvServerFile :: NewProjectDetails -> File
generateDotEnvServerFile = undefined

-- TODO: implement generateOtherNewProjectFiles based on existing CNP.createWaspProjectDir function.
generateOtherNewProjectFiles :: NewProjectDetails -> [File]
generateOtherNewProjectFiles = undefined -- Maybe add dotenvserver under this.

generatePlan :: NewProjectDetails -> CodeAgent Plan
generatePlan = undefined

updateWaspFileWithEntities :: FilePath -> [P.Entity] -> CodeAgent ()
updateWaspFileWithEntities waspFilePath entities = do
  -- TODO: assemble code for each entity and write it to wasp file.
  undefined

generateAction :: NewProjectDetails -> [P.Entity] -> P.Action -> CodeAgent Action
generateAction = undefined

writeActionToFile :: Action -> CodeAgent ()
writeActionToFile = undefined

updateWaspFileWithAction :: FilePath -> Action -> CodeAgent ()
updateWaspFileWithAction waspFilePath action = undefined

data Action = Action
  { _actionWaspDecl :: String,
    _actionJsImpl :: String,
    _actionPlan :: P.Action
  }

generateQuery :: NewProjectDetails -> [P.Entity] -> P.Query -> CodeAgent Query
generateQuery = undefined

writeQueryToFile :: Query -> CodeAgent ()
writeQueryToFile = undefined

updateWaspFileWithQuery :: FilePath -> Query -> CodeAgent ()
updateWaspFileWithQuery waspFilePath query = undefined

data Query = Query
  { _queryWaspDecl :: String,
    _queryJsImpl :: String,
    _queryPlan :: P.Action
  }

generatePage :: NewProjectDetails -> [P.Entity] -> [Query] -> [Action] -> P.Page -> CodeAgent Page
generatePage = undefined

writePageToFile :: Page -> CodeAgent ()
writePageToFile = undefined

updateWaspFileWithPage :: FilePath -> Page -> CodeAgent ()
updateWaspFileWithPage waspFilePath page = undefined

data Page = Page
  { _pageWaspDecl :: String,
    _pageJsImpl :: String,
    _pagePlan :: P.Action
  }
