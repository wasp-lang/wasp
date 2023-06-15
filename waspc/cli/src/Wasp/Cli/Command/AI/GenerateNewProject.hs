module Wasp.Cli.Command.AI.GenerateNewProject
  ( generateNewProject,
  )
where

-- TODO: Probably move this module out of here into general wasp lib.

import Control.Monad (forM)
import Data.Text (Text)
import qualified Data.Text as T
import NeatInterpolation (trimming)
import Wasp.Cli.Command.AI.CodeAgent (CodeAgent, writeToLog)
import Wasp.Cli.Command.AI.GenerateNewProject.Common (NewProjectDetails (..))
import Wasp.Cli.Command.AI.GenerateNewProject.Plan (generatePlan)
import qualified Wasp.Cli.Command.AI.GenerateNewProject.Plan as Plan
import Wasp.Cli.Command.AI.GenerateNewProject.Skeleton (generateAndWriteProjectSkeleton)

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
  waspFilePath <- generateAndWriteProjectSkeleton newProjectDetails
  writeToLog "Generated project skeleton."

  writeToLog "Generating plan..."
  plan <- generatePlan newProjectDetails
  writeToLog $ "Plan generated!\n" <> summarizePlan plan

  writeEntitiesToWaspFile waspFilePath (Plan.entities plan)
  writeToLog "Added entities to wasp file."

  writeToLog "Generating actions..."
  actions <- forM (Plan.actions plan) $ generateAndWriteAction waspFilePath plan

  writeToLog "Generating queries..."
  queries <- forM (Plan.queries plan) $ generateAndWriteQuery waspFilePath plan

  writeToLog "Generating pages..."
  _pages <- forM (Plan.pages plan) $ generateAndWritePage waspFilePath plan queries actions

  -- TODO: what about having additional step here that goes through all the files once again and fixes any stuff in them (Wasp, JS files)? REPL?
  -- TODO: add some commented out lines to wasp file that showcase other features? jobs, api, serverSetup, sockets, ... .
  -- TODO: Idea: give it chatGPT-function `queryDocs` that it can use whenever to look up Wasp docs.
  -- TODO: Idea: maybe use chatGPT-functions also to increase the chance of it producing correct JSON
  --   when generating actions, operations and similar. Maybe an overkill.
  writeToLog "Done!"
  where
    summarizePlan plan =
      let numQueries = showT $ length $ Plan.queries plan
          numActions = showT $ length $ Plan.actions plan
          numPages = showT $ length $ Plan.pages plan
          numEntities = showT $ length $ Plan.entities plan
          queryNames = showT $ Plan.queryName <$> Plan.queries plan
          actionNames = showT $ Plan.actionName <$> Plan.actions plan
          pageNames = showT $ Plan.pageName <$> Plan.pages plan
          entityNames = showT $ Plan.entityName <$> Plan.entities plan
       in [trimming|
            - ${numQueries} queries: ${queryNames}
            - ${numActions} actions: ${actionNames}
            - ${numEntities} entities: ${entityNames}
            - ${numPages} pages: ${pageNames}
          |]

    showT :: Show a => a -> Text
    showT = T.pack . show

    generateAndWriteAction waspFilePath plan actionPlan = do
      action <- generateAction newProjectDetails (Plan.entities plan) actionPlan
      writeActionToFile action
      writeActionToWaspFile waspFilePath action
      writeToLog $ "Generated action: " <> T.pack (Plan.actionName actionPlan)
      return action

    generateAndWriteQuery waspFilePath plan queryPlan = do
      query <- generateQuery newProjectDetails (Plan.entities plan) queryPlan
      writeQueryToFile query
      writeQueryToWaspFile waspFilePath query
      writeToLog $ "Generated query: " <> T.pack (Plan.queryName queryPlan)
      return query

    generateAndWritePage waspFilePath plan queries actions pagePlan = do
      page <- generatePage newProjectDetails (Plan.entities plan) queries actions pagePlan
      writePageToFile page
      writePageToWaspFile waspFilePath page
      writeToLog $ "Generated page: " <> T.pack (Plan.pageName pagePlan)
      return page

writeEntitiesToWaspFile :: FilePath -> [Plan.Entity] -> CodeAgent ()
writeEntitiesToWaspFile waspFilePath entities = do
  -- TODO: assemble code for each entity and write it to wasp file.
  undefined

generateAction :: NewProjectDetails -> [Plan.Entity] -> Plan.Action -> CodeAgent Action
generateAction = undefined

writeActionToFile :: Action -> CodeAgent ()
writeActionToFile = undefined

writeActionToWaspFile :: FilePath -> Action -> CodeAgent ()
writeActionToWaspFile waspFilePath action = undefined

data Action = Action
  { _actionWaspDecl :: String,
    _actionJsImpl :: String,
    _actionPlan :: Plan.Action
  }

generateQuery :: NewProjectDetails -> [Plan.Entity] -> Plan.Query -> CodeAgent Query
generateQuery = undefined

writeQueryToFile :: Query -> CodeAgent ()
writeQueryToFile = undefined

writeQueryToWaspFile :: FilePath -> Query -> CodeAgent ()
writeQueryToWaspFile waspFilePath query = undefined

data Query = Query
  { _queryWaspDecl :: String,
    _queryJsImpl :: String,
    _queryPlan :: Plan.Action
  }

generatePage :: NewProjectDetails -> [Plan.Entity] -> [Query] -> [Action] -> Plan.Page -> CodeAgent Page
generatePage = undefined

writePageToFile :: Page -> CodeAgent ()
writePageToFile = undefined

writePageToWaspFile :: FilePath -> Page -> CodeAgent ()
writePageToWaspFile waspFilePath page = undefined

data Page = Page
  { _pageWaspDecl :: String,
    _pageJsImpl :: String,
    _pagePlan :: Plan.Action
  }
