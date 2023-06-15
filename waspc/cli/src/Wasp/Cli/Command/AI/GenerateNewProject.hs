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
import Wasp.Cli.Command.AI.GenerateNewProject.Entity (writeEntitiesToWaspFile)
import Wasp.Cli.Command.AI.GenerateNewProject.Operation (OperationType (..), generateAndWriteOperation)
import Wasp.Cli.Command.AI.GenerateNewProject.Page (generateAndWritePage)
import Wasp.Cli.Command.AI.GenerateNewProject.Plan (generatePlan)
import qualified Wasp.Cli.Command.AI.GenerateNewProject.Plan as Plan
import Wasp.Cli.Command.AI.GenerateNewProject.Skeleton (generateAndWriteProjectSkeleton)

generateNewProject :: NewProjectDetails -> CodeAgent ()
generateNewProject newProjectDetails = do
  (waspFilePath, planRules) <- generateAndWriteProjectSkeleton newProjectDetails
  writeToLog "Generated project skeleton."

  writeToLog "Generating plan..."
  plan <- generatePlan newProjectDetails planRules
  writeToLog $ "Plan generated!\n" <> summarizePlan plan

  writeEntitiesToWaspFile waspFilePath (Plan.entities plan)
  writeToLog "Added entities to wasp file."

  writeToLog "Generating actions..."
  actions <- forM (Plan.actions plan) $ generateAndWriteOperation Action newProjectDetails waspFilePath plan

  writeToLog "Generating queries..."
  queries <- forM (Plan.queries plan) $ generateAndWriteOperation Query newProjectDetails waspFilePath plan

  writeToLog "Generating pages..."
  _pages <-
    forM (Plan.pages plan) $
      generateAndWritePage newProjectDetails waspFilePath (Plan.entities plan) queries actions

  -- TODO: what about having additional step here that goes through all the files once again and
  --   fixes any stuff in them (Wasp, JS files)? REPL?

  -- TODO: Consider going through all the prompts and trying to reduce their length,
  --   to make sure we are not droping anyting out of context + that we are not wasteful.
  writeToLog "Done!"
  where
    summarizePlan plan =
      let numQueries = showT $ length $ Plan.queries plan
          numActions = showT $ length $ Plan.actions plan
          numPages = showT $ length $ Plan.pages plan
          numEntities = showT $ length $ Plan.entities plan
          queryNames = showT $ Plan.opName <$> Plan.queries plan
          actionNames = showT $ Plan.opName <$> Plan.actions plan
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
