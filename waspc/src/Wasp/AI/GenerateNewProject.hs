module Wasp.AI.GenerateNewProject
  ( generateNewProject,
  )
where

import Control.Monad (forM, forM_)
import Data.List (nub)
import Data.Text (Text)
import qualified Data.Text as T
import NeatInterpolation (trimming)
import StrongPath (File', Path, Rel, System)
import Wasp.AI.CodeAgent (CodeAgent, writeToLog)
import Wasp.AI.GenerateNewProject.Common (NewProjectDetails (..))
import Wasp.AI.GenerateNewProject.Entity (writeEntitiesToWaspFile)
import Wasp.AI.GenerateNewProject.Operation (OperationType (..), generateAndWriteOperation, getOperationJsFilePath)
import Wasp.AI.GenerateNewProject.OperationsJsFile (fixOperationsJsFile)
import Wasp.AI.GenerateNewProject.Page (generateAndWritePage)
import Wasp.AI.GenerateNewProject.Plan (generatePlan)
import qualified Wasp.AI.GenerateNewProject.Plan as Plan
import Wasp.AI.GenerateNewProject.Skeleton (generateAndWriteProjectSkeletonAndPresetFiles)
import Wasp.AI.GenerateNewProject.WaspFile (fixWaspFile)
import Wasp.Project (WaspProjectDir)

generateNewProject ::
  NewProjectDetails ->
  -- | @waspProjectSkeletonFiles@ are files that every new Wasp project should start with, excluding
  --   main.wasp file. They are not specific to the app itself, but are neccessary configuration
  --   "boilerplate" (i.e. .gitignore, tsconfig.json, .wasproot, ...).
  [(Path System (Rel WaspProjectDir) File', Text)] ->
  CodeAgent ()
generateNewProject newProjectDetails waspProjectSkeletonFiles = do
  writeToLog . T.pack $
    "Generating new wasp project named " <> _projectAppName newProjectDetails <> "!"

  writeToLog "Generating project skeleton..."
  (waspFilePath, planRules) <-
    generateAndWriteProjectSkeletonAndPresetFiles newProjectDetails waspProjectSkeletonFiles
  writeToLog "Generated project skeleton."

  writeToLog "Generating plan..."
  plan <- generatePlan newProjectDetails planRules
  writeToLog $ "Plan generated!\n" <> summarizePlan plan

  writeEntitiesToWaspFile waspFilePath (Plan.entities plan)
  writeToLog "Added entities to wasp file."

  writeToLog "Generating actions..."
  actions <-
    forM (Plan.actions plan) $
      generateAndWriteOperation Action newProjectDetails waspFilePath plan

  writeToLog "Generating queries..."
  queries <-
    forM (Plan.queries plan) $
      generateAndWriteOperation Query newProjectDetails waspFilePath plan

  writeToLog "Generating pages..."
  _pages <-
    forM (Plan.pages plan) $
      generateAndWritePage newProjectDetails waspFilePath (Plan.entities plan) queries actions

  -- TODO: Pass plan rules into fixWaspFile, as extra guidance what to keep an eye on? We can't just
  --   do it blindly though, some of them are relevant only to plan (e.g. not generating login /
  --   signup page), we would have to do some adapting.
  writeToLog "Fixing any mistakes in Wasp file..."
  fixWaspFile newProjectDetails waspFilePath
  writeToLog "Wasp file fixed."

  writeToLog "Fixing any mistakes in NodeJS operations files..."
  forM_ (nub $ getOperationJsFilePath <$> (queries <> actions)) $ \opFp -> do
    fixOperationsJsFile newProjectDetails waspFilePath opFp
    writeToLog $ T.pack $ "Fixed NodeJS operations file '" <> opFp <> "'."
  writeToLog "NodeJS operations files fixed."

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
