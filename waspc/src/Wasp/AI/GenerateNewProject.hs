module Wasp.AI.GenerateNewProject
  ( generateNewProject,
  )
where

import Control.Monad (forM, forM_)
import Data.List (nub)
import Data.Text (Text)
import qualified Data.Text as T
import StrongPath (File', Path, Rel, System)
import Text.Printf (printf)
import Wasp.AI.CodeAgent (CodeAgent, getTotalTokensUsage, writeToLog)
import Wasp.AI.GenerateNewProject.Common (NewProjectDetails (..))
import Wasp.AI.GenerateNewProject.Entity (writeEntitiesToWaspFile)
import Wasp.AI.GenerateNewProject.Operation (OperationType (..), generateAndWriteOperation, getOperationJsFilePath)
import Wasp.AI.GenerateNewProject.OperationsJsFile (fixOperationsJsFile)
import Wasp.AI.GenerateNewProject.Page (generateAndWritePage, getPageComponentPath)
import Wasp.AI.GenerateNewProject.PageComponentFile (fixPageComponent)
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

  plan <- generatePlan newProjectDetails planRules

  writeEntitiesToWaspFile waspFilePath (Plan.entities plan)
  writeToLog "Updated wasp file with entities."

  writeToLog "Generating actions..."
  actions <-
    forM (Plan.actions plan) $
      generateAndWriteOperation Action newProjectDetails waspFilePath plan

  writeToLog "Generating queries..."
  queries <-
    forM (Plan.queries plan) $
      generateAndWriteOperation Query newProjectDetails waspFilePath plan

  writeToLog "Generating pages..."
  pages <-
    forM (Plan.pages plan) $
      generateAndWritePage newProjectDetails waspFilePath (Plan.entities plan) queries actions

  -- TODO: Pass plan rules into fixWaspFile, as extra guidance what to keep an eye on? We can't just
  --   do it blindly though, some of them are relevant only to plan (e.g. not generating login /
  --   signup page), we would have to do some adapting.
  writeToLog "Fixing any mistakes in Wasp file..."
  fixWaspFile newProjectDetails waspFilePath plan
  writeToLog "Wasp file fixed."

  writeToLog "Fixing any mistakes in NodeJS operations files..."
  forM_ (nub $ getOperationJsFilePath <$> (queries <> actions)) $ \opFp -> do
    fixOperationsJsFile newProjectDetails waspFilePath opFp
    writeToLog $ T.pack $ "Fixed NodeJS operations file '" <> opFp <> "'."
  writeToLog "NodeJS operations files fixed."

  writeToLog "Fixing any mistakes in pages..."
  forM_ (getPageComponentPath <$> pages) $ \pageFp -> do
    fixPageComponent newProjectDetails waspFilePath pageFp
    writeToLog $ T.pack $ "Fixed '" <> pageFp <> "' page."
  writeToLog "Pages fixed."

  (promptTokensUsed, completionTokensUsed) <- getTotalTokensUsage
  writeToLog $
    T.pack $
      printf "Total tokens usage: ~%.1fk" $
        fromIntegral (promptTokensUsed + completionTokensUsed) / (1000 :: Double)

  writeToLog "Done!"
  where
