module Wasp.AI.GenerateNewProject
  ( generateNewProject,
  )
where

import Control.Monad (forM, forM_)
import Data.Function ((&))
import Data.List (nub)
import Data.String (fromString)
import Data.Text (Text)
import StrongPath (File', Path, Rel, System)
import Text.Printf (printf)
import Wasp.AI.CodeAgent (getTotalTokensUsage, writeToLog)
import Wasp.AI.GenerateNewProject.Common
  ( CodeAgent,
    NewProjectDetails (..),
    codingChatGPTParams,
    planningChatGPTParams,
  )
import Wasp.AI.GenerateNewProject.Entity (writeEntitiesToWaspFile)
import qualified Wasp.AI.GenerateNewProject.LogMsg as L
import Wasp.AI.GenerateNewProject.Operation
  ( OperationType (..),
    generateAndWriteOperation,
    getOperationJsFilePath,
  )
import Wasp.AI.GenerateNewProject.OperationsJsFile (fixOperationsJsFile)
import Wasp.AI.GenerateNewProject.Page (generateAndWritePage, getPageComponentPath)
import Wasp.AI.GenerateNewProject.PageComponentFile (fixPageComponent)
import Wasp.AI.GenerateNewProject.Plan (generatePlan)
import qualified Wasp.AI.GenerateNewProject.Plan as Plan
import Wasp.AI.GenerateNewProject.Skeleton (generateAndWriteProjectSkeletonAndPresetFiles)
import Wasp.AI.GenerateNewProject.WaspFile (fixWaspFile)
import qualified Wasp.AI.OpenAI.ChatGPT as ChatGPT
import Wasp.Project (WaspProjectDir)

generateNewProject ::
  NewProjectDetails ->
  -- | @waspProjectSkeletonFiles@ are files that every new Wasp project should start with, excluding
  --   main.wasp file. They are not specific to the app itself, but are neccessary configuration
  --   "boilerplate" (i.e. .gitignore, tsconfig.json, .wasproot, ...).
  [(Path System (Rel WaspProjectDir) File', Text)] ->
  CodeAgent ()
generateNewProject newProjectDetails waspProjectSkeletonFiles = do
  writeToLog $
    "\nGenerating a new Wasp project named " <> L.styled L.Important (fromString $ _projectAppName newProjectDetails) <> "!"

  let showParams chatGPTParams =
        L.styled L.Important (fromString $ show $ ChatGPT._model chatGPTParams)
          <> (ChatGPT._temperature chatGPTParams & maybe "" (\t -> " (temp " <> fromString (show t) <> ")"))
   in writeToLog $
        "\n"
          <> "Using "
          <> showParams (planningChatGPTParams newProjectDetails)
          <> " for planning and "
          <> showParams (codingChatGPTParams newProjectDetails)
          <> " for coding."

  writeToLogGenerating "project skeleton..."
  (waspFilePath, planRules) <-
    generateAndWriteProjectSkeletonAndPresetFiles newProjectDetails waspProjectSkeletonFiles
  writeToLog "Generated project skeleton."

  plan <- generatePlan newProjectDetails planRules
  writeEntitiesToWaspFile waspFilePath (Plan.entities plan)
  writeToLog "Updated the Wasp file with entities."

  writeToLogGenerating "actions..."
  actions <-
    forM (Plan.actions plan) $
      generateAndWriteOperation Action newProjectDetails waspFilePath plan

  writeToLogGenerating "queries..."
  queries <-
    forM (Plan.queries plan) $
      generateAndWriteOperation Query newProjectDetails waspFilePath plan

  writeToLogGenerating "pages..."
  pages <-
    forM (Plan.pages plan) $
      generateAndWritePage newProjectDetails waspFilePath (Plan.entities plan) queries actions

  -- TODO: Pass plan rules into fixWaspFile, as extra guidance what to keep an eye on? We can't just
  --   do it blindly though, some of them are relevant only to plan (e.g. not generating login /
  --   signup page), we would have to do some adapting.
  writeToLogFixing "mistakes in the Wasp file..."
  fixWaspFile newProjectDetails waspFilePath plan
  writeToLog "Wasp file fixed."

  writeToLogFixing "mistakes in NodeJS operation files..."
  forM_ (nub $ getOperationJsFilePath <$> (queries <> actions)) $ \opFp -> do
    fixOperationsJsFile newProjectDetails waspFilePath opFp
    writeToLog $ "Fixed NodeJS operation file '" <> fromString opFp <> "'."
  writeToLog "NodeJS operation files fixed."

  writeToLogFixing "common mistakes in pages..."
  forM_ (getPageComponentPath <$> pages) $ \pageFp -> do
    fixPageComponent newProjectDetails waspFilePath pageFp
    writeToLog $ "Fixed '" <> fromString pageFp <> "' page."
  writeToLog "Pages fixed."

  (promptTokensUsed, completionTokensUsed) <- getTotalTokensUsage
  writeToLog $
    "\nTotal tokens usage: "
      <> ( L.styled L.Important . fromString . printf "~%.1fk" $
             fromIntegral (promptTokensUsed + completionTokensUsed) / (1000 :: Double)
         )

  writeToLog $ L.styled L.Important "\nDone!"
  where
    writeToLogFixing msg = writeToLog $ "\n" <> L.styled L.Fixing "Fixing " <> msg
    writeToLogGenerating msg = writeToLog $ "\n" <> L.styled L.Generating "Generating " <> msg
