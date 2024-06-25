module Wasp.AI.GenerateNewProject.PrismaModel
  ( writeModelsToPrismaFile,
    modelPlanToPrismaModelText,
  )
where

import Data.Text (Text)
import qualified Data.Text as T
import NeatInterpolation (trimming)
import Wasp.AI.GenerateNewProject.Common (CodeAgent, writeToWaspFileEnd)
import qualified Wasp.AI.GenerateNewProject.Plan as Plan

writeModelsToPrismaFile :: FilePath -> [Plan.Model] -> CodeAgent ()
writeModelsToPrismaFile prismaFilePath modelPlans = do
  writeToWaspFileEnd prismaFilePath $ "\n" <> modelsCode
  where
    modelsCode = T.intercalate "\n\n" $ modelPlanToPrismaModelText <$> modelPlans

modelPlanToPrismaModelText :: Plan.Model -> Text
modelPlanToPrismaModelText plan =
  let name = T.pack $ Plan.modelName plan
      pslBody = T.pack $ Plan.modelBody plan
   in [trimming|
        model ${name} {
          ${pslBody}
        }
      |]

-- TODO: Add data PrismaModel that contains waspDeclaration + entity plan.
