module Wasp.AI.GenerateNewProject.Entity
  ( writeEntitiesToWaspFile,
    entityPlanToWaspDecl,
  )
where

import Data.Text (Text)
import qualified Data.Text as T
import NeatInterpolation (trimming)
import Wasp.AI.GenerateNewProject.Common (CodeAgent, writeToWaspFileEnd)
import qualified Wasp.AI.GenerateNewProject.Plan as Plan

writeEntitiesToWaspFile :: FilePath -> [Plan.Entity] -> CodeAgent ()
writeEntitiesToWaspFile waspFilePath entityPlans = do
  writeToWaspFileEnd waspFilePath $ "\n" <> entitiesCode
  where
    entitiesCode = T.intercalate "\n\n" $ entityPlanToWaspDecl <$> entityPlans

entityPlanToWaspDecl :: Plan.Entity -> Text
entityPlanToWaspDecl plan =
  let name = T.pack $ Plan.entityName plan
      pslBody = T.pack $ Plan.entityBodyPsl plan
   in [trimming|
        entity ${name} {=psl
          ${pslBody}
        psl=}
      |]

-- TODO: Add data Entity that contains waspDeclaration + entity plan.
