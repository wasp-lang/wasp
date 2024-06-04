module Wasp.AI.GenerateNewProject.PrismaFile
  ( fixPrismaFile,
  )
where

import Data.Functor ((<&>))
import Data.Maybe (fromMaybe)
import Wasp.AI.CodeAgent (getFile, writeToFile)
import Wasp.AI.GenerateNewProject.Common
  ( CodeAgent,
    NewProjectDetails,
  )
import Wasp.AI.GenerateNewProject.Plan (Plan)

fixPrismaFile :: NewProjectDetails -> FilePath -> Plan -> CodeAgent ()
fixPrismaFile _newProjectDetails prismaFilePath _plan = do
  currentPrismaFileContent <- getFile prismaFilePath <&> fromMaybe (error "couldn't find Prisma file to fix")

  writeToFile prismaFilePath (const currentPrismaFileContent)
