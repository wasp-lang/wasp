module Wasp.AI.GenerateNewProject.PrismaFile
  ( fixPrismaFile,
  )
where

import Control.Monad.IO.Class (liftIO)
import Data.Functor ((<&>))
import Data.Maybe (fromMaybe)
import Wasp.AI.CodeAgent (getFile, writeToFile)
import Wasp.AI.GenerateNewProject.Common
  ( CodeAgent,
    NewProjectDetails,
  )
import Wasp.AI.GenerateNewProject.Plan (Plan)
import qualified Wasp.Psl.Format as Prisma

fixPrismaFile :: NewProjectDetails -> FilePath -> Plan -> CodeAgent ()
fixPrismaFile _newProjectDetails prismaFilePath _plan = do
  currentPrismaFileContent <- getFile prismaFilePath <&> fromMaybe (error "couldn't find Prisma file to fix")

  result <- liftIO $ Prisma.prismaFormat currentPrismaFileContent

  let formattedPrismaFileContent = Prisma._formattedSchemaPsl result

  writeToFile prismaFilePath (const formattedPrismaFileContent)
