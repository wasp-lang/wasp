module Wasp.Generator.ServerGenerator.JobGenerator
  ( genJobs,
    genJobFactories,
  )
where

import Data.Aeson (object, (.=))
import Data.Maybe
  ( fromJust,
  )
import qualified GHC.Enum as Enum
import StrongPath
  ( Dir,
    Path,
    Posix,
    Rel,
    parseRelFile,
    reldir,
    reldirP,
    relfile,
    (</>),
  )
import Wasp.AppSpec (AppSpec, getJobs)
import Wasp.AppSpec.Job (Job (perform))
import Wasp.Generator.ExternalCodeGenerator.Common (GeneratedExternalCodeDir)
import Wasp.Generator.FileDraft (FileDraft)
import Wasp.Generator.JsImport (getJsImportDetailsForExtFnImport)
import Wasp.Generator.Monad (Generator)
import Wasp.Generator.ServerGenerator.Common
  ( ServerSrcDir,
  )
import qualified Wasp.Generator.ServerGenerator.Common as C

-- | TODO: Make this not hardcoded!
relPosixPathFromJobFileToExtSrcDir :: Path Posix (Rel (Dir ServerSrcDir)) (Dir GeneratedExternalCodeDir)
relPosixPathFromJobFileToExtSrcDir = [reldirP|../ext-src|]

data JobFactory = PassthroughJobFactory
  deriving (Show, Eq, Ord, Enum, Enum.Bounded)

-- TODO: In future we will detect what type of JobFactory
-- to use based on what the Job is using.
jobFactoryForJob :: Job -> JobFactory
jobFactoryForJob _ = PassthroughJobFactory

genJobs :: AppSpec -> Generator [FileDraft]
genJobs spec = return $ genJob <$> getJobs spec
  where
    tmplFile = C.asTmplFile [relfile|src/jobs/_jobs.js|]
    dstFile jobName = C.asServerFile $ [reldir|src/jobs/|] </> fromJust (parseRelFile $ jobName ++ ".js")
    genJob :: (String, Job) -> FileDraft
    genJob (jobName, job) =
      let (jobFnName, jobFnImportStatement) = getJsImportDetailsForExtFnImport relPosixPathFromJobFileToExtSrcDir $ perform job
       in C.mkTmplFdWithDstAndData
            tmplFile
            (dstFile jobName)
            ( Just $
                object
                  [ "jobName" .= jobName,
                    "jobFnName" .= jobFnName,
                    "jobFnImportStatement" .= jobFnImportStatement,
                    "jobFactoryName" .= show (jobFactoryForJob job)
                  ]
            )

genJobFactories :: Generator [FileDraft]
genJobFactories = return $ genJobFactory <$> jobFactoryNames
  where
    genJobFactory :: String -> FileDraft
    genJobFactory jobFactoryName =
      let jobFactoryFile = [reldir|src/jobs/|] </> fromJust (parseRelFile $ jobFactoryName ++ ".js")
       in C.mkTmplFdWithDstAndData (C.asTmplFile jobFactoryFile) (C.asServerFile jobFactoryFile) Nothing
    jobFactoryNames :: [String]
    jobFactoryNames =
      let jobFactories = enumFrom minBound :: [JobFactory]
       in map show jobFactories
