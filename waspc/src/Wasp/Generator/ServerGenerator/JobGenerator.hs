module Wasp.Generator.ServerGenerator.JobGenerator
  ( genJobs,
    genJobFactories,
    isPgBossUsed,
    pgBossVersionBounds,
    maybePgBossDependency,
  )
where

import Data.Aeson (object, (.=))
import Data.Maybe
  ( fromJust,
  )
import StrongPath
  ( Dir,
    File',
    Path,
    Path',
    Posix,
    Rel,
    parseRelFile,
    reldir,
    reldirP,
    relfile,
    (</>),
  )
import Wasp.AppSpec (AppSpec, getJobs)
import Wasp.AppSpec.JSON (JSON (JSON))
import Wasp.AppSpec.Job (Job (executor, perform), JobExecutor (PgBoss), Perform (fn, options))
import Wasp.Generator.ExternalCodeGenerator.Common (GeneratedExternalCodeDir)
import Wasp.Generator.FileDraft (FileDraft)
import Wasp.Generator.JsImport (getJsImportDetailsForExtFnImport)
import Wasp.Generator.Monad (Generator)
import Wasp.Generator.ServerGenerator.Common
  ( ServerSrcDir,
  )
import qualified Wasp.Generator.ServerGenerator.Common as C

data JobFactory = PassthroughJobFactory | PgBossJobFactory
  deriving (Show, Eq, Ord, Enum, Bounded)

genJobs :: AppSpec -> Generator [FileDraft]
genJobs spec = return $ genJob <$> getJobs spec
  where
    tmplFile = C.asTmplFile [relfile|src/jobs/_job.js|]
    dstFileFromJobName jobName = C.asServerFile $ [reldir|src/jobs/|] </> fromJust (parseRelFile $ jobName ++ ".js")
    genJob :: (String, Job) -> FileDraft
    genJob (jobName, job) =
      let (jobPerformFnName, jobPerformFnImportStatement) = getJsImportDetailsForExtFnImport relPosixPathFromJobFileToExtSrcDir $ (fn . perform) job
       in C.mkTmplFdWithDstAndData
            tmplFile
            (dstFileFromJobName jobName)
            ( Just $
                object
                  [ "jobName" .= jobName,
                    "jobPerformFnName" .= jobPerformFnName,
                    "jobPerformFnImportStatement" .= jobPerformFnImportStatement,
                    "jobFactoryName" .= show (jobFactoryForJob $ executor job),
                    -- TODO: Handle defaults in a helper in Job.hs?
                    "jobPerformOptions" .= maybe "{}" (\(JSON str) -> "{" ++ str ++ "}") (options . perform $ job)
                  ]
            )

-- | TODO: Make this not hardcoded!
relPosixPathFromJobFileToExtSrcDir :: Path Posix (Rel (Dir ServerSrcDir)) (Dir GeneratedExternalCodeDir)
relPosixPathFromJobFileToExtSrcDir = [reldirP|../ext-src|]

jobFactoryForJob :: JobExecutor -> JobFactory
jobFactoryForJob PgBoss = PgBossJobFactory

genJobFactories :: Generator [FileDraft]
genJobFactories = return $ genJobFactory <$> jobFactories
  where
    genJobFactory :: JobFactory -> FileDraft
    genJobFactory jobFactory =
      let jobFactoryFp = jobFactoryFilePath jobFactory
          sourceTemplateFp = C.asTmplFile jobFactoryFp
       in C.mkTmplFd sourceTemplateFp

jobFactories :: [JobFactory]
jobFactories = enumFrom minBound :: [JobFactory]

jobFactoryFilePath :: JobFactory -> Path' (Rel d) File'
jobFactoryFilePath PassthroughJobFactory = [relfile|src/jobs/PassthroughJobFactory.js|]
jobFactoryFilePath PgBossJobFactory = [relfile|src/jobs/PgBossJobFactory.js|]

isPgBossUsed :: AppSpec -> Bool
isPgBossUsed spec = any (\(_, job) -> executor job == PgBoss) (getJobs spec)

pgBossVersionBounds :: String
pgBossVersionBounds = "^7.2.1"

maybePgBossDependency :: Maybe AppSpec -> Maybe (String, String)
maybePgBossDependency Nothing = Nothing
maybePgBossDependency (Just spec)
  | isPgBossUsed spec = Just ("pg-boss", pgBossVersionBounds)
  | otherwise = Nothing
