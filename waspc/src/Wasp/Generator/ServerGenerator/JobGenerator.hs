module Wasp.Generator.ServerGenerator.JobGenerator
  ( genJobs,
    genJobExecutors,
    pgBossVersionBounds,
    pgBossDependency,
    depsRequiredByJobs,
  )
where

import Data.Aeson (object, (.=))
import Data.Maybe (fromJust, fromMaybe)
import StrongPath
  ( Dir,
    File',
    Path,
    Path',
    Posix,
    Rel,
    basename,
    parseRelFile,
    reldir,
    reldirP,
    relfile,
    toFilePath,
    (</>),
  )
import qualified StrongPath as SP
import Wasp.AppSpec (AppSpec, getJobs)
import qualified Wasp.AppSpec.App.Dependency as AS.Dependency
import qualified Wasp.AppSpec.JSON as AS.JSON
import Wasp.AppSpec.Job (Job, JobExecutor (Passthrough, PgBoss), jobExecutors)
import qualified Wasp.AppSpec.Job as J
import Wasp.AppSpec.Util (isPgBossJobExecutorUsed)
import Wasp.Generator.ExternalCodeGenerator.Common (GeneratedExternalCodeDir)
import Wasp.Generator.FileDraft (FileDraft)
import Wasp.Generator.JsImport (getJsImportDetailsForExtFnImport)
import Wasp.Generator.Monad (Generator)
import Wasp.Generator.ServerGenerator.Common (ServerSrcDir, ServerTemplatesDir)
import qualified Wasp.Generator.ServerGenerator.Common as C

genJobs :: AppSpec -> Generator [FileDraft]
genJobs spec = return $ genJob <$> getJobs spec
  where
    tmplFile = C.asTmplFile [relfile|src/jobs/_job.js|]
    dstFileFromJobName jobName = C.asServerFile $ [reldir|src/jobs/|] </> fromJust (parseRelFile $ jobName ++ ".js")
    genJob :: (String, Job) -> FileDraft
    genJob (jobName, job) =
      let (jobPerformFnName, jobPerformFnImportStatement) = getJsImportDetailsForExtFnImport relPosixPathFromJobFileToExtSrcDir $ (J.fn . J.perform) job
       in C.mkTmplFdWithDstAndData
            tmplFile
            (dstFileFromJobName jobName)
            ( Just $
                object
                  [ "jobName" .= jobName,
                    "jobPerformFnName" .= jobPerformFnName,
                    "jobPerformFnImportStatement" .= jobPerformFnImportStatement,
                    -- TODO: make this a relative path backed by SP
                    "executorJobFilename" .= executorJobDestinationFilename (J.executor job),
                    "jobPerformOptions" .= show (fromMaybe AS.JSON.emptyObject (J.options . J.perform $ job))
                  ]
            )

-- | TODO: Make this not hardcoded!
relPosixPathFromJobFileToExtSrcDir :: Path Posix (Rel (Dir ServerSrcDir)) (Dir GeneratedExternalCodeDir)
relPosixPathFromJobFileToExtSrcDir = [reldirP|../ext-src|]

genJobExecutors :: Generator [FileDraft]
genJobExecutors = return $ jobExecutorFds ++ jobExecutorHelperFds
  where
    jobExecutorFds :: [FileDraft]
    jobExecutorFds = genJobExecutor <$> jobExecutors

    genJobExecutor :: JobExecutor -> FileDraft
    genJobExecutor jobExecutor = C.mkTmplFd $ C.asTmplFile $ executorJobTemplateFilePath jobExecutor

    jobExecutorHelperFds :: [FileDraft]
    jobExecutorHelperFds =
      [ C.mkTmplFd $ C.asTmplFile [relfile|src/jobs/core/pgBoss.js|],
        C.mkTmplFd $ C.asTmplFile [relfile|src/jobs/core/Job.js|],
        C.mkTmplFd $ C.asTmplFile [relfile|src/jobs/core/SubmittedJob.js|]
      ]

executorJobTemplateFilePath :: JobExecutor -> Path' (Rel ServerTemplatesDir) File'
executorJobTemplateFilePath Passthrough = [relfile|src/jobs/core/passthroughJob.js|]
executorJobTemplateFilePath PgBoss = [relfile|src/jobs/core/pgBossJob.js|]

-- Same path in project output destination server/src dir as template server/src dir.
executorJobDestinationFilePath :: JobExecutor -> Path' (Rel ServerSrcDir) File'
executorJobDestinationFilePath = SP.castRel . executorJobTemplateFilePath

-- TODO: make relative file path that can be used in _jobs.js
executorJobDestinationFilename :: JobExecutor -> FilePath
executorJobDestinationFilename = toFilePath . basename . executorJobDestinationFilePath

pgBossVersionBounds :: String
pgBossVersionBounds = "^7.2.1"

pgBossDependency :: AS.Dependency.Dependency
pgBossDependency = AS.Dependency.make ("pg-boss", pgBossVersionBounds)

depsRequiredByJobs :: AppSpec -> [AS.Dependency.Dependency]
depsRequiredByJobs spec = [pgBossDependency | isPgBossJobExecutorUsed spec]
