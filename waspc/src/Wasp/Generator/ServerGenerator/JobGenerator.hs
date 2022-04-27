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
    parseRelFile,
    reldir,
    reldirP,
    relfile,
    toFilePath,
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
import Wasp.Generator.ServerGenerator.Common
  ( ServerRootDir,
    ServerSrcDir,
    ServerTemplatesDir,
    srcDirInServerTemplatesDir,
  )
import qualified Wasp.Generator.ServerGenerator.Common as C

genJobs :: AppSpec -> Generator [FileDraft]
genJobs spec = return $ genJob <$> getJobs spec
  where
    tmplFile = C.asTmplFile $ jobsDirInServerTemplatesDir SP.</> [relfile|_job.js|]
    dstFileFromJobName jobName = jobsDirInServerRootDir SP.</> fromJust (parseRelFile $ jobName ++ ".js")
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
                    "jobPerformOptions" .= show (fromMaybe AS.JSON.emptyObject (J.options . J.perform $ job)),
                    "executorJobRelFP" .= toFilePath (executorJobTemplateInJobsDir (J.executor job))
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
    genJobExecutor jobExecutor = C.mkTmplFd $ executorJobTemplateInServerTemplatesDir jobExecutor

    jobExecutorHelperFds :: [FileDraft]
    jobExecutorHelperFds =
      [ C.mkTmplFd $ jobsDirInServerTemplatesDir SP.</> [relfile|core/pgBoss/pgBoss.js|],
        C.mkTmplFd $ jobsDirInServerTemplatesDir SP.</> [relfile|core/Job.js|],
        C.mkTmplFd $ jobsDirInServerTemplatesDir SP.</> [relfile|core/SubmittedJob.js|]
      ]

data JobsDir

jobsDirInServerTemplatesDir :: Path' (Rel ServerTemplatesDir) (Dir JobsDir)
jobsDirInServerTemplatesDir = srcDirInServerTemplatesDir SP.</> [reldir|jobs|]

executorJobTemplateInServerTemplatesDir :: JobExecutor -> Path SP.System (Rel ServerTemplatesDir) File'
executorJobTemplateInServerTemplatesDir = (jobsDirInServerTemplatesDir SP.</>) . executorJobTemplateInJobsDir

executorJobTemplateInJobsDir :: JobExecutor -> Path' (Rel JobsDir) File'
executorJobTemplateInJobsDir PgBoss = [relfile|core/pgBoss/pgBossJob.js|]
executorJobTemplateInJobsDir Passthrough = [relfile|core/passthroughJob.js|]

-- Path to destination files are the same as in templates dir.
jobsDirInServerRootDir :: Path' (Rel ServerRootDir) (Dir JobsDir)
jobsDirInServerRootDir = SP.castRel jobsDirInServerTemplatesDir

pgBossVersionBounds :: String
pgBossVersionBounds = "^7.2.1"

pgBossDependency :: AS.Dependency.Dependency
pgBossDependency = AS.Dependency.make ("pg-boss", pgBossVersionBounds)

depsRequiredByJobs :: AppSpec -> [AS.Dependency.Dependency]
depsRequiredByJobs spec = [pgBossDependency | isPgBossJobExecutorUsed spec]
