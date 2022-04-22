module Wasp.Generator.ServerGenerator.JobGenerator
  ( genJobs,
    genJobExecutors,
    pgBossVersionBounds,
    pgBossDependency,
    depsRequiredByJobs,
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
    basename,
    parseRelFile,
    reldir,
    reldirP,
    relfile,
    toFilePath,
    (</>),
  )
import Wasp.AppSpec (AppSpec, getJobs)
import qualified Wasp.AppSpec.App.Dependency as AS.Dependency
import Wasp.AppSpec.JSON (waspJSONtoString)
import Wasp.AppSpec.Job
  ( Job (executor, perform),
    JobExecutor (Passthrough, PgBoss),
    Perform (options),
    fn,
    jobExecutors,
  )
import Wasp.AppSpec.Valid (isPgBossJobExecutorUsed)
import Wasp.Generator.ExternalCodeGenerator.Common (GeneratedExternalCodeDir)
import Wasp.Generator.FileDraft (FileDraft)
import Wasp.Generator.JsImport (getJsImportDetailsForExtFnImport)
import Wasp.Generator.Monad (Generator)
import Wasp.Generator.ServerGenerator.Common
  ( ServerSrcDir,
  )
import qualified Wasp.Generator.ServerGenerator.Common as C

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
                    "jobExecutorFilename" .= toFilePath (basename $ jobCreatorFilePath $ executor job),
                    "jobPerformOptions" .= maybe "{}" waspJSONtoString (options . perform $ job)
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
    genJobExecutor jobExecutor =
      let jobExecutorFp = jobCreatorFilePath jobExecutor
          sourceTemplateFp = C.asTmplFile jobExecutorFp
       in C.mkTmplFd sourceTemplateFp

    jobExecutorHelperFds :: [FileDraft]
    jobExecutorHelperFds =
      [ C.mkTmplFd $ C.asTmplFile [relfile|src/jobs/pgBoss.js|]
      ]

jobCreatorFilePath :: JobExecutor -> Path' (Rel d) File'
jobCreatorFilePath Passthrough = [relfile|src/jobs/passthroughJob.js|]
jobCreatorFilePath PgBoss = [relfile|src/jobs/pgBossJob.js|]

pgBossVersionBounds :: String
pgBossVersionBounds = "^7.2.1"

pgBossDependency :: AS.Dependency.Dependency
pgBossDependency = AS.Dependency.make ("pg-boss", pgBossVersionBounds)

depsRequiredByJobs :: AppSpec -> [AS.Dependency.Dependency]
depsRequiredByJobs spec = [pgBossDependency | isPgBossJobExecutorUsed spec]
