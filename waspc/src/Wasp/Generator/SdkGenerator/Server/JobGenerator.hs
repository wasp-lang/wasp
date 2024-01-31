module Wasp.Generator.SdkGenerator.Server.JobGenerator
  ( genNewJobsApi,
    getImportPathForJobName,
    getJobExecutorTypesImportPath,
  )
where

import Data.Aeson (object, (.=))
import Data.Maybe (fromJust)
import StrongPath (File', Path, Posix, Rel, reldir, relfile, relfileP, (</>))
import qualified StrongPath as SP
import StrongPath.TH (reldirP)
import Wasp.AppSpec (AppSpec, getJobs)
import qualified Wasp.AppSpec as AS
import Wasp.AppSpec.Job (Job, JobExecutor (PgBoss))
import qualified Wasp.AppSpec.Job as J
import Wasp.Generator.Common (makeJsonWithEntityData)
import Wasp.Generator.FileDraft (FileDraft)
import Wasp.Generator.Monad (Generator)
import Wasp.Generator.SdkGenerator.Common (makeSdkImportPath)
import qualified Wasp.Generator.SdkGenerator.Common as C
import Wasp.Util

genNewJobsApi :: AppSpec -> Generator [FileDraft]
genNewJobsApi spec =
  case getJobs spec of
    [] -> return []
    jobs ->
      sequence
        [ genIndexTs jobs
        ]
        <++> mapM genJobType jobs

genIndexTs :: [(String, Job)] -> Generator FileDraft
genIndexTs jobs = return $ C.mkTmplFdWithData tmplFile tmplData
  where
    tmplFile = [relfile|server/jobs/index.ts|]
    tmplData = object ["jobs" .= map getJobTmplData jobs]
    getJobTmplData (jobName, _) =
      object
        [ "typeName" .= toUpperFirst jobName,
          "jobName" .= jobName
        ]

genJobType :: (String, Job) -> Generator FileDraft
genJobType (jobName, job) =
  return $
    C.mkTmplFdWithDstAndData
      tmplFile
      dstFile
      $ Just tmplData
  where
    tmplFile = [relfile|server/jobs/_jobTypes.ts|]
    dstFile = [reldir|server/jobs|] </> fromJust (SP.parseRelFile $ jobName ++ ".ts")
    tmplData =
      object
        [ "typeName" .= toUpperFirst jobName,
          "jobExecutorTypesImportPath" .= SP.fromRelFileP jobExecutorTypesImportPath,
          "entities" .= maybe [] (map (makeJsonWithEntityData . AS.refName)) (J.entities job)
        ]

    jobExecutorTypesImportPath = getJobExecutorTypesImportPath (J.executor job)

getImportPathForJobName :: String -> Path Posix (Rel d) File'
getImportPathForJobName jobName = makeSdkImportPath $ [reldirP|server/jobs|] </> fromJust (SP.parseRelFileP jobName)

-- | We are importing relevant types per executor e.g. JobFn, this functions maps
--   the executor to the import path of the relevant types.
getJobExecutorTypesImportPath :: JobExecutor -> Path Posix (Rel r) File'
getJobExecutorTypesImportPath PgBoss = makeSdkImportPath [relfileP|server/jobs/pgBoss/types|]
