module Wasp.Generator.SdkGenerator.JobGenerator (genJobTypes) where

import Data.Aeson (object, (.=))
import Data.Maybe (fromJust)
import StrongPath (reldir, relfile, (</>))
import qualified StrongPath as SP
import Wasp.AppSpec (AppSpec, getJobs)
import qualified Wasp.AppSpec as AS
import Wasp.AppSpec.Job (Job)
import qualified Wasp.AppSpec.Job as J
import Wasp.Generator.Common (makeJsonWithEntityData)
import Wasp.Generator.FileDraft (FileDraft)
import Wasp.Generator.Job (jobExecutorTypesImportPathFromSdk)
import Wasp.Generator.Monad (Generator)
import qualified Wasp.Generator.SdkGenerator.Common as C
import Wasp.Util

genJobTypes :: AppSpec -> Generator [FileDraft]
genJobTypes spec = case getJobs spec of
  [] -> return []
  jobs -> return $ map genJobType jobs

genJobType :: (String, Job) -> FileDraft
genJobType (jobName, job) =
  C.mkTmplFdWithDstAndData
    tmplFile
    dstFile
    $ Just tmplData
  where
    tmplFile = [relfile|jobs/_jobTypes.ts|]
    dstFile = [reldir|jobs|] </> fromJust (SP.parseRelFile $ jobName ++ ".ts")
    tmplData =
      object
        [ "typeName" .= toUpperFirst jobName,
          "jobExecutorTypesImportPath" .= SP.fromRelFileP jobExecutorTypesImportPath,
          "entities" .= maybe [] (map (makeJsonWithEntityData . AS.refName)) (J.entities job)
        ]

    jobExecutorTypesImportPath = jobExecutorTypesImportPathFromSdk (J.executor job)
